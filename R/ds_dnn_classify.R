#' This function gives the prediction of the deep neural network (DNN) model.
#'
#' @param dnn_model The DNN model output of the ds_dnn_model function.
#' @param model_data The output of the ds_split_data_dnn function.
#' @param query.data The scaled/normalized gene expression data you want to annotate.
#' @param threshold The minimum probability to classify cell types (Default=AUTO).
#'
#'
#' @return List containing the classification output and the probability matrix of the model.
#' @export
#' @examples
#' #not run
#' enc <- ds_encoder(data = scaled,genes = gg,dims = 2000,verbose = T,hnodes = c(5000))
#' features <- ds_get_features(enc = enc,data = scaled,genes=gg)
#' out <- ds_split_data_encoder(features = features,clus = cluster,prop = 0.8,verbose = T)
#' model <- ds_dnn_model(out = out,hnodes = c(1000),verbose = T,epochs = 10,batch_size = 32)
#  ids <- ds_dnn_classify(dnn_model = model,model.data = out,query.data = query.data) #### query.data is a normalized/scaled.data

ds_dnn_classify <- function(dnn_model,model.data,query.data,threshold="AUTO"){

  library(keras)
  train_x <- model.data$train_x
  features <- colnames(train_x)
  test_x <- t(as.matrix(query.data))
  do.genes <- setdiff(features,colnames(test_x))


  if(length(do.genes)>0){
    do.genes_x <- matrix(0,ncol = length(do.genes),nrow=nrow(test_x))
    colnames(do.genes_x) <- do.genes
    test_x <- cbind(test_x,do.genes_x)
    test_x <- test_x[,colnames(train_x)]
  }else{
    test_x <- as.matrix(test_x[,features])
  }


  probs <- dnn_model %>% predict(test_x)
  probs <- data.frame(probs,check.names = F)
  names(probs) <- c("unclassified",model.data$classes)

  if(threshold=="AUTO"){
    threshold_vec<-find_optimal_thresholds(dnn_model = dnn_model,model.data = model.data)
  }else{
    if (length(threshold) == 1) {
      threshold_vec <- rep(threshold, length(model.data$classes))
      names(threshold_vec) <- model.data$classes
    } else {
      threshold_vec <- threshold
      if (!all(model.data$classes %in% names(threshold_vec))) {
        stop("Threshold vector must be named and include all model classes.")
      }
    }
  }

  predicted <- apply(probs, 1, function(x) {
    scores <- x[-1]  # exclude "unclassified"
    class_names <- names(scores)
    thresholds <- threshold_vec[class_names]

    valid <- scores >= thresholds
    if (any(valid)) {
      best_class <- names(which.max(scores[valid]))
      return(best_class)
    } else {
      return("unclassified")
    }
  })

  ids <- factor(predicted)

  return(list(ids=ids,fit.prob=probs))
}
