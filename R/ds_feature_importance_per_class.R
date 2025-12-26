#' This function calculates the feature importance of the deep learning model
#'
#' This function generates a dataframe with the feature importance score of the input features calculated
#' based on the "Permutation Feature Importance" approach.
#'
#' @param X The data with the feature information
#' @param Y The data with the output information
#' @param model The deep learning model
#' @param classes The classes to be grouped
#' @param features The manually selected features whose importance will be calculated. If not provided,
#' the function will use all the features. You can control the number of used features by using the parameter
#' `max_n`.
#' @param  max_n Number of features selected from X if `features` not provided.
#'
#' @import keras
#' @import tensorflow
#'
#' @return A dataframe with the importance score of each feature
#'
#' @export
#' @examples
#' ds_feature_importance_per_class(X=x_test,Y=y_test,model=scOMM_model,classes=c(),max_n=500)
#'

ds_feature_importance_per_class<-function(X,Y,model,classes,features,max_n=1000){
  # Accuracy
  colnames(Y)<-c("unclassified",classes)
  calc_acc<-function(X,Y,classes,model){
    y_pred <- model %>% predict(X)
    names(y_pred) <- c("unclassified",classes)
    y_pred_class <- apply(y_pred,1,function(x) ifelse(max(x)>0.5,names(y_pred)[which(x==max(x))],"unclassified"))
    y_true_class <- colnames(Y)[max.col(Y, ties.method = "first")]
    classes <- sort(unique(y_true_class))

    baseline<-c()
    for (cls in classes) {
      idx <- which(y_true_class == cls)
      acc <- mean(y_true_class[idx] == y_pred_class[idx])
      baseline<-c(baseline,acc)
    }
    names(baseline)<-classes
    return(baseline)
  }
  baseline<-calc_acc(X=X,Y=Y,classes = classes,model=model)

  if(missing(features)){
    if(missing(max_n)){
      features<-colnames(X)
    }
    else{
      features<-colnames(X)[1:max_n]
    }
  }

  importance<-c()
  num_permutations<-length(features)
  for (i in 1:num_permutations) {
    xi = rep(0,nrow(X))
    Xi = cbind(xi, X[,-which(colnames(X) == features[i])])
    permuted_metrics <- calc_acc(X=Xi,Y=Y,classes = classes,model=model)
    importance <- rbind(importance,((permuted_metrics-baseline)/baseline)*100)
    print(paste0(i,' --- ',features[i]))
  }
  importance<- abs(importance)
  PFI_final = as.data.frame(importance)
  #PFI_final<- -1 * PFI_final
  rownames(PFI_final)<-features
  #PFI_final <- PFI_final[order(PFI_final$Score, decreasing = TRUE),]
  return(PFI_final)
}


