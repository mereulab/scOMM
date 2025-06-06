#' This function is used to find the optimal threshold for each cell type
#'
#' @param model_data The output of the ds_split_data_dnn function.
#' @param prob The predicted probability matrix
#'
#' @import pROC
#'
#' @return List containing the classification output and the probability matrix of the model.
#' @export
#'

find_optimal_thresholds <- function(dnn_model=model, model.data=out) {
  library(pROC)

  test_x <- model.data$test_x
  test_y <- model.data$test_y
  classes <- model.data$classes

  probs <- dnn_model %>% predict(test_x)
  probs <- data.frame(probs, check.names = FALSE)
  colnames(probs) <- c("unclassified", classes)
  colnames(test_y)<-colnames(probs)

  thresholds <- sapply(classes, function(class_name) {
    true_labels <- as.numeric(test_y[,class_name])

    # Skip if class is not present in test_y
    if (length(unique(true_labels)) < 2) {
      warning(sprintf("Skipping class '%s': not enough positive/negative examples", class_name))
      return(NA)
    }

    pred_scores <- probs[[class_name]]
    roc_obj <- roc(response = true_labels, predictor = pred_scores, quiet = TRUE)
    best_coords <- coords(roc_obj, x = "best", best.method = "closest.topleft", transpose = TRUE)

    return(best_coords["threshold"])
  })

  names(thresholds) <- sub("\\.threshold$", "", names(thresholds))

  return(thresholds)
}

