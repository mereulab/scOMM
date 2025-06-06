#' Benchmarking: Calculate clustering evaluation metrics
#'
#' This function calculates Purity/F1/Jaccard Index score for each cell type, in the case of the ARI score, it will provide the user a global score.
#'
#' @param predicted_clusters The predicted clusters
#' @param true_labels The true cluster labels.
#' @param Metric The calculated metric, "Purity","F1","Jaccard", or "ARI".
#'
#' @import cluster
#' @import dplyr
#' @import mclust
#' @import lisi
#'
#' @return A dataset containing the score dataset.
#' @export
#'
#'

bench_clustering_metrics<-function(predicted_clusters,true_labels,Metric=c("Purity","F1","Jaccard","ARI")){
  if(Metric=="Purity"){
    # Create a contingency table
    contingency_table <- table(predicted_clusters, true_labels)

    # Compute purity for each cluster
    cluster_purity <- apply(contingency_table, 1, function(x) max(x) / sum(x))

    # Convert to dataframe
    purity_df <- data.frame(Cluster=names(cluster_purity),Purity = cluster_purity)

    return(purity_df)
  }
  if(Metric=="F1"){
    contingency_table <- table(predicted_clusters, true_labels)
    cluster_f1 <- numeric(nrow(contingency_table))

    for (i in 1:nrow(contingency_table)) {
      row <- contingency_table[i, ]
      TP <- max(row)
      FP <- sum(row) - TP
      col_index <- which.max(row)
      FN <- sum(contingency_table[, col_index]) - TP

      precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
      recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))

      f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
      cluster_f1[i] <- f1
    }

    f1_df <- data.frame(Cluster = rownames(contingency_table), F1_Score = cluster_f1)
    rownames(f1_df)<-f1_df$Cluster
    return(f1_df)
  }
  if(Metric=="Jaccard"){
    contingency_table <- table(predicted_clusters, true_labels)
    cluster_jaccard <- numeric(nrow(contingency_table))

    for (i in 1:nrow(contingency_table)) {
      row <- contingency_table[i, ]
      TP <- max(row)
      FP <- sum(row) - TP
      col_index <- which.max(row)
      FN <- sum(contingency_table[, col_index]) - TP

      jaccard <- ifelse((TP + FP + FN) == 0, 0, TP / (TP + FP + FN))
      cluster_jaccard[i] <- jaccard
    }

    jaccard_df <- data.frame(Cluster = rownames(contingency_table), Jaccard_Index = cluster_jaccard)
    rownames(jaccard_df)<-jaccard_df$Cluster
    return(jaccard_df)
  }
  if(Metric=="ARI"){
    ari <- adjustedRandIndex(predicted_clusters, true_labels)
    return(ari)
  }
}
