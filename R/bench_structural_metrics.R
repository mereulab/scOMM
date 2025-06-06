#' Benchmarking: Calculate structural-aware metrics
#'
#' This function calculates silhouette/cLISI score for each cell type
#'
#' @param Obj A Seurat Object
#' @param cluster The analyzing clusters' name
#' @param Metric The calculated metric, "Silhouette" or "cLISI".
#' @param reduction The dimensionality reduction which will be used to calculate the score. It should already exist in the seurat object.
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
#'

bench_structural_metrics<-function(Obj,Metric=c("Silhouette","cLISI"),cluster,reduction){
  if(Metric=="Silhouette"){
    # Extract embeddings (PCA or UMAP)
    embeddings <- Embeddings(Obj, reduction = reduction)

    # Get cluster assignments
    clusters <- Obj@meta.data[[cluster]]

    cluster_levels <- levels(factor(clusters))

    # Compute silhouette scores
    sil_scores <- silhouette(as.numeric(factor(clusters)), dist(embeddings))

    # Convert to dataframe
    sil_df <- data.frame(Cluster = factor(clusters, levels = cluster_levels), Silhouette = sil_scores[, 3])

    # Compute average silhouette score per cluster
    cluster_sil_df <- sil_df %>%
      group_by(Cluster) %>%
      summarise(Average_Silhouette = mean(Silhouette, na.rm = TRUE))

    cluster_sil_df <- as.data.frame(cluster_sil_df)

    rownames(cluster_sil_df)<-cluster_sil_df$Cluster

    return(cluster_sil_df)
  }
  if(Metric=="cLISI"){
    metadata <- data.frame(
      UMAP_1 = Embeddings(Obj, reduction)[,1],
      UMAP_2 = Embeddings(Obj, reduction)[,2],
      Cell_Type = Obj[[cluster]][,1]
    )

    clisi_scores <- compute_lisi(metadata[, c("UMAP_1", "UMAP_2")], metadata, label_colnames = "Cell_Type")

    metadata$cLISI <- clisi_scores$Cell_Type

    cLISI_Xct <- metadata %>%
      group_by(Cell_Type) %>%
      summarise(Average_cLISI = mean(cLISI, na.rm = TRUE))

    cLISI_Xct<-as.data.frame(cLISI_Xct)

    row.names(cLISI_Xct)<-cLISI_Xct$Cell_Type


    return(cLISI_Xct)
  }
}
