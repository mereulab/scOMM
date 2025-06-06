#' Benchmarking: Plotting the average AUC of a list of outputs from `bench_calcAUC()`
#'
#' This function plots a list of outputs from the function `bench_calcAUC()`
#'
#' @param out_list The list of outputs from the function `bench_calcAUC()`
#' @param bar_cols Color vector for the bar plot layer
#' @param ct_mapping Option to fill bar plot layer by grouping cols, if not provided, by default will only take information from `out`
#' @param group_case Column specifying the benchmarking cases
#' @param group_cols Color vector for the line and point layers
#' @param alpha_vec Alpha vector for the bar plot layer
#'
#' @import dplyr
#' @import reshape2
#'
#' @return A dataset of AUC per cell type of each case
#' @export
#'

bench_plotMeanAUC<-function(out_list){
  n_case<-nrow(out_list[[1]])
  for (i in 1:length(out_list)) {
    out_list[[i]]<-out_list[[i]][,1:ncol(out_list[[i]])-1]
    out_list[[i]]<-as.data.frame(t(out_list[[i]]))
    out_list[[i]]$ct<-rownames(out_list[[i]])
  }

  if(length(out_list) > 2){
    out_general<-rbind(out_list[[1]],out_list[[2]])
    for (i in 3:length(out_list)) {
      out_general<-rbind(out_general,out_list[[i]])
    }
  }else{
    out_general<-rbind(out_list[[1]],out_list[[2]])
  }

  ct<-unique(out_general$ct)
  general_per_ct<-c()
  for (i in 1:length(ct)) {
    general_per_ct<-rbind(general_per_ct,colMeans(out_general[which(out_general$ct==ct[i]),1:n_case]))
  }
  general_per_ct<-as.data.frame(general_per_ct)
  general_per_ct$ct<-ct
  general_per_ct_melt<-melt(general_per_ct)
  general_per_ct_melt$ct<-factor(general_per_ct_melt$ct,levels = unique(out_general$ct))
  general_per_ct_melt$variable<-factor(general_per_ct_melt$variable,levels = colnames(out_general)[1:ncol(out_general)-1])

  plot<-ggplot(general_per_ct_melt, aes(x = ct, y = variable, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
    scale_fill_viridis_c() +
    theme_minimal()
  return(plot)
}
