#' Benchmarking: Plotting the output of the function `bench_calcAUC()`
#'
#' This function plots the output of the function `bench_calcAUC()`
#'
#' @param out The output of the function `bench_calcAUC()`
#' @param tile_cols Color vector for the tiles plot layer
#' @param tile_leg_title Title of tile legend
#' @param ct_leg_title Title of ct legend
#' @param ct_cols Color vector for the line and point layers
#'
#' @import dplyr
#' @import reshape2
#' @import ggnewscale
#'
#' @return A dataset of AUC per cell type of each case
#' @export
#'

bench_plotTileAUC<-function(out,tile_cols,tile_leg_title,ct_leg_title,ct_cols){
  out_t<-out[,1:ncol(out)-1]
  out_t<-as.data.frame(t(out_t))
  out_t$ct<-rownames(out_t)
  out_max<-c()
  for (i in 1:nrow(out)) {
    out_max<-c(out_max,out_t[,i][which.max(out_t[,i])])
  }
  out_max<-as.data.frame(out_max)
  out_max$Case<-rownames(out)
  out_max<-out_max[order(out_max$out_max,decreasing = T),]
  rownames(out_max)<-out_max$Case
  out_melt<-melt(out_t)
  out_melt$ct<-factor(out_melt$ct,levels = rownames(out_t))
  out_melt$variable<-factor(out_melt$variable,levels = rownames(out))

  plot<-ggplot()+
    geom_tile(data = out_max, aes(x = Case, y = out_max, fill = Case),
              width = 1, height = Inf, alpha = 0.2) +
    scale_fill_manual(values = tile_cols) +
    scale_fill_manual(values = case_colors)+
    guides(fill=guide_legend(title=tile_leg_title))+
    geom_line(data=out_melt,aes(x = variable,y = value,col=ct,group=ct))+
    geom_point(data=out_melt,aes(x = variable,y = value,col=ct),size=3.5)+
    scale_color_manual(values = ct_cols)+
    guides(color=guide_legend(title=ct_leg_title))+
    scale_size_continuous(guide = "none")+
    scale_alpha_continuous(guide = "none")+
    theme_bw()+
    theme(
      axis.ticks.y.right = element_blank(),
      axis.title.y.right = element_blank(),
      axis.text.y.right = element_blank()
    )
  return(plot)
}
