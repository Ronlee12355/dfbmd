#' Output volcano plot with ggplot2
#'
#' @param dt Dataframe with specific columns
#' @param mainTitle Main title of the plot
#'
#' @return Object of ggplot2
#' @details Input data should be dataframe containing following columns:
#' log2FC: the value of fold change; padj: adjusted p value; threshold: Up-, down-regulated genes
volcanoPlot<-function(dt, mainTitle=''){
  ggplot(data = dt, aes(x = log2FC, y = -log10(padj), colour=threshold)) +
    geom_point(size=0.5) +
    scale_color_manual(values=c("green", "blue","red")) +
    xlim(c(-20, 20)) +
    geom_vline(xintercept=c(-2,2),lty=4,col="black",lwd=0.4) +
    geom_hline(yintercept = -log10(0.05),lty=4,col="black",lwd=0.4) +
    labs(x="log2(fold change)",y="-log10 (p-value)",title=mainTitle,fill="threshold") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="right",
          legend.title = element_blank(),
          panel.grid = element_line(colour=NA))
}
