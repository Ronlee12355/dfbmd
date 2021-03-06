#' Code to change the axis of ggplot image
#' @description This function computes the standard error of the values in x. If na.rm is TRUE then missing values are
#' removed before computation proceeds.
#' @return value of standard error
#' @importFrom ggplot2 theme_bw theme element_blank element_rect element_text element_line
#' @seealso \code{\link{ggplot2}}
#' @examples \dontrun{
#' library(tidyverse)
#' ggplot(iris, aes(x=Sepal.Length , y= Species, fill= Species))+geom_boxplot()+
#' specific_theme()
#' }
#' @export
specific_theme <- function(){
  theme_bw()+
    theme(panel.grid = element_blank(),panel.border = element_rect(size=1))+
    theme(strip.background = element_rect(size = 1,fill="NA"),strip.text = element_text(size=12,color="black"))+
    theme(plot.title = element_text(size=13,color="black"))+
    theme(axis.ticks = element_line(size=1),axis.text = element_text(size=12,colour = "black",angle = 00),axis.title = element_text(size=18,color="black"))+
    theme(legend.text = element_text(size=11,colour = "black"))
}
