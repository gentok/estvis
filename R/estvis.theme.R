#' Default \code{ggplot} theme for \code{estvis} package.
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#'
#' @export
estvis.theme <- function(){
  gt <- theme(text = element_text(size=10, colour="black"), # General Text Setting
        axis.text.x=element_text(size=10, colour="black"), # x axis labels text
        axis.text.y=element_text(size=10, colour="black"), # y axis labels text
        axis.title.x=element_text(size=11,vjust=-1.5), # x axis title text
        axis.title.y=element_text(size=11,vjust=1.5), # y axis title text
        plot.title=element_text(size=11,lineheight=.8,face="bold",vjust=2,hjust=0.5), # plot title
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), # margins of the graph area *top, right, bottom, left
        panel.grid.major.x = element_line(colour = "grey90", linetype=2), # x axis grid line (major)
        panel.grid.major.y = element_line(colour = "grey90", linetype=2), # y axis grid line (major)
        panel.grid.minor.x = element_line(colour = "grey90", linetype=2), # x axis grid line (minor)
        panel.grid.minor.y = element_line(colour = "grey90", linetype=2), # y axis grid line
        panel.background = element_rect(fill=NA, colour="black", size=0.5, linetype=1), # plot panel area setting
        legend.background = element_rect(fill="white",colour="black"), # legend box setting
        legend.position = "bottom", # position of the legend: c(x,y) OR "top","bottom","left","right"
        legend.title=element_blank(), # Setting of the legend title
        legend.margin = margin(t=0.01,r=0.2,b=0.1,l=0.1,"cm"), # margins for legend box
        legend.key = element_rect(fill=NA,colour=NA), # legend keys settings
        legend.key.width = unit(1.2, "cm"), # size(width) of the legend keys
        strip.background = element_rect(fill=NA,colour=NA) # Setting of the facetted title area
        )
  return(gt)
}
