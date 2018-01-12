# Following variables are global
globalVariables(c("CF", "lower", "overlap", "upper", "vars"))

#' Default \code{ggplot} theme for \code{viseffect} package.
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#'
#' @export
viseffect.theme <- function(){
  gt <- theme(text = element_text(size=10, colour="black"), # General Text Setting
        axis.text.x=element_text(colour="black"), # x axis labels text
        axis.text.y=element_text(colour="black"), # y axis labels text
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

#' Plotting Coefficients
#'
#' @description Drawing the coefficient plots from either model result or coefficient table. Borrowed the idea of odds ratio plotting from the code in \url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}.
#'
#' @param x The estimated model result object.
#' @param direct Import the result directly from the table (boulean). If \code{FALSE} (default), coefficients imported from estimated model object (\code{\link[stats]{coef}} and \code{\link[stats]{confint}} must be applicable). If \code{TRUE}, coefficients imported directly from coefficients table (rows=variables, columns=(coefficient, lower CI, upper CI), row names = variable names).
#' @param order.variable Order of coefficients in the plot. \code{"original"} (default) preserves the original order of the variable. \code{"coeforder"} plots by the discending order of the coefficient size. \code{"asis"} use the default \code{ggplot} setting.
#' @param odds Use odds ratio instead of coefficient in the output (boulean). The default is \code{FALSE}. If \code{TRUE}, the exponent of the coefficients will be plotted.
#' @param drop.intercept Drop the intercept from the plot (boulean). If \code{FALSE} (default), intercept included in the plot.
#' @param drop.intercept.names The name(s) of intercept (character/character vector). Needed if \code{drop.intercept} is \code{TRUE}. This is used to identify and eliminate intercept variables from the output. Default value is \code{"(Intercept)"}.
#' @param drop.variable.names The name(s) of additional variables to drop (character/character vector) from the ouput. The default is \code{NULL}.
#' @param point.shape Shape of the point outputs (numeric/character). The default is \code{16} (filled circle).
#' @param point.size Size of point outputs (numeric). The default is \code{1.5}.
#' @param confint.linetype The line type of confidence interval outputs (numeric). The default is \code{1}.
#' @param confint.linewidth The line width of confidence interval outputs (numeric). The default is \code{0.5}.
#' @param confint.height The height of the vertical line added to the edge of confidence interval outputs. The default is \code{0.2}.
#' @param overlap.x.list The list of additional estimated model result to be overlapped with the first result (list). Each element of the list must have the same set of variables and same class as the \code{x}. The default is \code{NULL}.
#' @param overlap.class.names The set of names that identifies overlapping \code{x}s (character vector). Needed if \code{overlap.x.list} is not \code{NULL}. The length of the vector must correspond with the length of \code{overlap.x.list} + 1. The first element of the vector is the name for \code{x}, then from the second element, the name order must correspond with the order in \code{overlap.x.list}.
#' @param overlap.gapwidth The gap between overlapped ouputs (number). The default value is \code{0.5}.
#' @param overlap.shape.index The index of shapes for overlapped point ouputs. Must be in the same length as \code{overlap.x.list} + 1. The first element of the vector is the shape for \code{x}, then from the second element, the order must correspond with the order in \code{overlap.x.list}. If \code{NULL}, \code{point.shape} is applied to all classes.
#' @param overlap.linetype.index The index of line types for overlapped confidence interval ouputs. Must be in the same length as \code{overlap.x.list} + 1. The first element of the vector is the shape for \code{x}, then from the second element, the order must correspond with the order in \code{overlap.x.list}. If \code{NULL}, the number corresponding with the order is assigned to each class.
#' @param overlap.legend.position The position of the legend for overlapping classess. See \code{legend.position} in ggplot theme for possible values. The default is \code{"top"}.
#' @param facet.category.names The categories of variables (factor). If not \code{NULL}, the output provides the separate panels for variables in each category. The length of the vector must much with the number of variables in \code{x} (This is considered BEFORE the application of \code{drop.intercept} and \code{drop.variable.names}. Just insert \code{NA} for those variables to be dropped.).
#' @param facet.names.location The location of facetted category names. Either \code{"left"} or \code{"right"}. The default is \code{"left"}.
#' @param facet.names.angle The angle of facetted category names (numeric). The default is \code{0} (horizontal).
#' @param title Plot title (character). The default is to include no title.
#' @param custom.variable.names List of alternative variable names in the output (character vector). The default is \code{NULL}. This is applied AFTER \code{drop.intercept} and \code{drop.variable.names} are applied, thus you don't need the names for dropped variables.
#' @param custom.themes ggplot themes that overrides the default theme. The default is \code{NULL}.
#'
#' @return \code{ggplot} object (axis and theme settings can be added later).
#'
#' @importFrom stats coef
#' @importFrom stats confint
#' @importFrom stats reorder
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 scale_y_log10
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 facet_grid
#'
#' @export
plot_coef<-function(x,
                    direct=FALSE,
                    order.variable="original",
                    odds=FALSE,
                    drop.intercept=FALSE,
                    drop.intercept.names = "(Intercept)",
                    drop.variable.names = NULL,
                    point.shape = 16,
                    point.size = 1.5,
                    confint.linetype = 1,
                    confint.linewidth = 0.5,
                    confint.height = 0.2,
                    overlap.x.list = NULL,
                    overlap.class.names = NULL,
                    overlap.gapwidth = 0.5,
                    overlap.shape.index = NULL,
                    overlap.linetype.index = NULL,
                    overlap.legend.position = "top",
                    facet.category.names = NULL,
                    facet.names.location = "left",
                    facet.names.angle = 0,
                    title = NULL,
                    custom.variable.names = NULL,
                    custom.themes = NULL
                    ){

# Set Default Graph Theme
gtheme <- viseffect.theme() +
 theme(panel.grid.major.x = element_blank(), # x axis grid line (major)
       panel.grid.minor.x = element_blank()) # x axis grid line (minor))

## Import Coefficients
if (direct){
  tmp <- x
  originalx <- x
  varnames <- row.names(originalx)
  facetnames <- facet.category.names
  if (is.null(overlap.x.list)==FALSE) {
    for(i in 1:length(overlap.x.list)){
      x <- rbind(x,overlap.x.list[[i]])
    }
    varnames <- rep(row.names(originalx),length(overlap.x.list)+1)
    facetnames <- rep(facet.category.names,length(overlap.x.list)+1)
  }
} else {
  tmp <- data.frame(cbind(coef(x), confint(x)))
  originalx <- tmp
  varnames <- row.names(originalx)
  facetnames <- facet.category.names
  if (is.null(overlap.x.list)==FALSE) {
    for(i in 1:length(overlap.x.list)){
      tmp <- rbind(tmp,data.frame(cbind(coef(overlap.x.list[[i]]), confint(overlap.x.list[[i]]))))
    }
    varnames <- rep(row.names(originalx),length(overlap.x.list)+1)
    facetnames <- rep(facet.category.names,length(overlap.x.list)+1)
  }
}
if (odds){
  tmp <- exp(x)
} else {
  tmp <- x
}
coefs <- tmp
names(coefs)<-c('CF', 'lower', 'upper')
coefs$vars <- varnames

## Variables for overlaps and facettng
if (is.null(overlap.x.list)==FALSE) {
  coefs$overlap <- rep(overlap.class.names, each=nrow(originalx))
  coefs$overlap <- factor(coefs$overlap,levels=overlap.class.names)
}
if (is.null(facet.category.names)==FALSE) {
  coefs$facet <- factor(facetnames,levels=levels(facet.category.names))
}

## Drop intercept
if (drop.intercept) {
  coefs<-coefs[-which(coefs$vars %in% drop.intercept.names),]
}

## Drop additional variables
if (is.null(drop.variable.names)==FALSE) {
  coefs<-coefs[-which(coefs$vars %in% drop.variable.names),]
}

## Use Custom Variable Names or Not
if (is.null(custom.variable.names)==FALSE) {
  coefs$vars <- custom.variable.names
  if (is.null(overlap.x.list)==FALSE) {
    coefs$vars <- rep(custom.variable.names, length(overlap.x.list))
  }
}

## Change Setting for ticks if Odds Ratio
if (odds){
  ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
}

## Settings for Overlapped Outputs
if (is.null(overlap.x.list)==FALSE) {
  classn <- length(overlap.x.list) + 1
  if(is.null(overlap.shape.index)==FALSE){
    overlap.shapes <- overlap.shape.index
  } else {
    overlap.shapes <- rep(point.shape,classn)
  }
  if(is.null(overlap.shape.index)==FALSE){
    overlap.linetypes <- overlap.linetype.index
  } else {
    overlap.linetypes <- seq(1,classn,1)
  }
}

## Start Plotting
if (order.variable=="asis"){
  plotstart = ggplot(coefs, aes(y= CF, x = vars )) +
    gtheme + coord_flip()
} else if (order.variable=="coeforder") {
  plotstart = ggplot(coefs, aes(y= CF, x = reorder(vars, CF) )) +
    gtheme + coord_flip()
} else if (order.variable=="original") {
  plotstart = ggplot(coefs, aes(y= CF, x = reorder(vars, (length(vars)+1) - seq(1,length(vars),1)) )) +
    gtheme + coord_flip()
}

## Intermediate Plot
if (is.null(overlap.x.list)==TRUE) {
  plotmid <- plotstart + geom_point(shape=point.shape, size=point.size) +
  geom_errorbar(aes(ymin=lower, ymax=upper), linetype=confint.linetype, width=confint.height, size=confint.linewidth)
} else {
  plotmid <- plotstart +
  geom_point(aes(shape=overlap), size=point.size, position=position_dodge(width = -overlap.gapwidth)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, linetype=overlap), width=confint.height, size=confint.linewidth, position=position_dodge(width = -overlap.gapwidth)) +
  scale_shape_manual(name="Class",values=overlap.shapes)+
  scale_linetype_manual(name="Class",values=overlap.linetypes) +
  theme(legend.position=overlap.legend.position)
}

## Intermediate Plot 2 (If Facetted)
if (is.null(facet.category.names)==FALSE) {
  if (facet.names.location=="left"){
    plotmid <- plotmid +
    facet_grid(facet~.,margins=F,scales="free_y",space="free_y",switch="y") +
    theme(strip.placement = "outside",
          strip.text.y = element_text(size=11, angle=facet.names.angle+180, face="bold"))
  } else if (facet.names.location=="right"){
    if(facet.names.angle>0){
      facet.names.angle <- 360-facet.names.angle
    }
    plotmid <- plotmid +
    facet_grid(facet~.,margins=F,scales="free_y",space="free_y") +
    theme(strip.text.y = element_text(size=11, angle=facet.names.angle, face="bold"))
  }
}

## Final Plot
if (odds){
  plotfin <- plotmid + scale_y_log10(breaks=ticks, labels = ticks) +
  scale_x_discrete() +
  geom_hline(yintercept = 1, linetype=2) +
  labs(title = title, x = NULL, y = 'Odds Ratio')
} else {
  plotfin <- plotmid + scale_x_discrete() +
  geom_hline(yintercept = 0, linetype=2) +
  labs(title = title, x = NULL, y = 'Coefficient')
}

## Override Custom Themes
if (is.null(custom.themes)==FALSE){
  plotfin <- plotfin + custom.themes
}

## Return the Plot
return(plotfin)

}
