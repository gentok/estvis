# Following variables are global
globalVariables(c("CF", "lower", "overlap", "upper", "vars"))

#' Plotting Coefficients
#'
#' @description Drawing the coefficient plots from either model result or coefficient table. Borrowed the idea of odds ratio plotting from the code in \url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}.
#'
#' @param x The estimated model result object.
#' @param direct Import the result directly from the table (boulean). If \code{FALSE} (default), coefficients imported from estimated model object (\code{\link[stats]{coef}} and \code{\link[stats]{confint}} must be applicable). If \code{TRUE}, coefficients imported directly from coefficients table (rows=variables, columns=(coefficient, lower CI, upper CI), row names = variable names).
#' @param order.variable Order of coefficients in the plot. \code{"original"} (default) preserves the original order of the variable. \code{"coeforder"} plots by the discending order of the coefficient size. \code{"asis"} use the default \code{ggplot} setting.
#' @param odds Use odds ratio instead of coefficient in the output (boulean). The default is \code{FALSE}. If \code{TRUE}, the exponent of the coefficients will be plotted.
#' @param show.confint Show confidence interval (boulean). The default is \code{TRUE}.
#' @param percent.confint The percentage used for confidence interval (numeric: 0-100). The default is \code{95}.
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
#' @param overlap.legend.position The position of the legend for overlapping classess. See \code{legend.position} in ggplot theme for possible values. The default is \code{"bottom"}.
#' @param facet.category.names The categories of variables (factor). If not \code{NULL}, the output provides the separate panels for variables in each category. The length of the vector must much with the number of variables in \code{x} (This is considered BEFORE the application of \code{drop.intercept} and \code{drop.variable.names}. Just insert \code{NA} for those variables to be dropped.).
#' @param facet.names.location The location of facetted category names. Either \code{"left"} or \code{"right"}. The default is \code{"left"}.
#' @param facet.names.angle The angle of facetted category names (numeric). The default is \code{0} (horizontal).
#' @param title Plot title (character). The default is to include no title.
#' @param custom.variable.names List of alternative variable names in the output (character vector). The default is \code{NULL}. This is applied AFTER \code{drop.intercept} and \code{drop.variable.names} are applied, thus you don't need the names for dropped variables.
#' @param custom.themes ggplot themes that overrides the default theme. The default is \code{NULL}.
#'
#' @return \code{ggplot} object (axis and theme settings can be added later).
#'
#' @examples
#' ## Load Data
#' library(pscl)
#' data(vote92)
#'
#' ## Recode Variables
#' vote92$voteBush <- as.numeric(
#'   factor(vote92$vote,levels=c("Clinton","Bush")))*1 - 1
#' vote92$bushdis <- sqrt(vote92$bushdis)
#' vote92$clintondis <- sqrt(vote92$clintondis)
#'
#' ## Estimate Logistic Regression
#' fm <- formula(voteBush ~ dem + rep +
#'                 clintondis + bushdis +
#'                 persfinance + natlecon)
#' m <- glm(fm, data = vote92,
#'          family = binomial("logit"))
#'
#' ## Basic Graph
#' plot_coef(m)
#'
#' ## Change X-axis to Odds Ratio
#' plot_coef(m, odds = TRUE)
#'
#' ## Export the Coefficients Table First, and import directly
#' cfci <- data.frame(cbind(coef(m), confint(m, level = 0.95)))
#' plot_coef(cfci, direct = TRUE)
#'
#' ## Estimate Model by Male and Female Subset
#' m_male <- glm(fm, data = vote92[vote92$female==0,],
#'          family = binomial("logit"))
#' m_female <- glm(fm, data = vote92[vote92$female==1,],
#'               family = binomial("logit"))
#'
#' ## Overlap Subsetted Results
#' plot_coef(m_male,
#'           overlap.x.list = list(m_female),
#'           overlap.class.names = c("Male", "Female"))
#'
#' ## Add Title and Custom Variable Names
#' vn <- c("(Intercept)",
#'         "Democrat","Republican",
#'         "Ideological Distance from Clinton",
#'         "Ideological Distance from Bush",
#'         "Retrospective Personal Finance",
#'         "Retrospective National Economy")
#' plot_coef(m_male,
#'           overlap.x.list = list(m_female),
#'           overlap.class.names = c("Male", "Female"),
#'           title = "Vote for Bush (1992)",
#'           custom.variable.names = vn)
#'
#' ## Overlap the Third Model & Facet Variables by Category
#' fn <- c("Constant",rep("Preference",4),rep("Evaluation",2))
#' fn <- factor(fn,levels=unique(fn))
#' plot_coef(m_male,
#'           overlap.x.list = list(m_female, m),
#'           overlap.class.names = c("Male", "Female", "All"),
#'           facet.category.names = fn,
#'           title = "Vote for Bush (1992)",
#'           custom.variable.names = vn)
#'
#' @importFrom stats coef
#' @importFrom stats confint
#' @importFrom stats reorder
#' @import MASS
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
#' @import pscl
#'
#' @export
plot_coef<-function(x,
                    direct=FALSE,
                    order.variable="original",
                    odds=FALSE,
                    show.confint = TRUE,
                    percent.confint = 95,
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
                    overlap.legend.position = "bottom",
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
if (direct==TRUE){
  tmp <- x
  originalx <- x
  varnames <- row.names(originalx)
  facetnames <- facet.category.names
  if (is.null(overlap.x.list)==FALSE) {
    for(i in 1:length(overlap.x.list)){
      x <- rbind(x,overlap.x.list[[i]])
    }
    tmp <- x
    varnames <- rep(row.names(originalx),length(overlap.x.list)+1)
    facetnames <- rep(facet.category.names,length(overlap.x.list)+1)
  }
} else if (direct==FALSE) {
  pci <- percent.confint/100
  tmp <- data.frame(cbind(coef(x), confint(x, level = pci)))
  originalx <- tmp
  varnames <- row.names(originalx)
  facetnames <- facet.category.names
  if (is.null(overlap.x.list)==FALSE) {
    for(i in 1:length(overlap.x.list)){
      tmp <- rbind(tmp,data.frame(cbind(coef(overlap.x.list[[i]]), confint(overlap.x.list[[i]], level = pci))))
    }
    varnames <- rep(row.names(originalx),length(overlap.x.list)+1)
    facetnames <- rep(facet.category.names,length(overlap.x.list)+1)
  }
}
if (odds){
  tmp <- exp(tmp)
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
    coefs$vars <- rep(custom.variable.names, length(overlap.x.list)+1)
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
  plotmid <- plotstart + geom_point(shape=point.shape, size=point.size)
  if (show.confint==TRUE) {
    plotmid <- plotmid +
    geom_errorbar(aes(ymin=lower, ymax=upper), linetype=confint.linetype, width=confint.height, size=confint.linewidth)
  }
} else {
  plotmid <- plotstart +
  geom_point(aes(shape=overlap), size=point.size, position=position_dodge(width = -overlap.gapwidth)) +
  scale_shape_manual(name="Class",values=overlap.shapes)+
  theme(legend.position=overlap.legend.position)
  if (show.confint==TRUE) {
    plotmid <- plotmid +
    geom_errorbar(aes(ymin=lower, ymax=upper, linetype=overlap), width=confint.height, size=confint.linewidth, position=position_dodge(width = -overlap.gapwidth)) +
    scale_linetype_manual(name="Class",values=overlap.linetypes)
  }
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
  if (show.confint==TRUE) {
    ylabtxt <- paste('Odds Ratio with ', percent.confint, '% Confidence Interval', sep="")
  } else {
    ylabtxt <- 'Odds Ratio'
  }
  plotfin <- plotmid + scale_y_log10(breaks=ticks, labels = ticks) +
  scale_x_discrete() +
  geom_hline(yintercept = 1, linetype=2) +
  labs(title = title, x = NULL, y = ylabtxt)
} else {
  if (show.confint==TRUE) {
    ylabtxt <- paste('Coefficient with ', percent.confint, '% Confidence Interval', sep="")
  } else {
    ylabtxt <- 'Coefficient'
  }
  plotfin <- plotmid + scale_x_discrete() +
  geom_hline(yintercept = 0, linetype=2) +
  labs(title = title, x = NULL, y = ylabtxt)
}

## Override Custom Themes
if (is.null(custom.themes)==FALSE){
  plotfin <- plotfin + custom.themes
}

## Return the Plot
return(plotfin)

}
