# Following variables are global
globalVariables(c("CF", "lower", "overlap", "upper", "vars"))

#' Extracting GOFs from Models for Footnote using \code{\link[texreg]{extract}()} function
#'
#' @description Extracting GOFs from Models for footnote using \code{\link[texreg]{extract}()} function from \code{texreg} package.
#'
#' @param m Single model object or the list of model objects.
#' @param m.names The names for models (character vector). The default is \code{NULL}, but required when there are multiple models in the input.
#' @param nobs The position of "number of observation (N)" to be printed. The default is \code{"right"} (the end of the sentence). \code{"left"} (the start of the sentence) and \code{"none"} (do not print number of observation) are also available.
#' @param gof.extracts The GOF measures to be included in the output (character/character vector). The default is \code{c("AIC", "Adj. R$^2$", "Log Likelihood")}. If \code{"all"}, then all GOFs extracted by \code{\link[texreg]{extract}()} function are included in texts. If \code{"none"} and \code{nobs} is not \code{"none"}, then only number of observation will be printed.
#' @param gof.reorder The numeric vector specifying the alternative order of GOF  (e.g., if there are three GOFs, and you want to flip the order of first and second GOF, then input <code>c(2,1,3)</code>). The length of the vector must correspond with the number of <i>exported</i> GOFs (it may not be the same as the length of <code>gof.extracts</code>). If <code>NULL</code> (default), the original order is used. The order does not include the number of observation (if exported).
#' @param linebreak Include linebreak between model (boulean). If \code{TRUE} (default), linebreak will be included between models. If \code{FALSE}, the ouput will be single line. Only applicable when there are more than one model.
#' @param ... Additional arguments passed to \code{\link[texreg]{extract}()} function.
#'
#' @return A \code{character} vector, including GOFs and number of observations of results. Subsequently used within \code{\link{plot_coef}} function.
#'
#' @importFrom texreg extract
#'
#' @export
extract_gofchr <- function(m,
                           m.names = NULL,
                           nobs = "right",
                           gof.extracts = c("AIC", "Adj. R$^2$", "Log Likelihood"),
                           gof.reorder = NULL,
                           linebreak = TRUE, ...
                           ){

  ## Make a list if it's not list
  if(class(m)[1]!="list") {
    m <- list(m)
  }

  ## Name Vector Error
  if (is.null(m.names)==TRUE) {
    if(length(m)>1){
      stop("m.names is required for multiple models.")
    }
  } else if (is.null(m.names)==FALSE) {
    if(length(m)>1){
      if (length(m)!=length(m.names)) {
        stop("m and m.names must have the same length.")
      }
    }
  }

  ## For each model
  goflist <- list()
  for (i in 1:length(m)) {

    ## Extract GOFs
    a <- extract(m[[i]], ...)
    nloc <- which(a@gof.names == "Num. obs.")
    if (gof.extracts[1]=="none") {
      if (length(nloc)==0) {
        stop("No extracts for Num. obs. found. There is no output.")
      } else {
        if (nobs=="none") {
          stop("Both nobs and gof.extracts are set to 'none'. There is no ouput.")
        } else {
          gn <- paste("N:", round(a@gof[nloc]), sep="")
        }
      }
    } else {
      if (length(nloc)==0) {
        warning("No extracts for Num. obs. found. Omitted from the output.")
      }
      if (gof.extracts[1] == "all") {
        if (length(nloc)==1) {
          gofloc <- seq(1,length(a@gof),1)[-nloc]
        } else if (length(nloc)==0) {
          gofloc <- seq(1,length(a@gof),1)
        } else {
          stop("More than one Num. obs. found.")
        }
      } else {
        gofloc <- which(a@gof.names %in% gof.extracts)
      }
      if (is.null(gof.reorder)==FALSE){
        if (length(gof.reorder)==length(gofloc)){
          gofloc <- gofloc[gof.reorder]
        } else {
          warning("The length of gof.reorder does not match with the length of exported GOFs. The original order is preserved.")
        }
      }

      if (length(gofloc)==0) {
        if (length(nloc)==0) {
          stop("No extracts for Num. obs. and GOFs found. There is no output.")
        } else {
          if (nobs=="none") {
            stop("nobs set to 'none' and no GOFs found. There is no ouput.")
          } else {
            gn <- paste("N:", round(a@gof[nloc]), sep="")
            warning("No GOFs found. Only Num. obs. will be printed.")
          }
        }
      } else {
        ## Store The Extracted Data as Character Vector
        gn <- character()
        for (j in gofloc){
          gn <- paste(gn, a@gof.names[j], ":", round(a@gof[j],3), "; ", sep="")
        }
        if (length(nloc)==1) {
          gn <- paste(gn, "N:",round(a@gof[nloc]), sep="")
        } else if (length(nloc)==0) {
          gn <- gsub('.{2}$', '', gn)
        }
      }
    }

    ## Save it in the GOF list
    goflist[[i]] <- gn

  }

  ## Export Character Vector for the Footnote
  if (length(goflist)==1) {
    gofchr <- goflist[[1]]
  } else if (length(goflist)>1) {
    gofchr <- character()
    for (k in 1:length(goflist)){
      gofchr <- paste(gofchr, m.names[k]," (", goflist[k], ")\n", sep="")
    }
    gofchr <- gsub('.{1}$', '', gofchr)
  } else {
    stop("No Ouput of the GOF Character Vector")
  }

  if (linebreak==FALSE) {
    gofchr <- gsub('\n', ' ', gofchr)
  }

  ## Return The Character Vector
  return(gofchr)

}

#' Plotting Coefficients
#'
#' @description Drawing the coefficient plots from either model result or coefficient table.
#'
#' @param m Single or the list of model object or the \code{matrix} or \code{data.frame} object. If it's a list all elements of the list should be the same class. If model object, \code{\link[stats]{coef}} and \code{\link[lmtest]{coefci}} must be applicable to the object. If  \code{matrix} or \code{data.frame}, then it should include following three columns and row names. Each row should correspond to each variable.
##' \itemize{
##'  \item{Row Names: }{Name of Variables}
##'  \item{First Column: }{Coefficient}
##'  \item{Second Column: }{Lower limit of confidence interval}
##'  \item{Third Column: }{Upper limit of confidence interval}
##' }
#' @param m.names The set of names that identifies each element in \code{m}. Considered if \code{m} is a list of models. The length of the vector must correspond with the length of \code{m}. If \code{NULL} (default), each element \code{i} is temporarily named as \code{Model i}.
#' @param order.variable Order of coefficients in the plot(character/numeric vector). \code{"original"} (default) preserves the original order of the variable. \code{"cfdescend"} plots by the descending order of coefficient values. \code{"cfascend"} plots by the ascending order of coefficient values. \code{"asis"} use the default \code{ggplot} setting.
#' @param odds Use odds ratio instead of coefficient in the output (boulean). The default is \code{FALSE}. If \code{TRUE}, the exponent of the coefficients will be plotted.
#' @param vcov.est Single or the list of the alternative variance-covariance matrix to be used for the model object \code{m}. Ignored if \code{NULL} (default) or the \code{m} is not model object. Must have the same length as \code{m} if it is a list.
#' @param sims.est Number of iterations if simulation method is used to calculate mean coefficient value and confidence interval. If \code{NULL} (default), the analytical method is used to calculate confidence intervals.
#' @param show.ci Show confidence interval (boulean). The default is \code{TRUE}.
#' @param level.ci The level used for confidence interval (numeric: 0-1). The default is \code{0.95}.
#' @param drop.intercept Drop the intercept from the plot (boulean). If \code{FALSE} (default), intercept included in the plot.
#' @param drop.intercept.names The name(s) of intercept (character/character vector). Needed if \code{drop.intercept} is \code{TRUE}. This is used to identify and eliminate intercept variables from the output. Default value is \code{"(Intercept)"}.
#' @param drop.variable.names The name(s) of additional variables to drop (character/character vector) from the ouput. The default is \code{NULL}.
#' @param point.shape Shape of the point outputs (numeric/character). The default is \code{16} (filled circle).
#' @param point.size Size of point outputs (numeric). The default is \code{1.5}.
#' @param ci.linetype The line type of confidence interval outputs (numeric). The default is \code{1}.
#' @param ci.size The line width of confidence interval outputs (numeric). The default is \code{0.5}.
#' @param ci.height The height of the vertical line added to the edge of confidence interval outputs. The default is \code{0.2}.
#' @param overlap.gapwidth The gap between overlapped ouputs (number). The default value is \code{0.5}.
#' @param overlap.shape.index The index of shapes for overlapped point ouputs. Must be in the same length as \code{overlap.m.list} + 1. The first element of the vector is the shape for \code{m}, then from the second element, the order must correspond with the order in \code{overlap.m.list}. If \code{NULL}, \code{point.shape} is applied to all classes.
#' @param overlap.linetype.index The index of line types for overlapped confidence interval ouputs. Must be in the same length as \code{overlap.m.list} + 1. The first element of the vector is the shape for \code{m}, then from the second element, the order must correspond with the order in \code{overlap.m.list}. If \code{NULL}, the number corresponding with the order is assigned to each class.
#' @param overlap.legend.position The position of the legend for overlapping classess. See \code{legend.position} in ggplot theme for possible values. The default is \code{"bottom"}.
#' @param facet.category.names The categories of variables (factor). If not \code{NULL}, the output provides the separate panels for variables in each category. The length of the vector must much with the number of variables in \code{m} (This is considered BEFORE the application of \code{drop.intercept} and \code{drop.variable.names}. Just insert \code{NA} for those variables to be dropped.).
#' @param facet.names.location The location of facetted category names. Either \code{"left"} or \code{"right"}. The default is \code{"left"}.
#' @param facet.names.angle The angle of facetted category names (numeric). The default is \code{0} (horizontal).
#' @param title Plot title (character). The default is to include no title.
#' @param ytitle Y axis title (character). The default is to include no axis title.
#' @param custom.variable.names List of alternative variable names in the output (character vector). The default is \code{NULL}. This is applied AFTER \code{drop.intercept} and \code{drop.variable.names} are applied, thus you don't need the names for dropped variables.
#' @param custom.x.title Custom name for the X axis title (character). The default is \code{NULL}.
#' @param custom.x.lim Custom limit for the X axis. The default is \code{NULL}.
#' @param custom.x.breaks Custom breaks for the X axis. The default is \code{NULL}.
#' @param custom.themes ggplot themes that overrides the default theme. The default is \code{NULL}.
#' @param footnote.gof Include GOF measures in the footnote (boulean). The default is \code{FALSE}. If \code{TRUE}, footnote with GOF measures are added to the plot by \code{\link{extract_gofchr}} and \code{\link{plot_footnote}} function, and the function exports \code{gtable} object. Note that \code{gtable} object is less customizable than \code{ggplot} object.
#' @param footnote.gof.nobs The position of "number of observation (N)" to be printed. The default is \code{"right"} (the end of the sentence). \code{"left"} (the start of the sentence) and \code{"none"} (do not print number of observation) are also available.
#' @param footnote.gof.extracts GOF measures to be inluded if \code{footnote.gof == TRUE}. See \code{\link{extract_gofchr}} documentation for more details.#' #' @param gof.reorder The numeric vector specifying the alternative order of GOF  (e.g., if there are three GOFs, and you want to flip the order of first and second GOF, then input <code>c(2,1,3)</code>). The length of the vector must correspond with the number of <i>exported</i> GOFs (it may not be the same as the length of <code>footnote.gof.extracts</code>). If <code>NULL</code> (default), the original order is used. The order does not include the number of observation (if exported).
#' @param footnote.gof.linebreak Include linebreak between models (boulean). If \code{TRUE} (default), linebreak will be included between models. If \code{FALSE}, the ouput will be single line. Only applicable when there are more than one model.
#' @param custom.footnote Custom footnote (character). The default is \code{NULL}. If assigned, footnote are added to the plot by \code{\link{plot_footnote}} function, and the function exports \code{gtable} object. Note that \code{gtable} object is less customizable than \code{ggplot} object. If it is also the case that \code{footnote.gof == TRUE}, custom footnote will be added as the new line after the GOF footnote.
#' @param footnote.fontsize The size of font. If \code{NULL} (default), the size is set to the the font size in \code{text} setting of ggplot theme - 1.
#' @param footnote.fontcol The color of the font. The default is \code{"black"}.
#' @param footnote.align The alignment of the footnote text. Either \code{"right"} or \code{"left"}.
#' @param footnote.distance.from.side The horizontal distance of notes from the edge of graph space by the proportion of graph width (numeric: 0-1). The default is \code{0.05}. The distance is measured from the side specified in \code{align}.
#' @param footnote.distance.from.bottom The vertical distance of notes from the bottom of graph bottom by the proportion of bottom graph margin height (numeric: 0-1). The default is \code{0.75}.
#' @param footnote.bottom.expand.rate The expansion rate of the bottom margin of the graph to incorporate footnote (numeric). The value of \code{1} indicates no expansion. If \code{NULL} (default), it is set to the number of lines in the footnote + 1.
#' @param show.plot Print the plot at the end of function (boulean). The default is \code{TRUE}.
#' @param flip.plot Flip the x and y axis of the plot. The default is \code{FALSE}. If \code{TRUE}, the variable names will be printed as columns and coefficients will be printed as rows. The original design does not intended for flipping the axis, thus flipping it may cause problems in the layout.
#'
#' @return \code{ggplot} object without footnote (axis and theme settings can be added later). If footnote is added, then \code{gtable} object is the output. It is impossible to add \code{ggplot} elements to the \code{gtable} object. \code{gtable} plot can be viewed by using either \code{\link[grid]{grid.draw}()} or \code{\link[graphics]{plot}()} function.
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
#' plot_coef(cfci)
#'
#' ## Estimate Model by Male and Female Subset
#' m_male <- glm(fm, data = vote92[vote92$female==0,],
#'          family = binomial("logit"))
#' m_female <- glm(fm, data = vote92[vote92$female==1,],
#'               family = binomial("logit"))
#'
#' ## Overlap Subsetted Results
#' plot_coef(list(m_male, m_female),
#'           m.names = c("Male", "Female"))
#'
#' ## Add Title and Custom Variable Names
#' vn <- c("(Intercept)",
#'         "Democrat","Republican",
#'         "Ideological Distance from Clinton",
#'         "Ideological Distance from Bush",
#'         "Retrospective Personal Finance",
#'         "Retrospective National Economy")
#' plot_coef(list(m_male, m_female),
#'           m.names = c("Male", "Female"),
#'           title = "Vote for Bush (1992)",
#'           custom.variable.names = vn)
#'
#' ## Overlap the Third Model & Facet Variables by Category & Add GOF Footnote
#' fn <- c("Constant",rep("Preference",4),rep("Evaluation",2))
#' fn <- factor(fn,levels=unique(fn))
#' plot_coef(list(m_male, m_female, m),
#'           m.names = c("Male", "Female", "All"),
#'           facet.category.names = fn,
#'           title = "Vote for Bush (1992)",
#'           custom.variable.names = vn,
#'           footnote.gof = TRUE)
#'
#' @references \url{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/} is where my initial idea come from.
#'
#' @importFrom stats reorder
#' @import MASS
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 coord_cartesian
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
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 waiver
#' @import pscl
#'
#' @export
plot_coef<-function(m,
                    m.names = NULL,
                    order.variable="original",
                    odds=FALSE,
                    vcov.est = NULL,
                    sims.est = NULL,
                    show.ci = TRUE,
                    level.ci = 0.95,
                    drop.intercept=FALSE,
                    drop.intercept.names = "(Intercept)",
                    drop.variable.names = NULL,
                    point.shape = 16,
                    point.size = 1.5,
                    ci.linetype = 1,
                    ci.size = 0.5,
                    ci.height = 0.2,
                    overlap.gapwidth = 0.5,
                    overlap.shape.index = NULL,
                    overlap.linetype.index = NULL,
                    overlap.legend.position = "bottom",
                    facet.category.names = NULL,
                    facet.names.location = "left",
                    facet.names.angle = 0,
                    title = NULL,
                    ytitle = NULL,
                    custom.variable.names = NULL,
                    custom.x.title = NULL,
                    custom.x.lim = NULL,
                    custom.x.breaks = NULL,
                    custom.themes = NULL,
                    footnote.gof = FALSE,
                    footnote.gof.nobs = "right",
                    footnote.gof.extracts = c("AIC", "Adj. R$^2$", "Log Likelihood"),
                    footnote.gof.reorder = NULL,
                    footnote.gof.linebreak = TRUE,
                    custom.footnote = NULL,
                    footnote.fontsize = NULL,
                    footnote.fontcol = "black",
                    footnote.align = "right",
                    footnote.distance.from.side = 0.05,
                    footnote.distance.from.bottom = 0.75,
                    footnote.bottom.expand.rate = NULL,
                    show.plot = TRUE,
                    flip.plot = FALSE
                    ){

# Set Default Graph Theme
gtheme <- estvis.theme() +
 theme(panel.grid.major.x = element_blank(), # x axis grid line (major)
       panel.grid.minor.x = element_blank()) # x axis grid line (minor))

# Split into the first model and the rest.
if (class(m)[1] != "list") {
  overlap.m.list <- NULL
  overlap.vcov.est.list <-  NULL
  overlap.class.names <- NULL
} else {
  overlap.m.list <- m[2:length(m)]
  if (is.null(vcov.est)==FALSE){
    overlap.vcov.est.list <- vcov.est[2:length(vcov.est)]
  } else {
    overlap.vcov.est.list <- NULL
  }
  m <- m[[1]]
  vcov.est <- vcov.est[[1]]
  if (is.null(m.names)==TRUE) {
    overlap.class.names <- rep(NA,length(overlap.m.list)+1)
    for (i in 1:(length(overlap.m.list)+1)) {
      overlap.class.names[i] <- paste("Model", i)
    }
  } else {
    overlap.class.names <- m.names
  }
}

# Identify if the input is table
if ((class(m)[1] %in% c("matrix","data.frame"))==TRUE) {
  direct = TRUE
  if(ncol(m) < 3){
    stop("Less than three columns in the matrix")
  } else if (ncol(m) > 3) {
    warning("More than three columns in the matrix. Only first three columns are used")
    m <- m[,1:3]
  }
  if (is.null(rownames(m))==TRUE) {
    stop("No row names for the matrix. Add them by rownames(m)")
  }
} else {
  direct = FALSE
}

## Import Coefficients
if (direct==TRUE){
  tmp <- m
  originalm <- m
  varnames <- row.names(originalm)
  facetnames <- facet.category.names
  if (is.null(overlap.m.list)==FALSE) {
    for(i in 1:length(overlap.m.list)){
      m <- rbind(m,overlap.m.list[[i]])
    }
    tmp <- m
    varnames <- rep(row.names(originalm),length(overlap.m.list)+1)
    facetnames <- rep(facet.category.names,length(overlap.m.list)+1)
  }
} else if (direct==FALSE) {
  pci <- level.ci
  tmp <- matrix_coefci (m, level=pci, vcov. = vcov.est, sims = sims.est)
  originalm <- tmp
  varnames <- row.names(originalm)
  facetnames <- facet.category.names
  if (is.null(overlap.m.list)==FALSE) {
    if(is.null(overlap.vcov.est.list)==TRUE){
      overlap.vcov.est.list <- vector("list", length(overlap.m.list))
    }
    for(i in 1:length(overlap.m.list)){
      tmp <- rbind(tmp,matrix_coefci (overlap.m.list[[i]], level=pci, vcov. = overlap.vcov.est.list[[i]], sims = sims.est))
    }
    varnames <- rep(row.names(originalm),length(overlap.m.list)+1)
    facetnames <- rep(facet.category.names,length(overlap.m.list)+1)
  }
}
if (odds){
  tmp <- exp(tmp)
}
coefs <- tmp
names(coefs)<-c('CF', 'lower', 'upper')
coefs$vars <- varnames

## Variables for overlaps and facettng
if (is.null(overlap.m.list)==FALSE) {
  coefs$overlap <- rep(overlap.class.names, each=nrow(originalm))
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
  if (is.null(overlap.m.list)==FALSE) {
    coefs$vars <- rep(custom.variable.names, length(overlap.m.list)+1)
  }
}

## Change Setting for ticks if Odds Ratio
if (odds){
  ticks <- c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
} else {
  ticks <- waiver()
}
## Override ticks if custom.x.breaks is not NULL.
if (is.null(custom.x.breaks)==FALSE){
  ticks <- custom.x.breaks
}

## Settings for Overlapped Outputs
if (is.null(overlap.m.list)==FALSE) {
  classn <- length(overlap.m.list) + 1
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
  plotstart <- ggplot(coefs, aes(y= CF, x = vars )) +
    gtheme + coord_cartesian(ylim=custom.x.lim)
} else if (order.variable=="cfdescend") {
  plotstart <- ggplot(coefs, aes(y= CF, x = reorder(vars, CF) )) +
    gtheme + coord_cartesian(ylim=custom.x.lim)
} else if (order.variable=="original") {
  plotstart <- ggplot(coefs, aes(y= CF, x = reorder(vars, (length(vars)+1) - seq(1,length(vars),1)) )) +
    gtheme + coord_cartesian(ylim=custom.x.lim)
}

## Flip the Plot if flip.plot = FALSE
if (flip.plot == FALSE) {
  plotstart <- plotstart + coord_flip()
}

## Override by Custom Themes
if (is.null(custom.themes)==FALSE){
  plotstart <- plotstart + custom.themes
}

## Intermediate Plot
if (is.null(overlap.m.list)==TRUE) {
  plotmid <- plotstart + geom_point(shape=point.shape, size=point.size)
  if (show.ci==TRUE) {
    plotmid <- plotmid +
    geom_errorbar(aes(ymin=lower, ymax=upper), linetype=ci.linetype, width=ci.height, size=ci.size)
  }
} else {
  plotmid <- plotstart +
  geom_point(aes(shape=overlap), size=point.size, position=position_dodge(width = -overlap.gapwidth)) +
  scale_shape_manual(name="Class",values=overlap.shapes)+
  theme(legend.position=overlap.legend.position)
  if (show.ci==TRUE) {
    plotmid <- plotmid +
    geom_errorbar(aes(ymin=lower, ymax=upper, linetype=overlap), width=ci.height, size=ci.size, position=position_dodge(width = -overlap.gapwidth)) +
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
  if (show.ci==TRUE) {
    ylabtxt <- paste('Odds Ratio with ', level.ci*100, '% Confidence Interval', sep="")
  } else {
    ylabtxt <- 'Odds Ratio'
  }
  if (is.null(custom.x.title)==FALSE) {
    ylabtxt <- custom.x.title
  }
  plotfin <- plotmid + scale_y_log10(breaks=ticks, labels = ticks) +
  scale_x_discrete() +
  geom_hline(yintercept = 1, linetype=2) +
  labs(title = title, x = ytitle, y = ylabtxt)
} else {
  if (show.ci==TRUE) {
    ylabtxt <- paste('Coefficient with ', level.ci*100, '% Confidence Interval', sep="")
  } else {
    ylabtxt <- 'Coefficient'
  }
  if (is.null(custom.x.title)==FALSE) {
    ylabtxt <- custom.x.title
  }
  plotfin <- plotmid + scale_y_continuous(breaks=ticks, labels = ticks) +
  scale_x_discrete() +
  geom_hline(yintercept = 0, linetype=2) +
  labs(title = title, x = ytitle, y = ylabtxt)
}

## Adding Footnote
if (footnote.gof == TRUE) {
  if (direct == TRUE) {
    stop("The model object is required to extract GOFs")
  } else {
    if (is.null(overlap.m.list)==FALSE) {
      mlist <- append(list(m), overlap.m.list)
      footnotechr <- extract_gofchr(m = mlist,
                               m.names = overlap.class.names,
                               nobs = footnote.gof.nobs,
                               gof.extracts = footnote.gof.extracts,
                               gof.reorder = footnote.gof.reorder,
                               linebreak = footnote.gof.linebreak
                               )
    } else {
      footnotechr <- extract_gofchr(m = m,
                                    nobs = footnote.gof.nobs,
                                    gof.extracts = footnote.gof.extracts,
                                    gof.reorder = footnote.gof.reorder,
                                    linebreak = footnote.gof.linebreak
                                    )
    }
    if (is.null(custom.footnote) == FALSE) {
      footnotechr <- paste(footnotechr, custom.footnote, sep="\n")
    }
    plotfin <- plot_footnote(plotfin, footnotechr,
                             fontsize = footnote.fontsize,
                             fontcol = footnote.fontcol,
                             align = footnote.align,
                             distance.from.side = footnote.distance.from.side,
                             distance.from.bottom = footnote.distance.from.bottom,
                             bottom.expand.rate = footnote.bottom.expand.rate,
                             show.plot = FALSE)
  }
} else if (is.null(custom.footnote) == FALSE) {
  plotfin <- plot_footnote(plotfin, custom.footnote,
                           fontsize = footnote.fontsize,
                           fontcol = footnote.fontcol,
                           align = footnote.align,
                           distance.from.side = footnote.distance.from.side,
                           distance.from.bottom = footnote.distance.from.bottom,
                           bottom.expand.rate = footnote.bottom.expand.rate,
                           show.plot = FALSE)
}

# Display New Plot
if(show.plot == TRUE){
  plot(plotfin)
}

## Return the Plot
return(plotfin)

}
