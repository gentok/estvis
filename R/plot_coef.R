# Following variables are global
globalVariables(c("CF", "upperCI", "lowerCI","overlapnames","vars"))

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

  # Replace Some Latex Expressions
  gofchr <- gsub("$^2$", ' sq.', gofchr, fixed=TRUE)

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
#' @param m.names The set of names that identifies each element in \code{m}. 
#' Considered if \code{m} is a list of models. 
#' The length of the vector must correspond with the length of \code{m}. 
#' If \code{NULL} (default), each element \code{i} is temporarily named as \code{Model i}.
#' @param order.variable Order of coefficients in the plot(character/numeric vector). 
#' \code{"original"} (default) preserves the original order of the variables. 
#' \code{"cfdescend"} plots by the descending order of coefficient values. 
#' \code{"cfascend"} plots by the ascending order of coefficient values.
#' Alternatively, you can also specify the order of variables by numeric vector 
#' (applied after \code{drop.intercept} and \code{drop.variable.names} are applied, 
#' thus you don't need the names for dropped variables).
#' @param odds Use odds ratio instead of coefficient in the output (boulean). 
#' The default is \code{FALSE}. 
#' If \code{TRUE}, the exponent of the coefficients will be plotted.
#' @param vcov.est Single or a list of alternative variance-covariance matrix. 
#' Each element must be one of raw variance-covariance matrix, \code{"robust"}, 
#' \code{"cluster"}, \code{"boot"}, or \code{NULL} (default).
#' If \code{"robust"}, robust standard error (also see \code{robust.type}).
#' If \code{"cluster"}, cluster robust standard error (also see \code{cluster.var}).
#' if \code{"boot"}, bootstrapped standard error calculated by \code{\link[car]{Boot}} 
#' function is used.
#' Ignored if \code{NULL} (default) or the \code{m} is not model object. 
#' Must have the same length as \code{m} if it is a list.
#' @param robust.type The type of leverage adjustment passed to \code{[sandwich]{vcovHC}} 
#' (applied only when \code{vcov.est=="robust"}).
#' @param cluster.var Single or a list of \code{vector}, \code{matrix}, or \code{data.frame} 
#' of cluster variables, where each column is a separate variable. Alternatively, 
#' a formula specifying the cluster variables to be used 
#' (see details in \code{\link[multiwayvcov]{cluster.vcov}}. Applied only when \code{vcov.est=="cluster"}.)
#' @param boot.sims Number of iterations if bootstrap is used. 
#' @param boot.seed Random number seed if bootstrap method is used.
#' @param boot.ncores Number of cores to parallelize bootstrap. The default is \code{1}. Use \code{"auto"} to automatically detect number of cores in computer.
#' @param show.ci Show confidence interval (boulean). The default is \code{TRUE}.
#' @param level.ci The level used for confidence interval (numeric: 0-1). 
#' The default is \code{0.95}.
#' @param drop.intercept Drop the intercept from the plot (boulean). 
#' If \code{FALSE} (default), intercept included in the plot.
#' @param drop.intercept.names The name(s) of intercept (character/character vector). 
#' Needed if \code{drop.intercept} is \code{TRUE}. 
#' This is used to identify and eliminate intercept variables from the output. 
#' Default value is \code{"(Intercept)"}.
#' @param drop.variable.names The name(s) of additional variables to drop 
#' (character/character vector) from the ouput. The default is \code{NULL}.
#' @param point.shape Shape of the point outputs (numeric/character). 
#' The default is \code{16} (filled circle).
#' @param point.size Size of point outputs (numeric). The default is \code{1.5}.
#' @param ci.linetype The line type of confidence interval outputs (numeric). 
#' The default is \code{1}.
#' @param ci.size The line width of confidence interval outputs (numeric). 
#' The default is \code{0.5}.
#' @param ci.height The height of the vertical line added to the edge of confidence 
#' interval outputs. The default is \code{0.2}.
#' @param overlap.names Model labels that controls overlapping 
#' (applied only when there are two or more \code{m}s). 
#' Default is \code{"m.names"} (use values specified in \code{m.names}). 
#' You can alternatively give a character vector of the same length as \code{m}.
#' @param overlap.gapwidth The gap between overlapped ouputs (numeric). 
#' The default value is \code{0.5}.
#' @param overlap.shape.index The index of shapes for overlapped point ouputs. 
#' Must be in the same length as the list \code{m}. 
#' The first element of the vector is the shape for \code{m}, 
#' then from the second element, the order must correspond with the order in the list \code{m}. 
#' If \code{NULL}, \code{point.shape} is applied to all classes.
#' @param overlap.linetype.index The index of line types for overlapped confidence interval 
#' ouputs. Must be in the same length as the list \code{m}. 
#' The first element of the vector is the shape for \code{m}, then from the second element, 
#' the order must correspond with the order in the list \code{m}. If \code{NULL}, 
#' the number corresponding with the order is assigned to each class.
#' @param overlap.legend.position The position of the legend for overlapping classess. 
#' See \code{legend.position} in ggplot theme for possible values. 
#' The default is \code{"bottom"}.
#' @param category.names The categories of variables (factor). 
#' If not \code{NULL}, the output provides the separate panels for variables in each category. 
#' The length of the vector must much with the number of variables in \code{m} 
#' (applied after \code{drop.intercept} and \code{drop.variable.names} are applied, 
#' thus you don't need the names for dropped variables).
#' @param category.names.location The location of category names. Either \code{"left"} or \code{"right"}. The default is \code{"left"}.
#' @param category.names.angle The angle of category names (numeric). The default is \code{0} (horizontal).
#' @param facet.names If not \code{NULL}, facet models by this identifier. Use \code{"m.names"} to facet each model separately (\code{overlap.names} is forced to \code{NULL}). 
#' Alternatively, assign character vector of the same length as \code{m} (number of models).  
#' @param title Plot title (character). The default is to include no title.
#' @param y.title Y axis title (character). The default is to include no axis title.
#' @param custom.variable.names Set of alternative variable names in the output (character vector). The default is \code{NULL}. This is applied AFTER \code{drop.intercept} and \code{drop.variable.names} are applied, thus you don't need the names for dropped variables.
#' @param custom.x.title Custom name for the X axis title (character). The default is \code{NULL}.
#' @param custom.x.lim Custom limit for the X axis. The default is \code{NULL}.
#' @param custom.x.breaks Custom breaks for the X axis. The default is \code{NULL}.
#' @param custom.themes ggplot themes that overrides the default theme. The default is \code{NULL}.
#' @param footnote.gof Include GOF measures in the footnote (boulean). The default is \code{FALSE}. If \code{TRUE}, footnote with GOF measures are added to the plot by \code{\link{extract_gofchr}} and \code{\link{plot_footnote}} function, and the function exports \code{gtable} object. Note that \code{gtable} object is less customizable than \code{ggplot} object.
#' @param footnote.gof.nobs The position of "number of observation (N)" to be printed. The default is \code{"right"} (the end of the sentence). \code{"left"} (the start of the sentence) and \code{"none"} (do not print number of observation) are also available.
#' @param footnote.gof.extracts GOF measures to be inluded if \code{footnote.gof == TRUE}. See \code{\link{extract_gofchr}} documentation for more details.
#' @param footnote.gof.reorder The numeric vector specifying the alternative order of GOF  (e.g., if there are three GOFs, and you want to flip the order of first and second GOF, then input \code{c(2,1,3)}). The length of the vector must correspond with the number of exported GOFs (it may not be the same as the length of \code{footnote.gof.extracts}). If \code{NULL} (default), the original order is used. The order does not include the number of observation (if exported).
#' @param footnote.gof.linebreak Include linebreak between models (boulean). If \code{TRUE} (default), linebreak will be included between models. If \code{FALSE}, the ouput will be single line. Only applicable when there are more than one model.
#' @param custom.footnote Custom footnote (character). The default is \code{NULL}. If assigned, footnote are added to the plot by \code{\link{plot_footnote}} function, and the function exports \code{gtable} object. Note that \code{gtable} object is less customizable than \code{ggplot} object. If it is also the case that \code{footnote.gof == TRUE}, custom footnote will be added as the new line after the GOF footnote.
#' @param footnote.caption Use caption option in ggplot2 to add footnote. If \code{TRUE}, all footnote options below are ignored.
#' @param footnote.fontsize The size of font. If \code{NULL} (default), the size is set to the the font size in \code{text} setting of ggplot theme - 1.
#' @param footnote.fontcol The color of the font. The default is \code{"black"}.
#' @param footnote.align The alignment of the footnote text. Either \code{"right"} or \code{"left"}.
#' @param footnote.distance.from.side The horizontal distance of notes from the edge of graph space by the proportion of graph width (numeric: 0-1). The default is \code{0.05}. The distance is measured from the side specified in \code{align}.
#' @param footnote.distance.from.bottom The vertical distance of notes from the bottom of graph bottom by the proportion of bottom graph margin height (numeric: 0-1). The default is \code{0.75}.
#' @param footnote.bottom.expand.rate The expansion rate of the bottom margin of the graph to incorporate footnote (numeric). The value of \code{1} indicates no expansion. If \code{NULL} (default), it is set to the number of lines in the footnote + 1.
#' @param show.plot Print the plot at the end of function (boulean). The default is \code{TRUE}.
#' @param flip.plot Flip the x and y axis of the plot. The default is \code{FALSE}. If \code{TRUE}, the variable names will be printed as columns and coefficients will be printed as rows. The original design does not intended for flipping the axis, thus flipping it may cause problems in the layout.
#' @param ... Additional arguments passed to \code{[sandwich]{vcovHC}}, \code{[multiwayvcov]{cluster}}, or \code{\link[car]{Boot}} depending on the value of \code{vcov.est}. 
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
#'           category.names = fn,
#'           title = "Vote for Bush (1992)",
#'           custom.variable.names = vn,
#'           footnote.gof = TRUE)
#' 
#' ## Further Adding Facets 
#' plot_coef(list(m_male, m_female, m),
#'           m.names = c("Male", "Female", "ALL"),
#'           overlap.linetype.index = c(1,2,1),
#'           category.names = fn,
#'           facet.names = c("Gender Subsets","Gender Subsets","All"),
#'           title = "Vote for Bush (1992)",
#'           custom.variable.names = vn,
#'           footnote.gof = TRUE)
#' 
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
#' @importFrom grid grid.draw
#'
#' @export
plot_coef<-function(m,
                    m.names = NULL,
                    order.variable="original",
                    odds=FALSE,
                    vcov.est = NULL, # or "robust" or "cluster", "boot" or raw vcov 
                    robust.type = "HC1",
                    cluster.var = NULL,
                    boot.sims = 300,
                    boot.seed = 578,
                    boot.ncores = 1,
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
                    overlap.names = "m.names",
                    overlap.gapwidth = 0.5,
                    overlap.shape.index = NULL,
                    overlap.linetype.index = NULL,
                    overlap.legend.position = "bottom",
                    category.names = NULL,
                    category.names.location = "left",
                    category.names.angle = 0,
                    facet.names = NULL,
                    title = NULL,
                    y.title = NULL,
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
                    footnote.caption = FALSE,
                    footnote.fontsize = NULL,
                    footnote.fontcol = "black",
                    footnote.align = "right",
                    footnote.distance.from.side = 0.05,
                    footnote.distance.from.bottom = 0.75,
                    footnote.bottom.expand.rate = NULL,
                    show.plot = TRUE,
                    flip.plot = FALSE,
                    ...
                    )
{
  # Set Default Graph Theme
  gtheme <- estvis.theme() +
    theme(panel.grid.major.x = element_blank(), # x axis grid line (major)
          panel.grid.minor.x = element_blank()) # x axis grid line (minor))
  
  # Convert to a list if target is a model object
  if (class(m)[1] != "list") m <- list(m)
  
  # (vcov.list) Convert to a list if target is not a list
  if (class(vcov.est)[1]=="list") {
    vcov.list <- vcov.est
  } else {
    vcov.list <- list(vcov.est)
  }
  # (vcov.list) If length==1, replicate by the length of m
  if (length(m)>1 & length(vcov.list)==1) {
    tmp <- vcov.list[[1]]
    if (is.null(tmp)) tmp <- NA
    if (class(tmp)=="matrix") {
      stop("m and vcov.est must have the same length.")
    } else {
      for (i in 1:length(m)) vcov.list[[i]] <- tmp
    }
  }
  
  # (cluster.list) Convert to a list if target is not a list
  if (class(cluster.var)[1]=="list") {
    cluster.list <- cluster.var
  } else {
    cluster.list <- list(cluster.var)
  }
  # (cluster.list) If length==1, replicate by the length of m
  if (length(vcov.list)>1 & length(cluster.list)==1) {
    tmp <- cluster.list[[1]]
    if (is.null(tmp)) tmp <- NA
    for (i in 1:length(m)) cluster.list[[i]] <- tmp
  }
  
  # Check Length
  if (length(m)!=length(vcov.list)) {
    stop("m and vcov.est must have the same length.")
  }
  if (length(m)!=length(cluster.list)) {
    stop("m and cluster.var must have the same length.")
  }
  
  # Default Model Names
  if (is.null(m.names)) m.names <- paste("Model", seq(1,length(m),1))
  if (length(m)!=length(m.names)) {
    stop("m and m.names must have the same length.")
  }
  if (length(m.names)!=length(unique(m.names))) {
    stop("Each m.names must have unique values.")
  }
  
  # Default Facet Names
  add.facet <- TRUE
  if (is.null(facet.names)) {
    add.facet <- FALSE
    facet.names <- rep(NA, length(m))
  } else if (facet.names[1]=="m.names") {
    facet.names <- m.names
    if (overlap.names=="m.names") {
      overlap.names <- NULL
      warning("overlap.names set to NULL.")
    }
  }
  if (length(m)!=length(facet.names)) {
    stop("m and facet.names must have the same length.")
  }
  
  # Default Overlap Model Names
  add.overlap <- FALSE
  if (!is.null(overlap.names)) {
    if (overlap.names[1]=="m.names") overlap.names <- m.names
  }
  if (length(overlap.names)==0){
    overlap.names <- rep(NA, length(m))
  } else if (length(overlap.names)>=1) {
    if (length(overlap.names)>1) add.overlap <- TRUE
    if (length(m)!=length(overlap.names)) {
      stop("m and overlap.names must have the same length.")
    }
  }
  
  # Identify if the input is table
  direct <- list()
  for (i in 1:length(m)) {
    if ((class(m[[i]])[1] %in% c("matrix","data.frame"))==TRUE) {
      direct[[i]] = TRUE
      if(ncol(m[[i]]) < 3){
        stop("Less than three columns in the matrix")
      } else if (ncol(m[[i]]) > 3) {
        warning("More than three columns in the matrix. Only first three columns are used")
        m[[i]] <- m[[i]][,1:3]
      }
      if (is.null(rownames(m[[i]]))==TRUE) {
        stop("No row names for the matrix. Add them by rownames(m)")
      }
    } else {
      direct[[i]] = FALSE
    }
  }
  
  ## Import Coefficients
  coefs <- NULL
  for(i in 1:length(m)) {
    if (direct[[i]]==TRUE) {
      if(is.data.frame(m[[i]])) {
        tmp <- m[[i]]
      } else {
        tmp <- as.data.frame(m[[i]][,])
      }
    } else if (direct[[i]]==FALSE) {
      # Put NULL Back into vcov
      vcov_i <- vcov.list[[i]]
      if (!is.null(vcov_i)) {
        if (is.na(vcov_i)) vcov_i <- NULL
      }
      # Put NULL Back into cluster
      cluster_i <- cluster.list[[i]]
      if (!is.null(cluster_i)) {
        if (class(cluster_i)=="formula") {
        } else if (is.na(cluster_i[1])) {
          cluster_i <- NULL
        }
      }
      tmp <- matrix_coefci(m[[i]], level=level.ci, vcov.est = vcov_i, 
                           robust.type = robust.type, cluster.var = cluster_i,
                           boot.sims = boot.sims, boot.seed = boot.seed, 
                           ncores = boot.ncores, ...)
    }
    if (odds==TRUE) tmp <- exp(tmp)
    colnames(tmp)[1:3] <- c("CF", "lowerCI", "upperCI")
    tmp$vars <- rownames(tmp)
    rownames(tmp) <- NULL
    tmp$m.names <- m.names[i]
    tmp$overlapnames <- overlap.names[i]
    tmp$facetnames <- facet.names[i]
    if (is.null(coefs)) {
      coefs <- tmp
    } else {
      coefs <- rbind(coefs, tmp)
    }
  }
  coefs$m.names <- factor(coefs$m.names,levels=m.names)
  coefs$overlapnames <- factor(coefs$overlapnames,levels=unique(overlap.names))
  coefs$facetnames <- factor(coefs$facetnames,levels=unique(facet.names))
  
  ## Variable Names
  var.names <- unique(coefs$vars)
  
  ## Drop variables (if there is any)
  dropvars <- character()
  if (drop.intercept) dropvars <- c(dropvars,drop.intercept.names)
  if (!is.null(drop.variable.names)) drop.vars <- c(dropvars, drop.variable.names)
  if (length(dropvars) > 0) {
    coefs <- coefs[-which(coefs$vars %in% dropvars),]
  }
  
  ## Assign Variable Category
  add.cats <- FALSE
  if (!is.null(category.names)) {
    add.cats <- TRUE
    if (length(unique(coefs$vars))==length(category.names)) {
      coefs$cats <- NA
      if (!is.factor(category.names)) category.names <- as.factor(category.names)
      coefs$cats <- factor(coefs$cats, levels=levels(category.names))
      for(i in 1:length(var.names)) {
        coefs$cats[which(coefs$vars == var.names[i])] <- category.names[i]
      }
    } else {
      warning("The length of category.names does not match with the number of unique variable names.
            category.names not assigned.")
      coefs$cats <- NA
      add.cats <- FALSE
    }
  } else {
    coefs$cats <- NA
  }
  
  ## Assign Custom Variable Names
  if (!is.null(custom.variable.names)) {
    var.names.2 <- unique(coefs$vars)
    if (length(var.names.2)==length(custom.variable.names)) {
      vartemp <- coefs$vars
      for(i in 1:length(var.names.2)) {
        coefs$vars[which(vartemp == var.names.2[i])] <- custom.variable.names[i]
      }
    } else {
      warning("The length of custom.variable.names does not match with the number of 
              unique variable names. Custom variable names not assigned.")
    }
  }
  
  ## Print Variable Conversion
  catnames <- rep(NA, length(var.names))
  if (add.cats==TRUE){
    if (length(dropvars)==0) {
      catnames <- category.names
    } else {
      catnames[-which(var.names %in% dropvars)] <- category.names
    }
  } 
  convres <- data.frame(Category = catnames,
                        Dropped = "KEPT",
                        Original = var.names)
  if (length(dropvars)==0) {
    convres$Final <- unique(coefs$vars)
  } else {
    convres$Final <- NA
    convres$Final[-which(var.names %in% dropvars)] <- unique(coefs$vars)
    convres$Dropped[which(var.names %in% dropvars)] <- "DROPPED"
  }
  cat("Variable Manipulations: \n  ")
  print(convres, row.names=FALSE)
  
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
  if (add.overlap) {
    classn <- length(overlap.names)
    if(is.null(overlap.shape.index)==FALSE){
      overlap.shapes <- overlap.shape.index
    } else {
      overlap.shapes <- rep(point.shape,classn)
    }
    if(is.null(overlap.linetype.index)==FALSE){
      overlap.linetypes <- overlap.linetype.index
    } else {
      overlap.linetypes <- seq(1,classn,1)
    }
  }

  vn0 <- unique(coefs$vars)
  addvn <- character()
  addcn <- character()
  addmn <- character()
  addon <- character()
  addfn <- character()
  for(i in 1:length(m.names)) {
    vn1 <- coefs$vars[coefs$m.names==m.names[i]]
    vntmp <- vn0[-which(vn0 %in% vn1)]
    addvn <- c(addvn, vntmp)
    if (add.cats) {
      cntmp <- category.names[-which(vn0 %in% vn1)]
    } else {
      cntmp <- rep(NA, length(vntmp))
    }
    addcn <- c(addcn, cntmp)
    addmn <- c(addmn, rep(m.names[i], length(vntmp)))
    addon <- c(addon, rep(overlap.names[i], length(vntmp)))
    addfn <- c(addmn, rep(facet.names[i], length(vntmp)))
  }
  if (length(addvn)>0) {
    addcoefs <- as.data.frame(matrix(NA, ncol=ncol(coefs), nrow=length(addvn)))
    colnames(addcoefs) <- colnames(coefs)
    addcoefs$vars <- addvn
    addcoefs$cats <- addcn
    addcoefs$m.names <- addmn
    addcoefs$overlapnames <- addon
    addcoefs$facetnames <- addfn
    coefs <- rbind(coefs, addcoefs)
  }
  
  # Identify Variable Names After Dropping Variables 
  coefs$vars <- factor(coefs$vars, levels=rev(unique(coefs$vars)))
  
  ## Start Plotting
  if (is.numeric(order.variable)) {
    coefs$vars <- factor(as.character(coefs$vars), 
                        levels=unique(as.character(coefs$vars))[order.variable])
    p <- ggplot(coefs, aes(y= CF, x = vars)) +
      gtheme
  } else if (order.variable=="original"){
    p <- ggplot(coefs, aes(y= CF, x = vars)) +
      gtheme
  } else if (order.variable=="cfdescend") {
    p <- ggplot(coefs, aes(y= CF, x = reorder(vars, CF) )) +
      gtheme
  } else if (order.variable=="cfascend") {
    p <- ggplot(coefs, aes(y= CF, x = reorder(vars, -CF) )) +
      gtheme
  } 
  
  ## Flip the Plot if flip.plot = FALSE
  if (flip.plot == FALSE) {
    p <- p + coord_flip(ylim=custom.x.lim)
  } else {
    p <- p + coord_cartesian(ylim=custom.x.lim)
  }
  
  ## Override by Custom Themes
  if (is.null(custom.themes)==FALSE){
    p <- p + custom.themes
  }
  
  ## Intermediate Plot
  if (add.overlap==FALSE) {
    p <- p + geom_point(shape=point.shape, size=point.size)
    if (show.ci==TRUE) {
      p <- p +
        geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), 
                      linetype=ci.linetype, width=ci.height, size=ci.size)
    }
  } else {
    p <- p +
      geom_point(aes(shape=overlapnames), 
                 size=point.size, 
                 position=position_dodge(width = -overlap.gapwidth)) +
      scale_shape_manual(name="Class",values=overlap.shapes) +
      theme(legend.position=overlap.legend.position)
    if (show.ci==TRUE) {
      p <- p +
        geom_errorbar(aes(ymin=lowerCI, ymax=upperCI, linetype=overlapnames), 
                      width=ci.height, size=ci.size, 
                      position=position_dodge(width = -overlap.gapwidth)) +
        scale_linetype_manual(name="Class",values=overlap.linetypes)
    }
  }
  
  ## Intermediate Plot 2 (If Facetted)
  if (add.cats==TRUE & add.facet==TRUE) {
    if (category.names.location=="left"){
      p <- p +
        facet_grid(cats ~ facetnames, margins=F, 
                   scales="free_y",space="free_y",switch="y") +
        theme(strip.placement = "outside",
              strip.text.y = element_text(size=11, angle=category.names.angle+180, face="bold", hjust=0),
              strip.text.x = element_text(size=11, face="bold"))
    } else if (category.names.location=="right"){
      if(category.names.angle>0){
        category.names.angle <- 360-category.names.angle
      }
      p <- p +
        facet_grid(cats ~ facetnames,margins=F,scales="free_y",space="free_y") +
        theme(strip.text.y = element_text(size=11, angle=category.names.angle, face="bold", hjust=1),
              strip.text.x = element_text(size=11, face="bold"))
    }
  } else if (add.cats==TRUE) {
    if (category.names.location=="left"){
      p <- p +
        facet_grid(cats ~ ., margins=F,scales="free_y",space="free_y",switch="y") +
        theme(strip.placement = "outside",
              strip.text.y = element_text(size=11, angle=category.names.angle+180, face="bold", hjust=0))
    } else if (category.names.location=="right"){
      if(category.names.angle>0){
        category.names.angle <- 360-category.names.angle
      }
      p <- p +
        facet_grid(cats ~ .,margins=F,scales="free_y",space="free_y") +
        theme(strip.text.y = element_text(size=11, angle=category.names.angle, face="bold", hjust=1))
    }
  } else if (add.facet==TRUE) {
    p <- p +
      facet_grid(. ~ facetnames, margins=F) +
      theme(strip.placement = "outside",
            strip.text.x = element_text(size=11, face="bold"))
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
    p <- p + scale_y_log10(breaks=ticks, labels = ticks) +
      scale_x_discrete() +
      geom_hline(yintercept = 1, linetype=2) +
      labs(title = title, x = y.title, y = ylabtxt)
  } else {
    if (show.ci==TRUE) {
      ylabtxt <- paste('Coefficient with ', level.ci*100, '% Confidence Interval', sep="")
    } else {
      ylabtxt <- 'Coefficient'
    }
    if (is.null(custom.x.title)==FALSE) {
      ylabtxt <- custom.x.title
    }
    p <- p + scale_y_continuous(breaks=ticks, labels = ticks) +
      scale_x_discrete() +
      geom_hline(yintercept = 0, linetype=2) +
      labs(title = title, x = y.title, y = ylabtxt)
  }
  
  ## Adding Footnote
  if (footnote.gof == TRUE) {
    if (TRUE %in% direct){
      stop("The model object is required to extract GOFs")
    } else {
      if (length(m)>1) {
        if (add.overlap==TRUE & length(m.names)==length(overlap.names)) {
          footnote.m.names <- overlap.names
        } else if (add.facet==TRUE & length(m.names)==length(facet.names)) {
          footnote.m.names <- facet.names
        } else {
          footnote.m.names <- m.names
        }
        footnotechr <- extract_gofchr(m = m,
                                      m.names = footnote.m.names,
                                      nobs = footnote.gof.nobs,
                                      gof.extracts = footnote.gof.extracts,
                                      gof.reorder = footnote.gof.reorder,
                                      linebreak = footnote.gof.linebreak
        )
      } else {
        footnotechr <- extract_gofchr(m = m[[1]],
                                      nobs = footnote.gof.nobs,
                                      gof.extracts = footnote.gof.extracts,
                                      gof.reorder = footnote.gof.reorder,
                                      linebreak = footnote.gof.linebreak
        )
      }
      if (is.null(custom.footnote) == FALSE) {
        footnotechr <- paste(footnotechr, custom.footnote, sep="\n")
      }
      p <- plot_footnote(p, footnotechr,
                               caption = footnote.caption,
                               fontsize = footnote.fontsize,
                               fontcol = footnote.fontcol,
                               align = footnote.align,
                               distance.from.side = footnote.distance.from.side,
                               distance.from.bottom = footnote.distance.from.bottom,
                               bottom.expand.rate = footnote.bottom.expand.rate,
                               show.plot = FALSE)
    }
  } else if (is.null(custom.footnote) == FALSE) {
    p <- plot_footnote(p, custom.footnote,
                             caption = footnote.caption,
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
    grid.draw(p)
  }
  
  ## Return the Plot
  if (class(p)[1]=="gtable") {
    return(invisible(p))
  } else {
    return(p)
  }
  
}


