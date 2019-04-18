#' Simulate conditional effect from models with interaction terms
#' 
#' @description Compute interaction effects through Monte-Carlo Simulation. Wrapper function of \code{\link{simu_pred}}.
#' 
#' @examples
#' 
#' ## Load Data
#' library(pscl)
#' data(vote92)
#'
#' ## Recode Variables
#' vote92$voteBush <- as.numeric(
#' factor(vote92$vote,levels=c("Clinton","Bush")))*1 - 1
#' vote92$bushdis <- sqrt(vote92$bushdis)
#' vote92$clintondis <- sqrt(vote92$clintondis)
#' 
#' ## Estimate Logistic Regression with Interaction
#' fm <- formula(voteBush ~ dem*clintondis + 
#'                 rep + bushdis +
#'                 persfinance + natlecon)
#' m <- glm(fm, data = vote92,
#'          family = binomial("logit"))
#'          
#' # Moving Values
#' moveprof <- data.frame(clintondis = seq(0,4,length=50))
#' # Simulation
#' interactprof <- simu_interact(m, "dem", moveprof=moveprof,
#'                               var1.label = "Being Democrat",
#'                               y.label = "Bush Vote")
#' # Plot
#' plot_interact(interactprof, 
#'               label.var2="Ideological Distance from Clinton")
#' 
#' # Alternative Way
#' interactprof <- simu_interact(m, "dem", var2 = "clintondis",
#'                               var1.label = "Being Democrat",
#'                               y.label = "Bush Vote")
#' plot_interact(interactprof,
#'               label.var2="Ideological Distance from Clinton")
#' 
#' @param m Single model object.
#' @param var1 Variable of coeffcients to be simulated.
#' @param var1.label Text label for \code{var1} (optional).
#' @param moveprof Data.frame object that specifies moving values for variables included in interaction terms other than \code{var1}.
#' The first variable in data.frame is considered as the primary variable for simulation. 
#' All variables not included in \code{moveprof} are assumed not to be interacted.
#' Alternatively, you can specify \code{var2} if only two variables are interacted in the model and 
#' you want to simulate coefficients for full range of values for the second variable. (Note: You must give one of \code{moveprof} and \code{var2}). 
#' @param var2 Character for the second variable name in interaction term. Must be given is \code{moveprof} is \code{NULL}.
#' @param steps Maximum number of values to be simulated. (Applied if \code{var2} is not \code{NULL} and \code{moveprof} is \code{NULL}).
#' @param y.label The label of the dependent variable (optional, character).
#' @param show.ci Show confidence interval (boulean). The default is \code{TRUE}.
#' @param level.ci The level used for confidence interval (numeric: 0-1). The default is \code{0.95}.
#' @param vcov.est Variance-covariance matrix to draw coefficents. 
#' If \code{"robust"}, use robust variance-covariance matrix (also see \code{robust.type}).
#' If \code{"cluster"}, use cluster robust variance-covariance matrix (also see \code{cluster.var}).
#' If \code{"boot"}, draw beta by non-parametric bootstrap method using \code{\link[car]{Boot}} function.
#' If \code{"rawbeta"}, use pre-simulated beta given in \code{rawbeta} argument.
#' If \code{NULL} (default), use the standard variance covariance matrix stored the model. You can also directly set the variance-covariance matrix created by \code{\link[stats]{vcov}()} or \code{\link[sandwich]{vcovHC}()}.
#' @param robust.type The type of leverage adjustment passed to \code{\link[sandwich]{vcovHC}} (applied only when \code{vcov.est=="robust"}).
#' @param cluster.var A \code{vector}, \code{matrix}, or \code{data.frame} of cluster variables, where each column is a separate variable. Alternatively, a formula specifying the cluster variables to be used (see Details in \code{\link[multiwayvcov]{cluster}}. Applied only when \code{vcov.est=="cluster"}.)
#' @param iterate.num The number of iteration in simulation.
#' @param iterate.seed The seed value for random number generator used for the draws from multivariate normal distribution.
#' @param rawbeta The matrix of pre-simulated beta. Columns are variables, raws are simulated cases. Used only when \code{vcov.est=="rawbeta"}.
#' @param ... Additional arguments passed to \code{\link[sandwich]{vcovHC}}, \code{\link[multiwayvcov]{cluster}}, or \code{\link[car]{Boot}} depending on the value of \code{vcov.est}. 
#' 
#' @return A list of: 
#' \itemize{
#'  \item{\code{predsum}}{Summary Predictions Table}
#'  \item{\code{profile}}{Profile Used for Predictions}
#'  \item{\code{predres}}{Raw Predictions}
#'  \item{\code{formula}}{Estimation Formula}
#'  \item{\code{y.label}}{Dependent Variable Label}
#'  \item{\code{family}}{Estimation Method Family}
#'  \item{\code{type}}{Output Type}
#'  \item{\code{moveprof}}{Profile of Moving Values}
#'  \item{\code{var1.label}}{The Label of Target variable}
#'  \item{\code{var2}}{Primary condition variable}
#' }
#' 
#' @export

simu_interact <- function(m,
                          var1,
                          var1.label = NULL,
                          moveprof = NULL,
                          var2 = NULL,
                          steps = 100, 
                          y.label = "auto",
                          show.ci = TRUE,
                          level.ci = 0.95,
                          vcov.est = NULL,
                          robust.type = "HC1",
                          cluster.var = NULL,
                          iterate.num = 1000,
                          iterate.seed = 578,
                          rawbeta = NULL,
                          ...){
  
  # Profile Adjustment
  if (is.null(var2) & is.null(moveprof)) {
    stop("One of var2 or moveprof must be given.") 
  } else if (!is.null(moveprof)) {
    if (!is.null(var2)) warning("var2 is ignored since moveprof is given.")
    if ("data.frame" %in% class(moveprof)) {
      profile <- moveprof
      profile[,var1] <- 1
    } else {
      stop("moveprof must be a data.frame.")
    }
  } else {
    v2o <- m$model[,var2]
    if (length(table(v2o))<=steps) {
      if (is.numeric(v2o) | is.integer(v2o)) {
        var2.vals <- as.numeric(names(table(v2o)))
      } else if (is.factor(v2o)) {
        var2.vals <- names(table(v2o))
        var2.vals <- factor(var2.vals,levels=levels(v2o))
      }
    } else {
      if (is.factor(v2o)) {
        stop(paste("Cannot take factor variable that have more than", steps,"levels."))
      } else {
        var2.vals <- seq(min(v2o),max(v2o),length=steps)
      }
    }
    profile <- data.frame(var2=var2.vals,var1=1)
    moveprof <- data.frame(var2=var2.vals)
    colnames(profile) <- c(var2,var1)
    colnames(moveprof) <- var2
  }
  
  # Dropping Irrelvant Beta
  if (class(m)[1]=="lm") m$formula <- m$terms
  temp <- m$model
  temp[,var1][1] <- NA
  temp <- model.matrix(m$formula, temp)[1,]
  dropbeta <- as.numeric(which(!is.na(temp)))
  if (length(dropbeta)==0) {
    dropbeta <- NULL
  }
  
  # Final Result
  simures <- simu_pred(m = m,
                     profile = profile,
                     y.label = y.label,
                     at = 0,
                     type = "asis",
                     show.ci = show.ci,
                     level.ci = level.ci,
                     vcov.est = vcov.est,
                     robust.type = robust.type,
                     cluster.var = cluster.var,
                     iterate.num = iterate.num,
                     iterate.seed = iterate.seed,
                     dropbeta = dropbeta,
                     rawbeta = rawbeta,
                     ...)
  simures[["moveprof"]] <- moveprof
  simures[["var1.label"]] <- ifelse(is.null(var1.label),var1,var1.label)
  simures[["var2"]] <- colnames(profile)[1]
  class(simures) <- c("simuinteract","list")
  return(simures)
}

#' Summary Class for \code{simuinteract} Object
#' 
#' @description Summarizing \code{simuinteract} object
#' 
#' @return Summary output
#' 
#' @param object \code{simuinteract} object
#' @param digits Number of digits to show when summarizing data.
#' @param print.rows Max number of profile rows to be printed.
#' @param ... Additional arguments.
#' 
#' @export

summary.simuinteract <- function(object, ..., digits=3, print.rows=15) {
  cat(paste0("\n Simulated Conditional Effect of ", object$var1.label, " on ", object$y.label))
  cat("\n\n Formula: \n")
  print(object$formula)
  cat("\n Summary Conditional Coefficients: \n")
  o1 <- round(object$predsum,digits)
  for(i in 1:ncol(object$moveprof)) {
    if (is.numeric(object$moveprof[,i])) {
      object$moveprof[,i] <- round(object$moveprof[,i],digits)
    }
  }
  if (nrow(o1)>print.rows) {
    print(cbind(o1,object$moveprof)[1:print.rows,])
    cat(paste0("\n There are ", nrow(o1), " rows in total. Omitted after ", print.rows, " rows."))
  } else {
    print(cbind(o1,object$moveprof))
  }
  
} 

#' Plot Interaction Effects
#'
#' @description Drawing interaction plot. This function is a wrapper for \code{\link{plot_simu}} function.
#' 
#' @examples
#' 
#' ## Load Data
#' library(pscl)
#' data(vote92)
#'
#' ## Recode Variables
#' vote92$voteBush <- as.numeric(
#' factor(vote92$vote,levels=c("Clinton","Bush")))*1 - 1
#' vote92$bushdis <- sqrt(vote92$bushdis)
#' vote92$clintondis <- sqrt(vote92$clintondis)
#' 
#' ## Estimate Logistic Regression with Interaction
#' fm <- formula(voteBush ~ dem*clintondis + 
#'                 rep + bushdis +
#'                 persfinance + natlecon)
#' m <- glm(fm, data = vote92,
#'          family = binomial("logit"))
#'          
#' # Moving Values
#' moveprof <- data.frame(clintondis = seq(0,4,length=50))
#' # Simulation
#' interactprof <- simu_interact(m, "dem", moveprof=moveprof,
#'                               var1.label = "Being Democrat",
#'                               y.label = "Bush Vote")
#' # Plot
#' plot_interact(interactprof, 
#'               label.var2="Ideological Distance from Clinton")
#' 
#' # Alternative Way
#' interactprof <- simu_interact(m, "dem", var2 = "clintondis",
#'                               var1.label = "Being Democrat",
#'                               y.label = "Bush Vote")
#' plot_interact(interactprof,
#'               label.var2="Ideological Distance from Clinton")
#'
#' @param interactprof Conditional coefficients in either \code{simuinteract} (exported from \code{\link{simu_interact}} 
#' function), \code{data.frame} or \code{matrix} class. If in \code{data.frame} or \code{matrix},
#' it should have rows as predcion cases and columns for mean/median prediction, 
#' lower and upper confidence intervals (if \code{show.ci==TRUE}), and values for 
#' predictors that varies across profiles.
#' @param name.var2 Column name for the main variable that conditions the coeffient of target variable (plotted on x axis). 
#' If not given, the information will be extracted from \code{interactprof} (only when it is \code{simuinteract} object). 
#' @param label.var2 X axis label. If \code{"name.var2"} (default), use the variable name defined in \code{name.var2}.
#' @param label.y Y axis label. If \code{"auto"} (default) and \code{interactprof} being 
#' \code{simuinteract} object, the label is automatically determined.
#' @param hline0 If \code{TRUE}, add horizontal line at the value of zero.
#' @param addprof Optional \code{data.frame} that adds or replaces values of conditions. 
#' Must have the same number of cases as \code{interactprof}. If 
#' variable name is found in \code{interactprof}, values are replaced. If not, additional variable 
#' is added to \code{interactprof}. Added variables can be used in \code{name.var2}, 
#' \code{name.color}, \code{name.linetype}, \code{name.fill}, and \code{name.shape}.
#' Define variable as \code{factor} if you want to control order of appearance in legend.
#' @param name.est Column name for mean/median conditional coefficient estiamte.
#' @param show.ci If \code{TRUE} (default), plot confidence intervals.
#' @param name.lowerCI Column name for the upper limit of confidence interval (applied if \code{show.ci==TRUE}).
#' @param name.upperCI Column name for the upper limit of confidence interval (applied if \code{show.ci==TRUE}).
#' @param type.est Plotting type of estimates. If \code{"auto"}, the type is determined automatically chosen by 
#' the class of the variable defined by \code{name.var2}. If not, choose manually from \code{"line"}, \code{"bar"}, or \code{"point"}.
#' @param type.ci Plotting type of confidence intervals. If \code{"auto"}, the type is determined automatically chosen by 
#' the class of the variable defined by \code{name.var2}. If not, choose manually from \code{"errorbar"} (Error Bars) or \code{"ribbon"} (Appropriate if \code{type.est == "line"}).
#' @param name.facet.x Column name for the predictor that controls facets horizontally (optional). 
#' @param name.facet.y Column name for the predictor that controls facets vertically (optional). 
#' @param name.color Column name for the predictor that controls line colors (optional).
#' @param name.linetype COlumn name for the predictor that controls line types (optional).
#' @param name.fill Column name for the predictor that controls fill colors (optional).
#' @param name.shape Column name for the predictor that controls point shapes (optional).
#' @param line.width The width of line in line plot.
#' @param barpoint.gapwidth The gap between bars/points if plot is clustered.
#' @param point.size The size of points in point plot.
#' @param errorbar.width The width of errorbar.
#' @param ribbon.alpha The transparency of ribbon plot.
#' @param label.color Optional label if \code{name.color==TRUE}. If \code{"name.color"}, the value assigned to \code{name.color} will be used for name in legend.
#' @param label.linetype Optional label if \code{name.linetype==TRUE}. If \code{"name.linetype"}, the value assigned to \code{name.linetype} will be used for name in legend.
#' @param label.fill Optional label if \code{name.fill==TRUE}. If \code{"name.fill"}, the value assigned to \code{name.fill} will be used for name in legend.
#' @param label.shape Optional label if \code{name.shape==TRUE}. If \code{"name.shape"}, the value assigned to \code{name.shape} will be used for name in legend.
#' @param titletxt Title Text.
#'
#' @return \code{ggplot} object (axis and theme settings can be added later).
#'
#' @export

plot_interact <- function(interactprof,
                          name.var2 = NULL,
                          label.var2 = "name.var2",
                          label.y = "auto",
                          hline0 = TRUE, 
                          addprof = NULL,
                          name.est = "Mean",
                          show.ci = TRUE,
                          name.lowerCI = "lowerCI",
                          name.upperCI = "upperCI",
                          type.est = "auto",
                          type.ci = "auto",
                          name.facet.x = NULL,
                          name.facet.y = NULL,
                          name.color = NULL,
                          name.linetype = NULL,
                          name.fill = NULL,
                          name.shape = NULL,
                          line.width = 0.7,
                          barpoint.gapwidth = 0.5,
                          point.size = 2,
                          errorbar.width = 0.5,
                          ribbon.alpha = 0.5,
                          label.color = "name.color",
                          label.linetype = "name.linetype",
                          label.fill = "name.fill",
                          label.shape = "name.shape",
                          titletxt = NULL
                          ){
  
  # Plotting Data
  if (class(interactprof)[1]=="simuinteract") {
    predprof <- cbind(interactprof$predsum,interactprof$moveprof)
  } else if ("data.frame" %in% class(interactprof)) { 
    predprof <- interactprof
  } else if ("matrix" %in% class(interactprof)) {
    predprof <- as.data.frame(interactprof)
  } else {
    stop("Incompatible class of interactprof")
  }
  
  # Second Variable
  if (is.null(name.var2)) {
    if (class(interactprof)[1]=="simuinteract") {
      name.var2 <- interactprof[["var2"]]
    } else {
      stop("name.var2 must be given.")
    }
  } 
  
  # X label
  if (!is.null(label.var2)) {
    if (label.var2 == "name.var2") {
      label.var2 <- name.var2
    }
  }
  
  # Y label
  if (!is.null(label.y)) {
    if (label.y == "auto") {
      if (class(interactprof)[1]=="simuinteract") {
        label.y <- paste0("Conditional Effect of \n", interactprof$var1.label, " on ", interactprof$y.label) 
      } else {
        label.y <- "Conditional Effect"
      }
    }
  }
  
  p <- plot_simu(predprof = predprof,
                 name.x = name.var2,
                 label.x = label.var2,
                 label.y = label.y,
                 addprof = addprof,
                 name.est = name.est,
                 show.ci = show.ci,
                 name.lowerCI = name.lowerCI,
                 name.upperCI = name.upperCI,
                 type.est = type.est,
                 type.ci = type.ci,
                 name.facet.x = name.facet.x,
                 name.facet.y = name.facet.y,
                 name.color = name.color,
                 name.linetype = name.linetype,
                 name.fill = name.fill,
                 name.shape = name.shape,
                 line.width = line.width,
                 barpoint.gapwidth = barpoint.gapwidth,
                 point.size = point.size,
                 errorbar.width = errorbar.width,
                 ribbon.alpha = ribbon.alpha,
                 label.color = label.color,
                 label.linetype = label.linetype,
                 label.fill = label.fill,
                 label.shape = label.shape,
                 titletxt = titletxt)
  
  if (hline0 == TRUE) {
    p <- p + geom_hline(aes(yintercept=0), linetype=2) 
  }
  
  
  return(p)
}

