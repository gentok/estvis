# Following variables are global
globalVariables(c("Mean","Median","SE","lowerCI","upperCI"))

#' Exporting Simulated Predictions
#'
#' @description Generate prediction through Monte Carlo simulation based on  approximation. 
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
#' ## Estimate Logistic Regression
#' fm <- formula(voteBush ~ dem + rep +
#'                 clintondis + bushdis +
#'                 persfinance + natlecon)
#' m <- glm(fm, data = vote92,
#'          family = binomial("logit"))
#'
#' ## Comparing Partisans
#' 
#' # Profile
#' prof1 <- data.frame(dem=c(1,0,0),rep=c(0,0,1))
#' 
#' # Prediction (Missing Variables are Fixed at Mean/Mode)
#' predprof1 <- simu_pred(m, prof1, y.label = "Bush Vote")
#' summary(predprof1)
#' 
#' # Additional Profile to Give Labels
#' addprof1 <- data.frame(pid=c("Democrat","Independent","Republican"))
#' addprof1$pid <- factor(addprof1$pid, levels=unique(addprof1$pid))
#' 
#' # Plot 
#' plot_simu(predprof1, name.x="pid", addprof=addprof1, label.x = "Party ID")
#' # Change it to Point Graph
#' plot_simu(predprof1, name.x="pid", addprof=addprof1, 
#'           label.x = "Party ID", type.est = "point")
#' 
#' ## Comparing Effects of Ideological Distance by Party ID
#' 
#' # Profile
#' prof2 <- data.frame(dem=rep(rep(c(1,0,0),each=50),2),
#'                     rep=rep(rep(c(0,0,1),each=50),2),
#'                     bushdis=c(rep(seq(0,4,length=50),3), 
#'                               rep(0.5,150)),
#'                     clintondis=c(rep(0.5,150), 
#'                                  rep(seq(0,4,length=50),3)))
#' 
#' # Prediction (Missing Variables are Fixed at Mean/Mode)
#' predprof2 <- simu_pred(m, prof2, y.label = "Bush Vote")
#' summary(predprof2)
#' 
#' # Additional Profile to Give Labels
#' addprof2 <- data.frame(pid=rep(c("Democrat","Independent","Republican"),each=50),
#'                        dis=rep(seq(0,4,length=50),6),
#'                        labdis=rep(c("Ideological Distance from Bush",
#'                                     "Ideological Distance from Clinton"),each=150))
#' addprof2$pid <- factor(addprof2$pid, levels=rev(unique(addprof2$pid)))
#' 
#' # Plot 
#' plot_simu(predprof2, name.x="dis", addprof=addprof2,
#'           name.facet.x = "labdis", name.linetype="pid",
#'           label.x = NULL, label.linetype="Party ID")
#' # Change Color of CIs
#' plot_simu(predprof2, name.x="dis", addprof=addprof2,
#'           name.facet.x = "labdis", name.linetype="pid", name.fill="pid",
#'           label.x = NULL, label.linetype="Party ID")
#'
#' @param m Single model object.
#' @param profile The case profile  (\code{matrix} or \code{data.frame}). Column names are variables in the model and prediction is made for each row. Any missing variable in the profile is substituted by the rule defined by \code{at}. The default is \code{NULL} (all values are set using \code{at} option). 
#' @param y.label The label of the dependent variable (optional, character).
#' @param at Values to be fixed if not specified in \code{profile}. Choose from \code{"means"} (default), \code{"medians"}, or \code{NULL} (Stop if any variable is missing in \code{profile})
#' @param type If \code{"response"} (default), the output is transformed to the response format. If \code{"asis"}, transformation will not occur.
#' @param show.ci Show confidence interval (boulean). The default is \code{TRUE}.
#' @param level.ci The level used for confidence interval (numeric: 0-1). The default is \code{0.95}.
#' @param vcov.est Variance-covariance matrix to draw coefficents. 
#' If \code{"robust"}, use robust variance-covariance matrix (also see \code{robust.type}).
#' If \code{"cluster"}, use cluster robust variance-covariance matrix (also see \code{cluster.var}).
#' If \code{"boot"}, draw beta by non-parametric bootstrap method using \code{\link[car]{Boot}} function.
#' If \code{"rawbeta"}, use pre-simulated beta given in \code{rawbeta} argument.
#' If \code{NULL} (default), use the standard variance covariance matrix stored the model. You can also directly set the variance-covariance matrix created by \code{\link[stats]{vcov}()} or \code{\link[sandwich]{vcovHC}()}.
#' @param robust.type The type of leverage adjustment passed to \code{\link[sandwich]{vcovHC}} (applied only when \code{vcov.est=="robust"}).
#' @param cluster.var A \code{vector}, \code{matrix}, or \code{data.frame} of cluster variables, where each column is a separate variable. Alternatively, a formula specifying the cluster variables to be used (see Details in \code{\link[multiwayvcov]{cluster.vcov}}. Applied only when \code{vcov.est=="cluster"}.)
#' @param iterate.num The number of iteration in simulation.
#' @param iterate.seed The seed value for random number generator used for the draws from multivariate normal distribution.
#' @param rawbeta The matrix of pre-simulated beta. Columns are variables, raws are simulated cases. Used only when \code{vcov.est=="rawbeta"}.
#' @param dropbeta If not \code{NULL}, beta of specified locations (numeric vector) are not used for prediction (rarely used).
#' @param ... Additional arguments passed to \code{\link[sandwich]{vcovHC}}, \code{\link[multiwayvcov]{cluster.vcov}}, or \code{\link[car]{Boot}} depending on the value of \code{vcov.est}. 
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
#' }
#'
#' @importFrom MASS mvrnorm
#' @importFrom sandwich vcovHC
#' @importFrom stats model.matrix
#' @importFrom stats vcov
#' @importFrom stats pnorm 
#' @importFrom stats plogis
#' @importFrom stats sd
#' @importFrom stats model.matrix
#' @importFrom stats median
#' @importFrom stats model.frame
#' @importFrom car Boot
#' @importFrom multiwayvcov cluster.vcov
#'
#' @export

simu_pred <- function(m,
                      profile = NULL,
                      y.label = "auto",
                      at = "means",
                      type = "response",
                      show.ci = TRUE,
                      level.ci = 0.95,
                      vcov.est = NULL,
                      robust.type = "HC1",
                      cluster.var = NULL,
                      iterate.num = 1000,
                      iterate.seed = 578,
                      rawbeta = NULL,
                      dropbeta = NULL,
                      ...
                      ){
  
  # Starting Profile if NULL
  if (is.null(profile)) profile <- data.frame(ididid=1)
  if (class(m)[1]=="lm") m$formula <- m$terms
  if (is.null(type)) type <- "asis"
  
  # Extracting DV Name
  if (y.label=="auto") {
    y.label <- all.vars(m$formula)[1]
  } else if (is.null(y.label)) {
    warning("y.label is set automatically.")
    y.label <- all.vars(m$formula)[1]
  }
  
  # Missing Variables
  if (colnames(profile)[1]=="ididid") {
    missvars <- all.vars(m$formula)[-1]
  } else {
    missvars <- all.vars(m$formula)[-1][which(!(all.vars(m$formula)[-1] %in% colnames(profile)))]
  }
  
  if (length(missvars)>0){
    # Add missing variables to profile
    for (i in 1:length(missvars)){ profile[,missvars[i]] <- NA }
  }  
  # Adjusting dependent variable 
  profile[, all.vars(m$formula)[1]] <- 0
  # Transforming to model.frame
  profile <- model.frame(m$terms, profile, na.action=NULL)
  # Updating missing variable
  missvars <- colnames(profile)[which(is.na(profile[1,]))]
  # Updating existing variable
  existvars <- colnames(profile)[which(!is.na(profile[1,]))][-1]
  
  # Integer to Numeric
  for (i in 1:ncol(m$model)) {
    if (class(m$model[,i])=="integer") {
      m$model[,i] <- as.numeric(m$model[,i])
    }
  }
  
  # Adjusting Profile Format
  profile_temp <- m$model[seq(1,nrow(profile),1),]
  if (length(existvars)>0) {
    for (i in 1:length(existvars)) {
      vn <- existvars[i]
      profile_temp[,vn][seq(1,nrow(profile),1)] <- profile[,vn][seq(1,nrow(profile),1)]  
    }
  }
  profile <- profile_temp
  
  # Autofilling Missing Profile
  if (length(missvars)>0) {
    if (at=="means") {
      for (i in 1:length(missvars)){
        vn <- missvars[i]
        if (class(m$model[,vn])=="numeric" & length(unique(m$model[,vn]))>2){
          profile[,vn] <- mean(m$model[,vn], na.rm=TRUE)
        } else {
          profile[,vn] <- names(rev(sort(table(m$model[,vn]))))[1]
        }
      }
    } else if (at=="medians") {
      for (i in 1:length(missvars)){
        vn <- missvars[i]
        if (class(m$model[,vn])=="numeric" & length(unique(m$model[,vn]))>2){
          profile[,vn] <- median(m$model[,vn], na.rm=TRUE)
        } else {
          profile[,vn] <- names(rev(sort(table(m$model[,vn]))))[1]
        }
      }
    } else if (is.numeric(at)) {
      for (i in 1:length(missvars)){
        vn <- missvars[i]
        profile[,vn] <- at
      }
    } else if (is.null(at)) {
      stop("Missing variable(s) in profile.")
    } else {
      stop("Invalid 'at' parameter.")
    }
  }

  # Transform to Model Matrix
  profile_end <- profile
  profile_temp <- as.data.frame(model.matrix(m$terms, rbind(profile,m$model)))
  profile <- profile_temp[seq(1,nrow(profile),1),]
  
  if (is.null(vcov.est)) vcov.est <- "asis"
  
  if (vcov.est == "boot") {
    ncores <- ifelse(is.na(detectCores()),1,detectCores())
    set.seed(iterate.seed)
    s <- Boot(m, R = iterate.num, ncores=ncores, ...)
    betadraw <- s$t
  } else if (vcov.est == "rawbeta") {
    if (is.null(rawbeta)) {
      stop("rawbeta must be given.")
    } else {
      betadraw <- rawbeta
    }
  } else {
    # Coefficients
    coef_est <- m$coefficients
    # Variance-Covariance Matrix
    if (vcov.est=="robust") {
      vcov_est <- vcovHC(m, type=robust.type, ...)
    } else if (vcov.est=="cluster") {
      vcov_est <- cluster.vcov(m, cluster = cluster.var, ...)
    } else if (class(vcov.est)=="matrix") {
      vcov_est <- vcov.est
    } else if (vcov.est=="asis") {
      vcov_est <- vcov(m)
    }
    # Drawing Beta Values
    ndraws <- iterate.num
    set.seed(iterate.seed)
    betadraw <- mvrnorm(ndraws, coef_est, vcov_est)
  }
  
  
  
  # Dropping Specified Beta Values
  if (!is.null(dropbeta)) {
    for(i in 1:length(dropbeta)) {
      betadraw[,dropbeta[i]] <- 0
    }
  }
  
  # Determinig Family
  if (class(m)[1] == "lm") {
    method.family <- "gaussian"
  } else if ("glm" %in% class(m)) {
    if(m$family$family=="gaussian"){
      method.family <- "gaussian"
    } else if(m$family$family=="binomial" & m$family$link=="logit") {
      method.family <- "logit"
    } else if (m$family$family=="binomial" & m$family$link=="probit") {
      method.family <- "probit"
    } else if (m$family$family=="poisson") {
      method.family <- "poisson"
    } else if (grep("Negative Binomial",m$family$family)==1) {
      method.family <- "negbin"
    } else {
      warning("Unknown method.family type.")
      method.family <- "unknown glm"
    }
  } else {
    warning("Unknown method.family type.")
    method.family <- "unknown"
  }
  
  
  # Prediction
  predsum<-matrix(NA,nrow=nrow(profile),ncol=5)
  colnames(predsum)<-c("Mean","Median","SE","lowerCI","upperCI")
  predres <- list()
  for(i in 1:nrow(profile)){
    xfix <- as.numeric(profile[i,])
    pred <- betadraw %*% xfix
    if (type == "response") {
      if (class(m)[1]=="lm") {
        pred <- pred
      } else if ("glm" %in% class(m)) {
        if(m$family$family=="gaussian"){
          pred <- pred
        } else if(m$family$family=="binomial" & m$family$link=="logit") {
          pred <- plogis(pred)
        } else if (m$family$family=="binomial" & m$family$link=="probit") {
          pred <- pnorm(pred)
        } else if (m$family$family=="poisson") {
          pred <- exp(pred)
        } else if (grep("Negative Binomial",m$family$family)==1) {
          pred <- exp(pred)
        } else {
          stop("Incompatible model object for 'response' type.")
        }
      } else {
        stop("Incompatible model object for 'response' type.")
      }
    }
    predres[[i]] <- predstore <- pred
    if (any(is.nan(predstore))) { 
      stop("NaN appeared in prediction. Think about normalizing values.")    
    }
    meanpred <- mean(predstore)
    medianpred <- median(predstore)
    sdpred <- sd(predstore)
    cipred <- quantile(predstore, probs=c((1-level.ci)/2,level.ci + (1-level.ci)/2))
    predsum[i,] <- c(meanpred, medianpred, sdpred, cipred)
  }
  predsum <- as.data.frame(predsum)
  
  # Summary
  res <- list(predsum, profile_end[,-1], predres, m$formula, y.label, method.family, type)
  names(res) <- c("predsum","profile","predres", "formula", "y.label", "family", "type")
  class(res) <- c("simupred",class(res))
  return(res)
  
}

#' Summary Class for \code{simupred} Object
#' 
#' @description Summarizing \code{simupred} object
#' 
#' @return Summary output
#' 
#' @param object \code{simupred} object
#' @param digits Number of digits to show when summarizing data.
#' @param print.rows Max number of profile rows to be printed.
#' @param ... Additional arguments.
#' 
#' @export

summary.simupred <- function(object, ..., digits=3, print.rows=15) {
  cat(paste0("\n Simulated Prediction of ", object$y.label, 
             " (Type = ", object$type,"; Family = ", object$family,")"))
  cat("\n\n Formula: \n")
  print(object$formula)
  cat("\n Summary Predictions: \n")
  o1 <- round(object$predsum,digits)
  for(i in 1:ncol(object$profile)) {
    if (is.numeric(object$profile[,i])) {
      object$profile[,i] <- round(object$profile[,i],digits)
    }
  }
  if (nrow(o1)>print.rows) {
    print(cbind(o1,object$profile)[1:print.rows,])
    cat(paste0("\n There are ", nrow(o1), " rows in total. Omitted after ", print.rows, " rows."))
  } else {
    print(cbind(o1,object$profile))
  }
  
} 


#' Plotting Simulated Outcome
#' 
#' @description Plotting Simulated Outcome 
#' 
#' @return \code{ggplot} object of simulated outcome plot. 
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
#' ## Estimate Logistic Regression
#' fm <- formula(voteBush ~ dem + rep +
#'                 clintondis + bushdis +
#'                 persfinance + natlecon)
#' m <- glm(fm, data = vote92,
#'          family = binomial("logit"))
#'
#' ## Comparing Partisans
#' 
#' # Profile
#' prof1 <- data.frame(dem=c(1,0,0),rep=c(0,0,1))
#' 
#' # Prediction (Missing Variables are Fixed at Mean/Mode)
#' predprof1 <- simu_pred(m, prof1, y.label = "Bush Vote")
#' summary(predprof1)
#' 
#' # Additional Profile to Give Labels
#' addprof1 <- data.frame(pid=c("Democrat","Independent","Republican"))
#' addprof1$pid <- factor(addprof1$pid, levels=unique(addprof1$pid))
#' 
#' # Plot 
#' plot_simu(predprof1, name.x="pid", addprof=addprof1, label.x = "Party ID")
#' # Change it to Point Graph
#' plot_simu(predprof1, name.x="pid", addprof=addprof1, 
#'           label.x = "Party ID", type.est = "point")
#' 
#' ## Comparing Effects of Ideological Distance by Party ID
#' 
#' # Profile
#' prof2 <- data.frame(dem=rep(rep(c(1,0,0),each=50),2),
#'                     rep=rep(rep(c(0,0,1),each=50),2),
#'                     bushdis=c(rep(seq(0,4,length=50),3), 
#'                               rep(0.5,150)),
#'                     clintondis=c(rep(0.5,150), 
#'                                  rep(seq(0,4,length=50),3)))
#' 
#' # Prediction (Missing Variables are Fixed at Mean/Mode)
#' predprof2 <- simu_pred(m, prof2, y.label = "Bush Vote")
#' summary(predprof2)
#' 
#' # Additional Profile to Give Labels
#' addprof2 <- data.frame(pid=rep(c("Democrat","Independent","Republican"),each=50),
#'                        dis=rep(seq(0,4,length=50),6),
#'                        labdis=rep(c("Ideological Distance from Bush",
#'                                     "Ideological Distance from Clinton"),each=150))
#' addprof2$pid <- factor(addprof2$pid, levels=rev(unique(addprof2$pid)))
#' 
#' # Plot 
#' plot_simu(predprof2, name.x="dis", addprof=addprof2,
#'           name.facet.x = "labdis", name.linetype="pid",
#'           label.x = NULL, label.linetype="Party ID")
#' # Change Color of CIs
#' plot_simu(predprof2, name.x="dis", addprof=addprof2,
#'           name.facet.x = "labdis", name.linetype="pid", name.fill="pid",
#'           label.x = NULL, label.linetype="Party ID")
#'
#' @param predprof Predictions in either \code{simupred} (exported from \code{\link{simu_pred}} 
#' function), \code{data.frame} or \code{matrix} class. If in \code{data.frame} or \code{matrix},
#' it should have rows as predcion cases and columns for mean/median prediction, 
#' lower and upper confidence intervals (if \code{show.ci==TRUE}), and values for 
#' predictors that varies across profiles.
#' @param name.x Column name for the predictor to be on the x axis.
#' @param addprof Optional \code{data.frame} that adds or replaces values of predictor. 
#' Must have the same number of cases as \code{predprof}. If 
#' variable name is found in \code{predprof}, values are replaced. If not, additional variable 
#' is added to \code{predprof}. Added variables can be used in \code{name.x}, 
#' \code{name.color}, \code{name.linetype}, \code{name.fill}, and \code{name.shape}.
#' Define variable as \code{factor} if you want to control order of appearance in legend.
#' @param name.est Column name for mean/median prediction estiamte.
#' @param show.ci If \code{TRUE} (default), plot confidence intervals.
#' @param name.lowerCI Column name for the upper limit of confidence interval (applied if \code{show.ci==TRUE}).
#' @param name.upperCI Column name for the upper limit of confidence interval (applied if \code{show.ci==TRUE}).
#' @param type.est Plotting type of estimates. If \code{"auto"}, the type is determined automatically chosen by 
#' the class of the variable defined by \code{name.x}. If not, choose manually from \code{"line"}, \code{"bar"}, or \code{"point"}.
#' @param type.ci Plotting type of confidence intervals. If \code{"auto"}, the type is determined automatically chosen by 
#' the class of the variable defined by \code{name.x}. If not, choose manually from \code{"errorbar"} (Error Bars) or \code{"ribbon"} (Appropriate if \code{type.est == "line"}).
#' @param name.facet.x Column name for the predictor that controls facets horizontally (optional). 
#' @param name.facet.y Column name for the predictor that controls facets vertically (optional). 
#' @param name.color Column name for the predictor that controls line colors (optional).
#' @param name.linetype Column name for the predictor that controls line types (optional).
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
#' @param label.x X axis label. If \code{"name.x"} (default), use the variable name defined in \code{name.x}.
#' @param label.y Y axis label. If \code{"auto"} (default) and \code{predprof} being 
#' \code{simupred} object, the label is automatically determined.
#' @param titletxt Title Text.
#' 
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' 
#' @export

plot_simu <- function (predprof,
                       name.x,
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
                       label.x = "name.x",
                       label.y = "auto",
                       titletxt = NULL) 
{
  # Plot Theme
  plotsimu.theme <- estvis.theme() + 
    theme(legend.title = element_text(),
          strip.text = element_text(size=11, face="bold"))
  
  # Prediction Data
  if (class(predprof)[1]=="simupred") {
    d <- cbind(predprof$predsum,predprof$profile)
  } else if ("data.frame" %in% class(predprof)) { 
    d <- predprof
  } else if ("matrix" %in% class(predprof)) {
    d <- as.data.frame(predprof)
  } else {
    stop("Incompatible class of predprof")
  }
  
  # Replacing Labels
  if (!is.null(addprof)) {
    if ("data.frame" %in% class(addprof)) {
      for(i in 1:ncol(addprof)) {
        d[,colnames(addprof)[i]] <- addprof[,i]
      }
    } else {
      stop("The class of addprof must be data.frame")
    }
  }
  if (!is.null(name.color) & is.numeric(d[,name.color])) {
    d[,name.color] <- as.character(d[,name.color])
  }
  if (!is.null(name.linetype) & is.numeric(d[,name.linetype])) {
    d[,name.linetype] <- as.character(d[,name.linetype])
  }
  if (!is.null(name.fill) & is.numeric(d[,name.fill])) {
    d[,name.fill] <- as.character(d[,name.fill])
  }
  if (!is.null(name.shape) & is.numeric(d[,name.shape])) {
    d[,name.shape] <- as.character(d[,name.shape])
  }
  
  # Check for duplicated missing/inconsistent label issue
  if (!is.null(name.color) & !is.null(label.color)) {
    if (label.color != "name.color") {
      if (!is.null(name.linetype)) {
        if (name.color==name.linetype) {
          if (!is.null(label.linetype)) {
            if (label.color != label.linetype) {
              label.linetype <- label.color
              warning("Since name.color == name.linetype, label.color copied to label.linetype.")
            }
          }
        }
      }
      if (!is.null(name.fill)) {
        if (name.color==name.fill) {
          if (!is.null(label.fill)) {
            if (label.color != label.fill) {
              label.fill <- label.color
              warning("Since name.color == name.fill, label.color copied to label.fill.")
            }
          }
        }
      }
      if (!is.null(name.shape)) {
        if (name.color==name.shape) {
          if (!is.null(label.shape)) {
            if (label.color != label.shape) {
              label.shape <- label.color
              warning("Since name.color == name.shape, label.color copied to label.shape.")
            }
          }
        }
      }
    }
  }
  if (!is.null(name.linetype) & !is.null(label.linetype)) {
    if (label.linetype != "name.linetype") {
      if (!is.null(name.color)) {
        if (name.linetype==name.color) {
          if (!is.null(label.color)) {
            if (label.color == "name.color") {
              label.color <- label.linetype
              warning("Since name.linetype == name.color, label.linetype copied to label.color.")
            }
          }
        }
      }
      if (!is.null(name.fill)) {
        if (name.linetype==name.fill) {
          if (!is.null(label.fill)) {
            if (label.linetype != label.fill) {
              label.fill <- label.linetype
              warning("Since name.linetype == name.fill, label.linetype copied to label.fill.")
            }
          }
        }
      }
      if (!is.null(name.shape)) {
        if (name.linetype==name.shape) {
          if (!is.null(label.linetype)) {
            if (label.linetype != label.shape) {
              label.shape <- label.linetype
              warning("Since name.linetype == name.shape, label.linetype copied to label.shape.")
            }
          }
        }
      }
    }
  }
  if (!is.null(name.fill) & !is.null(label.fill)) {
    if (label.fill != "name.fill") {
      if (!is.null(name.color)) {
        if (name.fill==name.color) {
          if (!is.null(label.color)) {
            if (label.color == "name.color") {
              label.color <- label.fill
              warning("Since name.fill == name.color, label.fill copied to label.color.")
            }
          }
        }
      }
      if (!is.null(name.linetype)) {
        if (name.fill==name.linetype) {
          if (!is.null(label.linetype)) {
            if (label.linetype == "name.linetype") {
              label.linetype <- label.fill
              warning("Since name.fill == name.linetype, label.fill copied to label.linetype.")
            }
          }
        }
      }
      if (!is.null(name.shape)) {
        if (name.fill==name.shape) {
          if (!is.null(label.shape)) {
            if (label.fill != label.shape) {
              label.shape <- label.fill
              warning("Since name.fill == name.shape, label.fill copied to label.shape.")
            }
          }
        }
      }
    }
  }
  if (!is.null(name.shape) & !is.null(label.shape)) {
    if (label.shape != "name.shape") {
      if (!is.null(name.color)) {
        if (name.shape==name.color) {
          if (!is.null(label.color)) {
            if (label.color == "name.color") {
              label.color <- label.shape
              warning("Since name.shape == name.color, label.shape copied to label.color.")
            }
          }
        }
      }
      if (!is.null(name.linetype)) {
        if (name.shape==name.linetype) {
          if (!is.null(label.linetype)) {
            if (label.linetype == "name.linetype") {
              label.linetype <- label.shape
              warning("Since name.shape == name.linetype, label.shape copied to label.linetype.")
            }
          }
        }
      }
      if (!is.null(name.fill)) {
        if (name.shape==name.fill) {
          if (!is.null(label.fill)) {
            if (label.fill == "name.fill") {
              label.fill <- label.shape
              warning("Since name.shape == name.fill, label.shape copied to label.fill.")
            }
          }
        }
      }
    }
  }
  
  
  # Transform Labels
  if (!is.null(label.color) & !is.null(name.color)) {
    if (label.color != "name.color") {
      colnames(d)[which(colnames(d)==name.color)] <- label.color
      nameold.color <- name.color
      name.color <- paste0("`", label.color, "`")
    }
  }
  if (!is.null(label.linetype) & !is.null(name.linetype)) {
    if (label.linetype != "name.linetype") {
      colnames(d)[which(colnames(d)==name.linetype)] <- label.linetype
      nameold.linetype <- name.linetype
      name.linetype <- paste0("`", label.linetype, "`")
    }
  }
  if (!is.null(label.fill) & !is.null(name.fill)) {
    if (label.fill != "name.fill") {
      colnames(d)[which(colnames(d)==name.fill)] <- label.fill
      name.fill <- paste0("`", label.fill, "`")
    }
  }
  if (!is.null(label.shape) & !is.null(name.shape)) {
    if (label.shape != "name.shape") {
      colnames(d)[which(colnames(d)==name.shape)] <- label.shape
      name.shape <- paste0("`", label.shape, "`")
    }
  }
  
    
  # Initiate Plot
  p <- ggplot(d, aes_string(x=name.x,y=name.est))
  
  # Automatic Estimate Plotting Type Determination
  if (type.est == "auto") {
    if (class(d[,name.x])=="numeric") {
      type.est <- "line"
    } else if (class(d[,name.x]) %in% c("factor","character")) {
      type.est <- "bar"
    } else {
      "If type.est == 'auto', the class of predprof[,name.x] must be one of 'numeric', 'factor', or 'character'. "
    }
  }
  
  # Automatic Confidence Interval Type Determination
  if (show.ci == TRUE) {
    if (type.ci == "auto") {
      if (class(d[,name.x])=="numeric") {
        type.ci <- "ribbon"
      } else if (class(d[,name.x]) %in% c("factor","character")) {
        type.ci <- "errorbar"
      } else {
        stop("If type.ci == 'auto', the class of predprof[,name.x] must be one of 'numeric', 'factor', or 'character'. ")
      }
    }
  }
  
  # Plotting Confidence Interval (Ribbon)
  if (show.ci == TRUE) {
    if (type.ci == "ribbon") {
      fix.fill.color <- FALSE
      if (!is.null(name.linetype)) {
        if (!is.null(name.fill)) {
          if (name.fill != name.linetype) {
            nameold.fill <- name.fill
            name.fill <- name.linetype
            if (nameold.fill != nameold.linetype) {
              warning("For type.ci == ribbon, values of name.linetype copied to name.fill.")
            }
          }
        } else if (is.null(name.fill)) {
          name.fill <- name.linetype
          fix.fill.color <- TRUE
        }
      } else if (!is.null(name.color)) {
        if (!is.null(name.fill)) {
          if (name.fill != name.linetype) {
            nameold.fill <- name.fill
            name.fill <- name.color
            if (nameold.fill != nameold.color) {
              warning("For type.ci == ribbon, values of name.color copied to name.fill.")
            }
          }
        } else if (is.null(name.fill)) {
          name.fill <- name.color
          fix.fill.color <- TRUE
        }
      } else if (!is.null(name.fill)) {
        name.linetype <- name.fill
        warning("For type.ci == ribbon, values of name.fill copied to name.linetype.")
      }
      if (type.est != "line") {
        stop("For type.ci == 'ribbon', type.est must be 'line'.")
      }
      p <- p + geom_ribbon(aes_string(ymin=name.lowerCI, ymax=name.upperCI, 
                                      fill=name.fill), alpha=ribbon.alpha)
      if (fix.fill.color==TRUE) {
        l <- length(unique(d[,gsub("`","",name.fill)]))
        p <- p + scale_fill_manual(values=rep("grey20",l))
      }
    } 
  }
  
  # Plotting Estiamte
  if (type.est == "line") {
    if (!is.null(name.color) & !is.null(name.linetype)) {
      if (name.color!=name.linetype) {
        name.color <- NULL
        warning("For type.est=='line', name.color and name.linetype cannot differ. name.color is set to NULL.")
      }
    }
    p <- p + geom_line(aes_string(color=name.color,linetype=name.linetype), 
                           size=line.width)
  } else if (type.est == "bar") {
      p <- p + geom_bar(aes_string(fill=name.fill), 
                        stat="identity", position=position_dodge(width=barpoint.gapwidth))
  } else if (type.est == "point") {
      p <- p + geom_point(aes_string(shape=name.shape,color=name.color), size=point.size,
                          position=position_dodge(width=barpoint.gapwidth))
  } else {
      stop("Incompatible value of type.est")
  }
  
  # Plotting Confidence Interval (Errorbar)
  if (show.ci == TRUE) {
    if (type.est == "line" & type.ci == "errorbar") {
      p <- p + geom_errorbar(aes_string(ymin=name.lowerCI, ymax=name.upperCI,
                                        color=name.color),
                             width=errorbar.width) + 
        geom_point(aes_string(shape=name.shape, color=name.color), size=point.size)
    } else if (type.ci == "errorbar") {
      p <- p + geom_errorbar(aes_string(ymin=name.lowerCI, ymax=name.upperCI,
                                        color=name.color, linetype=name.linetype),
                             width=errorbar.width, size = line.width,
                             position=position_dodge(width=barpoint.gapwidth))
    } 
  }
  
  # Facetting
  if (!is.null(name.facet.x) & !is.null(name.facet.y)) {
    p <- p + facet_grid(d[,name.facet.y]~d[,name.facet.x], scales = "free_x") 
  } else if (!is.null(name.facet.x)) {
    p <- p + facet_grid(.~d[,name.facet.x], scales = "free_x") 
  } else if (!is.null(name.facet.y)) {
    p <- p + facet_grid(d[,name.facet.y]~., scales = "free_x")
  }
  
  # X label
  if (!is.null(label.x)) {
    if (label.x == "name.x") {
      label.x <- name.x
    }
  }
  
  # Y label
  if (!is.null(label.y)) {
    if (label.y == "auto") {
      if (class(predprof)[1]=="simupred") {
        if (predprof$type == "response") {
          if (predprof$family %in% c("logit","probit")) {
            label.y <- paste0("Predicted Probability \n of ", predprof$y.label) 
          } else {
            label.y <- paste0("Predicted Value \n of ", predprof$y.label)
          }
        } else {
          label.y <- paste0("Prediction \n of ", predprof$y.label)
        }
      } else {
        label.y <- "Predicted Value"
      }
    }
  }
  
  # Other Adjustments to Graph 
  p <- p + plotsimu.theme + 
    xlab(label.x) + ylab(label.y) + ggtitle(titletxt)
  
  # Return Graph
  return(p)
  
}


