# Following variables are global
globalVariables(c("Mean","Median","SE","lowCI","upCI"))

#' Inverse Logit Transformation
#' 
#' @description Computes the inverse logit transformation. 
#' The copy of \code{\link[faraway]{ilogit}()} function.
#' 
#' @return \code{exp(x)/(1+exp(x))} 
#' 
#' @param x a numeric vector
#' 
#' @export
ilogit <- function (x) 
{
  if (any(omit <- is.na(x))) {
    lv <- x
    lv[omit] <- NA
    if (any(!omit)) 
      lv[!omit] <- Recall(x[!omit])
    return(lv)
  }
  exp(x)/(1 + exp(x))
}

#' Exporting Simulated Predictions
#'
#' @description Generate prediction through Monte Carlo simulation. 
#'
#' @param m Single model object or the list of model objects.
#' @param profile The case profile  (\code{matrix} or \code{data.frame}). Column names are variables in the model and prediction is made for each row. Any missing variable in the profile is substituted by the rule defined by \code{at}. The default is \code{NULL} (see \code{at}). 
#' @param type If \code{response} (default), the output is transformed to the response format 
#' @param show.ci Show confidence interval (boulean). The default is \code{TRUE}.
#' @param level.ci The level used for confidence interval (numeric: 0-1). The default is \code{0.95}.
#' @param est.vcov Variance-covariance matrix. The default is \code{"robust"} (see \code{robust.type}). If \code{NULL}, use the standard variance covariance matrix stored the model. You can also directly set the variance-covariance matrix created by \code{\link[stats]{vcov}()} or \code{\link[sandwich]{vcovHC}()}.
#' @param robust.type The type of robust standard error (applied only when \code{est.vcov=="robust"}).
#' @param iterate.num The number of iteration in simulation.
#' @param iterate.seed The seed value for random number generator.
#'
#' @return A list of: 
#' \itemize{
#'  \item{\code{predsum}}{Summary Predictions}
#'  \item{\code{profile}}{Profile Used for Predictions}
#'  \item{\code{predres}}{Raw Predictions}
#' }
#'
#' @importFrom MASS mvrnorm
#' @importFrom sandwich vcovHC
#' @importFrom stats model.matrix
#' @importFrom stats vcov
#'
#' @export

simu_pred <- function(m,
                      profile = NULL,
                      type = "response",
                      at = "means",
                      show.ci = TRUE,
                      level.ci = 0.95,
                      est.vcov = "robust",
                      robust.type = "HC1",
                      iterate.num = 1000,
                      iterate.seed = 578
                      ){
  
  # Starting Profile if NULL
  if (is.null(profile)) profile <- data.frame(ididid=1)
  if (class(m)[1]=="lm") m$formula <- m$terms
  if (is.null(type)) type <- "asis"
  
  # Missing Variables
  if (colnames(profile)[1]=="ididid") {
    missvars <- all.vars(m$formula)[-1]
  } else {
    missvars <- all.vars(m$formula)[-1][which(!(all.vars(m$formula)[-1] %in% colnames(profile)))]
  }
  
  # Add missing variables to profile
  for (vn in missvars){ profile[,vn] <- 99999 }
  # Adjusting dependent variable 
  profile[, all.vars(m$formula)[1]] <- 0
  # Transforming profile matrix
  profile <- model.matrix(m$terms, profile)
  # Updating missing variable
  missvars <- names(profile)[which(profile[1,]==99999)]
  
  # Autofilling Missing Profile
  if (length(missvars)>0) {
    if (at=="means") {
      for (vn in missvars){
        if (class(m$model[,vn])=="numeric" & length(unique(m$model[,vn]))>2){
          profile[,vn] <- mean(m$model[,vn], na.rm=TRUE)
        } else {
          profile[,vn] <- names(rev(sort(table(m$model[,vn]))))[1]
        }
      }
    } else if (at=="medians") {
      for (vn in missvars){
        if (class(m$model[,vn])=="numeric" & length(unique(m$model[,vn]))>2){
          profile[,vn] <- median(m$model[,vn], na.rm=TRUE)
        } else {
          profile[,vn] <- names(rev(sort(table(m$model[,vn]))))[1]
        }
      }
    } else if (is.null(at)) {
      stop("Missing variable(s) in profile.")
    } else {
      stop("Invalid 'at' parameter.")
    }
  }
  
  # Coefficients
  coef_est <- m$coefficients
  # Variance-Covariance Matrix
  if (est.vcov=="robust") {
    vcov_est <- vcovHC(m, type=robust.type)
  } else if (class(est.vcov)=="matrix") {
    vcov_est <- est.vcov
  } else if (is.null(est.vcov)) {
    vcov_est <- vcov(m)
  }
  # Drawing Beta Values
  ndraws <- iterate.num
  set.seed(iterate.seed)
  betadraw <- mvrnorm(ndraws, coef_est, vcov_est)
  
  # Prediction
  predsum<-matrix(NA,nrow=nrow(profile),ncol=5)
  colnames(predsum)<-c("Mean","Median","SE","lowCI","upCI")
  predres <- list()
  for(i in 1:nrow(profile)){
    xfix <- as.vector(profile[i,])
    pred <- betadraw %*% xfix
    if (type == "response") {
      if (class(m)[1]=="lm") {
        pred <- pred
      } else if (class(m)[1]=="glm") {
        if(m$family$family=="gaussian"){
          pred <- pred
        } else if(m$family$family=="binomial" & m$family$link=="logit") {
          pred <- ilogit(pred)
        } else if (m$family$family=="binomial" & m$family$link=="probit") {
          pred <- pnorm(pred)
        } else if (m$family$family=="poisson") {
          pred <- exp(pred)
        } else {
          stop("Incompatible model object for 'response' type.")
        }
      } else if (class(m)[1]=="negbin") {
        pred <- exp(pred)
      } else {
        stop("Incompatible model object for 'response' type.")
      }
    }
    predres[[i]] <- predstore <- pred
    meanpred <- mean(predstore)
    medianpred <- median(predstore)
    sdpred <- sd(predstore)
    cipred <- quantile(predstore, probs=c(1-level.ci,level.ci))
    predsum[i,] <- c(meanpred, medianpred, sdpred, cipred)
  }
  predsum <- as.data.frame(predsum)
  
  # Summary
  res <- list(predsum, profile, predres, missvars)
  names(res) <- c("predsum","profile","predres","missvars")
  return(res)
  
}
