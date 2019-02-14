#' Export Matrix (Data.Frame) of Coefficients and Confidence Interval
#'
#' @param m A single model object.
#' @param level Confidence interval pecent (0-1).
#' @param vcov.est Alternative variance-covariance matrix. The value must be one of 
#' raw variance-covariance matrix, \code{"robust"}, \code{"boot"}, or \code{NULL} (default).
#' If \code{"robust"}, robust standard error is calculated according to \code{robust.type}.
#' If \code{"boot"}, bootstrapped standard error calculated by \code{\link[car]{Boot}} function is used.
#' Ignored if \code{NULL} (default) or the \code{m} is not model object. Must have the same length as \code{m} if it is a list.
#' @param robust.type The type of robust standard error (applied only when \code{vcov.est=="robust"}).
#' @param cluster.var A \code{vector}, \code{matrix}, or \code{data.frame} of cluster variables, where each column is a separate variable. Alternatively, a formula specifying the cluster variables to be used (see Details in \code{[multiwayvcov]{cluster}}. Applied only when \code{vcov.est=="cluster"}.)
#' @param boot.sims Number of iterations if bootstrap is used. 
#' @param boot.seed Random number seed if bootstrap method is used.
#' @param ncores Number of cores to be used in bootstrap. If \code{"auto"}, automatically detects number of cores in computer.
#' @param ... Additional arguments passed to \code{[sandwich]{vcovHC}}, \code{[multiwayvcov]{cluster}}, or \code{\link[car]{Boot}} depending on the value of \code{vcov.est}. 
#' 
#'
#' @importFrom lmtest coeftest
#' @importFrom lmtest coefci
#' @importFrom stats quantile
#' @importFrom stats vcov
#' @importFrom sandwich vcovHC
#' @importFrom car Boot
#' @importFrom car .carEnv
#' @importFrom parallel detectCores
#' @importFrom multiwayvcov cluster.vcov
#' @importFrom stats formula
#' @importFrom stats as.formula
#'
#' @export
matrix_coefci <- function(m,
                          level = 0.95,
                          vcov.est = NULL,
                          robust.type = "HC1",
                          cluster.var = NULL,
                          boot.sims = 500,
                          boot.seed = 578,
                          ncores = "auto",
                          ...
                          ) {
  
  if (is.null(vcov.est)) {
    tb <- cbind(coeftest(m)[,1], coefci(m, level = level, vcov. = vcov(m)))
  } else if (vcov.est=="robust") {
    tb <- cbind(coeftest(m)[,1], coefci(m, level = level, vcov. = vcovHC(m, type=robust.type, ...)))
  } else if (vcov.est=="cluster") {
    tb <- cbind(coeftest(m)[,1], coefci(m, level = level, vcov. = cluster.vcov(m, cluster=cluster.var, ...)))
  } else if (vcov.est=="boot") {
    if (ncores=="auto") ncores <- ifelse(is.na(detectCores()),1,detectCores())
    if (class(m$call$formula)=="name") {
      m$call$formula <- call("as.formula",deparse(formula(m)))
    }
    set.seed(boot.seed)
    s <- Boot(m, R = boot.sims, ncores=ncores, ...)
    dst <- (1-level)/2
    tb <- cbind(apply(s$t,2,mean),t(apply(s$t,2,quantile,probs=c(dst,1-dst))))
  } else if (class(vcov.est)=="matrix") {
    tb <- cbind(coeftest(m)[,1], coefci(m, level = level, vcov. = vcov.est))
  }

  colnames(tb) <- c('CF', 'lowerCI', 'upperCI')
  tb <- data.frame(tb)
  return(tb)

}

#' Export Matrix (Data.Frame) of Coefficients Test
#'
#' @param m A single model object.
#' @param vcov.est Alternative variance-covariance matrix. The value must be one of 
#' raw variance-covariance matrix, \code{"robust"}, \code{"boot"}, or \code{NULL} (default).
#' If \code{"robust"}, robust standard error is calculated according to \code{robust.type}.
#' If \code{"boot"}, bootstrapped standard error calculated by \code{\link[car]{Boot}} function is used.
#' Ignored if \code{NULL} (default) or the \code{m} is not model object. Must have the same length as \code{m} if it is a list.
#' @param robust.type The type of robust standard error (applied only when \code{vcov.est=="robust"}).
#' @param cluster.var A \code{vector}, \code{matrix}, or \code{data.frame} of cluster variables, where each column is a separate variable. Alternatively, a formula specifying the cluster variables to be used (see Details in \code{[multiwayvcov]{cluster}}. Applied only when \code{vcov.est=="cluster"}.)
#' @param boot.sims Number of iterations if bootstrap is used. 
#' @param boot.seed Random number seed if bootstrap method is used.
#' @param ncores Number of cores to be used in bootstrap. If \code{"auto"}, automatically detects number of cores in computer.
#' @param seed Random number seed.
#' @param ... Additional arguments passed to \code{[sandwich]{vcovHC}}, \code{[multiwayvcov]{cluster}}, or \code{\link[car]{Boot}} depending on the value of \code{vcov.est}. 
#' 
#'
#' @importFrom lmtest coeftest
#' @importFrom stats quantile
#' @importFrom stats vcov
#' @importFrom sandwich vcovHC
#' @importFrom car Boot
#' @importFrom car .carEnv
#' @importFrom parallel detectCores
#' @importFrom multiwayvcov cluster.vcov
#' 
#'
#' @export
matrix_coeftest <- function(m,
                          vcov.est = NULL,
                          robust.type = "HC1",
                          cluster.var = NULL,
                          boot.sims = 500,
                          boot.seed = 578,
                          ncores = "auto",
                          ...
) {
  
  if (is.null(vcov.est)) {
    tb <- coeftest(m)
  } else if (vcov.est=="robust") {
    tb <- coeftest(m, vcov.=vcovHC(m, type=robust.type, ...))
  } else if (vcov.est=="cluster") {
    tb <- coeftest(m, vcov.=cluster.vcov(m, cluster=cluster.var, ...))
  } else if (vcov.est=="boot") {
    if (ncores=="auto") ncores <- ifelse(is.na(detectCores()),1,detectCores())
    if (class(m$call$formula)=="name") {
      m$call$formula <- call("as.formula",deparse(formula(m)))
    }
    if ("weights" %in% names(m$call)) {
      wvec <- m$model$`(weights)`
      m$call$weights <- wvec
    }
    set.seed(boot.seed)
    s <- Boot(m, R = boot.sims, ncores=ncores, ...)
    tb <- coeftest(m, vcov.=vcov(s))
  } else if (class(vcov.est)=="matrix") {
    tb <- coeftest(m, vcov. = vcov.est)
  }
  
  return(tb)
  
}

