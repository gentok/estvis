#' Export Matrix (Data.Frame) of Coefficients and Confidence Interval
#'
#' @param m A single model object.
#' @param level Confidence interval pecent (0-1).
#' @param vcov. Alternative variance covariance matrix.
#' @param sims Number of iterations for simulation (integer). If \code{NULL}, the coefficients and confidence intervals are extracted analytically.
#'
#' @importFrom lmtest coeftest
#' @importFrom lmtest coefci
#'
#' @export
matrix_coefci <- function(m,
                          level = 0.95,
                          vcov. = NULL,
                          sims = NULL
                          ) {

  tb <- cbind(coeftest(m)[,1], coefci(m, level = level, vcov. = vcov.))

  if (is.null(sims)==FALSE) {
    if((class(m)[1] %in% c("lm", "glm", "polr"))==TRUE){
      s <- sim(m, n.sims=sims, vcov.=vcov.)
      dst <- (1-level)/2
      tb <- cbind(apply(s@coef,2,mean),t(apply(s@coef,2,quantile,probs=c(dst,1-dst))))
    } else {
      warning("Simulation method cannot be used. Analytic method used.")
    }
  }

  colnames(tb) <- c('CF', 'lower', 'upper')
  tb <- data.frame(tb)
  return(tb)

}
