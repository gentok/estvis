#' Save Plots
#'
#' @description Save and export plots with the specified names. Use \code{\link[grDevices]{pdf}}, \code{\link[grDevices]{bmp}}, \code{\link[grDevices]{png}}, \code{\link[grDevices]{jpeg}} or \code{\link[grDevices]{tiff}} to save the plot ouput to the specified directory.
#'
#' @param p The \code{ggplot} object or \code{gtable} object.
#' @param w The width of the plot ouput. If \code{NULL} (default), \code{8} for \code{format = "pdf"}, \code{800} for \code{format \%in\% c("bmp", "png", "jpeg", "tiff")}.
#' @param h The height of the plot output. If \code{NULL} (default), \code{6} for \code{format = "pdf"}, \code{600} for \code{format \%in\% c("bmp", "png", "jpeg", "tiff")}.
#' @param res The resolution of the plot ouput. Used if \code{format \%in\% c("bmp", "png", "jpeg", "tiff")}. The default value is \code{120}.
#' @param file The (path and) name of the file (including extention). If \code{NULL} (default), the file is saved to the current working directory with the name of the object \code{p}.
#' @param dir The directory to save the plot. If \code{NULL} (default), the file is saved to the current working directory (or the path included in \code{file} argument). If both \code{dir} and \code{file} are specified, then the file is saved to the combined path of \code{dir} and \code{file}.
#' @param format The format of the saving file. It can be chose from \code{"pdf", "bmp", "png", "jpeg", "tiff")}. Default value is \code{"pdf"}.
#' @param show.plot Print the plot at the end of function (boulean). The default is \code{TRUE}.
#' @param ... Additional arguments passed to the graphic device function specified in \code{format}.
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
#' p <- plot_coef(m)
#' p
#'
#' ## Save Graph
#' plot_save(p)
#'
#' @importFrom grDevices pdf
#' @importFrom grDevices bmp
#' @importFrom grDevices png
#' @importFrom grDevices jpeg
#' @importFrom grDevices tiff
#' @importFrom grDevices graphics.off
#' @importFrom grDevices dev.off
#' @importFrom graphics plot
#'
#' @rdname plot_save
#' @export
plot_save <- function(p,
                      w = NULL,
                      h = NULL,
                      res = 120,
                      file = NULL,
                      dir = NULL,
                      format = "pdf",
                      show.plot = TRUE, ...
                      ){
                        # Shutdown all the graphic editors
                        graphics.off()

                        if (format=="pdf"){
                          if(is.null(w)==TRUE){
                            w = 8
                          }
                          if(is.null(h)==TRUE){
                            h = 6
                          }
                          if (is.null(file) == TRUE) {
                            pdf(paste(substitute(p),"pdf",sep="."),width=w,height=h, ...)
                            if (is.null(dir) == FALSE) {
                              pdf(paste(dir,"/",substitute(p),".pdf",sep=""),width=w,height=h, ...)
                            }
                          }  else {
                            pdf(file,width=w,height=h, ...)
                            if (is.null(dir) == FALSE) {
                              pdf(paste(dir, file, sep="/"),width=w,height=h, ...)
                            }
                          }
                        } else {
                          if(is.null(w)==TRUE){
                            w = 800
                          }
                          if(is.null(h)==TRUE){
                            h = 600
                          }
                          fn <- get(format)
                          if (is.null(file)==TRUE){
                            fn(paste(substitute(p),substitute(format),sep="."), width = w, height = h, res = res, ...)
                            if (is.null(dir) == FALSE) {
                              fn(paste(dir, "/", substitute(p), ".", substitute(format), sep=""), width = w, height = h, res = res, ...)
                            }
                          } else {
                            fn(file, width = w, height = h, res = res, ...)
                            if (is.null(dir) == FALSE) {
                              fn(paste(dir, file, sep="/"), width = w, height = h, res = res, ...)
                            }
                          }
                        }

                        plot(p)

                        # Shutdown all the graphic editors
                        graphics.off()

                        if(show.plot == TRUE){
                          plot(p)
                        }
                     }
#' @rdname plot_save
#' @export
pdf_save <- function(p, w = 8, h = 6, file = NULL, dir = NULL, ...) {
  if (is.null(file)==TRUE) {
    file <- paste(substitute(p),"pdf",sep=".")
  }
  plot_save(p = p, w = w, h = h, format = "pdf", file = file, dir = dir)
}
#' @rdname plot_save
#' @export
bmp_save <- function(p, w = 800, h = 600, res = 120, file = NULL, dir = NULL, ...) {
  if (is.null(file)==TRUE) {
    file <- paste(substitute(p),"bmp",sep=".")
  }
  plot_save(p = p, w = w, h = h, res = res, format = "bmp", file = file, dir = dir)
}
#' @rdname plot_save
#' @export
png_save <- function(p, w = 800, h = 600, res = 120, file = NULL, dir = NULL, ...) {
  if (is.null(file)==TRUE) {
    file <- paste(substitute(p),"png",sep=".")
  }
  plot_save(p = p, w = w, h = h, res = res, format = "png", file = file, dir = dir)
}
#' @rdname plot_save
#' @export
jpeg_save <- function(p, w = 800, h = 600, res = 120, file = NULL, dir = NULL, ...) {
  if (is.null(file)==TRUE) {
    file <- paste(substitute(p),"jpeg",sep=".")
  }
  plot_save(p = p, w = w, h = h, res = res, format = "jpeg", file = file, dir = dir)
}
#' @rdname plot_save
#' @export
tiff_save <- function(p, w = 800, h = 600, res = 120, file = NULL, dir = NULL, ...) {
  if (is.null(file)==TRUE) {
    file <- paste(substitute(p),"tiff",sep=".")
  }
  plot_save(p = p, w = w, h = h, res = res, format = "tiff", file = file, dir = dir)
}
