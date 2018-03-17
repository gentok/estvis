#' Plotting Coefficients
#'
#' @description Drawing the coefficient table from model object(s). The wrapper function of \code{texreg}.
#'
#' @param m Single or the list of model object.
#' @param format The format for the table output (characater). The default is \code{"tex"} (LaTex table). Other options are \code{"html"} (HTML table), \code{"doc"} (HTML table saved as Microsof Word file), and \code{"screen"} (The R Output. Cannot be saved.)
#' @param file.name The name of the file to be saved (character). The default is \code{NULL} (The output not saved to file). Extension is automatically added, so do not include it.
#' @param dir The directory of the file to be saved. Only applicable when file.name is not \code{NULL}.
#' @param m.names The set of names that identifies each element in \code{m}. Considered if \code{m} is a list of models. The length of the vector must correspond with the length of \code{m}. If \code{NULL} (default), each element \code{i} is temporarily named as \code{Model i}.
#' @param vcov.est Single or the list of the alternative variance-covariance matrix to be used for the model object \code{m}. Ignored if \code{NULL} (default) or the \code{m} is not model object. Must have the same length as \code{m} if it is a list.
#' @param digits Rounding decimal points for values in the table.
#' @param drop.intercept Drop the intercept from the plot (boulean). If \code{FALSE} (default), intercept included in the plot.
#' @param drop.intercept.names The name(s) of intercept (character/character vector). Needed if \code{drop.intercept} is \code{TRUE}. This is used to identify and eliminate intercept variables from the output. Default value is \code{'(Intercept)'}.
#' @param drop.variable.names The name(s) of additional variables to drop (character/character vector) from the ouput. The default is \code{NULL}.
#' @param caption Table title caption (character). The default is to include no title caption.
#' @param custom.variable.names List of alternative variable names in the output (character vector). The default is \code{NULL}. This is applied AFTER \code{drop.intercept} and \code{drop.variable.names} are applied, thus you don't need the names for dropped variables.
#' @param custom.footnote Custom footnote (character). The default is \code{NULL}. If assigned, footnote are added to the plot by \code{\link{plot_footnote}} function, and the function exports \code{gtable} object. Note that \code{gtable} object is less customizable than \code{ggplot} object. If it is also the case that \code{footnote.gof == TRUE}, custom footnote will be added as the new line after the GOF footnote.
#' @param show.table Print the table at the end of function (boulean). The default is \code{TRUE}.
#' @param ... Other options in \code{texreg} function.
#'
#' @importFrom lmtest coeftest
#' @importFrom texreg texreg
#' @importFrom texreg htmlreg
#' @importFrom texreg screenreg
#'
#' @return Output from \code{texreg} function.
#'
#' @export
table_coef<-function(m,
                    format = "tex",
                    file.name = NULL,
                    dir = NULL,
                    m.names = NULL,
                    vcov.est = NULL,
                    digits = 3,
                    drop.intercept=FALSE,
                    drop.intercept.names = "(Intercept)",
                    drop.variable.names = NULL,
                    caption = NULL,
                    custom.variable.names = NULL,
                    footnote.psymbol = TRUE,
                    custom.footnote = NULL,
                    show.table = TRUE,
                    # Other formattings for texreg
                    booktabs = TRUE,
                    dcolumn = FALSE,
                    use.packages = FALSE,
                    stars = c(0.001,0.01,0.05,0.1),
                    symbol = "dagger",
                    caption.above = TRUE,
                    single.row = FALSE,
                    float.pos = "h!!",
                    fontsize = "footnotesize",
                    # Other settings for extract method
                    include.deviance = FALSE,
                    include.smooth = FALSE,
                    ... # Extra options for texreg function
                    ){

                      # Convert to a list if target is a model object
                      if (class(m)[1] != "list"){
                        m <- list(m)
                        alt.vcov <- list(vcov.est)
                      }

                      # Set alternative Standard Errors
                      if (is.null(vcov.est)==FALSE) {
                        alt.se <- list()
                        alt.pval <- list()
                        for (i in 1:length(alt.vcov)){
                            ct <- coeftest(m[[i]], vcov.=alt.vcov[[i]])
                            alt.se[[i]] <- ct[, 2]
                            alt.pval[[i]] <- ct[, 4]
                        }
                      } else {
                        alt.se <- 0
                        alt.pval <- 0
                      }

                      # Symbol settings
                      symbol.tex <- symbol.html <- symbol.screen <- symbol
                      if (symbol=="dagger"){
                          symbol.tex <- "\\dagger"
                          symbol.html <- "&dagger;"
                          symbol.screen <- "+"
                      }

                      # Footnote settings
                      footnote.text.tex <- footnote.text.html <- footnote.text.screen <- NULL
                      if (footnote.psymbol==TRUE){
                        footnote.text.tex <- footnote.text.html <- footnote.text.screen <- "%stars"
                        if (is.null(custom.footnote)==FALSE){
                          footnote.text.tex <- paste("\\parbox{.4\\linewidth}{\\vspace{2pt}%stars. \\\\", custom.footnote, "}")
                          footnote.text.html <- paste("%stars <br> ", custom.footnote)
                          footnote.text.screen <- paste("%stars", custom.footnote)
                        }
                      } else {
                        footnote.text.tex <- footnote.text.html <- footnote.text.screen <- ""
                        if (is.null(custom.footnote)==FALSE){
                          footnote.text.tex <- paste("\\parbox{.4\\linewidth}{\\vspace{2pt}", custom.footnote, "}")
                          footnote.text.html <- footnote.text.screen <- custom.footnote
                        }
                      }

                      # File paths
                      if (is.null(file.name) == FALSE){
                        if (is.null(dir) == FALSE) {
                          filepath <- paste(dir, "/", file.name, ".", format, sep="")
                        } else {
                          filepath <- paste(file.name, ".", format, sep="")
                        }
                      } else {
                        filepath <- NULL
                      }

                      # Dropping variables
                      if (drop.intercept==TRUE) {
                        omitvarnames <- drop.intercept.names
                        if (is.null(drop.variable.names)==FALSE){
                          omitvarnames <- c(omitvarnames, drop.variable.names)
                        }
                      } else {
                        omitvarnames <- NULL
                        if (is.null(drop.variable.names)==FALSE){
                          omitvarnames <- drop.variable.names
                        }
                      }

                      ## Replace Variable Names
                      varnames <- character()
                      for (i in 1:length(m)){
                        varnames <- c(varnames, rownames(coeftest(m[[i]])))
                      }
                      varnames <- unique(varnames)
                      cat("Original Variable Names: \n  ")
                      cat(varnames)
                      cat("\n")

                      if (is.null(omitvarnames) == FALSE) {
                        omitvarloc <- numeric()
                        for (i in 1:length(omitvarnames)) {
                          omitvarloc <- c(omitvarloc, which(varnames==omitvarnames[i]))
                        }

                        newvarnames <- varnames
                        newvarnames[omitvarloc] <- "OMITTED"
                        if (is.null(custom.variable.names)==FALSE) {
                          newvarnames[-omitvarloc] <- custom.variable.names
                        }
                        cat("Final Variable Names: \n  ")
                        cat(newvarnames)
                        cat(paste("\nLength:", length(newvarnames[-omitvarloc])))
                      } else {
                        newvarnames <- NULL
                        if (is.null(custom.variable.names)==FALSE) {
                          newvarnames <- custom.variable.names
                          cat("Final Variable Names: \n  ")
                          cat(newvarnames)
                        }
                      }

                      # Draw Table
                      tb.screen <- screenreg(m,
                             file = filepath,
                             override.se = alt.se,
                             override.pvalues = alt.pval,
                             booktabs=booktabs,
                             dcolumn=dcolumn,
                             use.packages=use.packages,
                             digits = digits,
                             custom.model.names = m.names,
                             custom.coef.names = newvarnames,
                             omit.coef="OMITTED",
                             caption = caption,
                             include.deviance=include.deviance,
                             include.smooth=include.smooth,
                             custom.note = footnote.text.html,
                             caption.above = caption.above,
                             single.row = single.row,
                             fontsize = fontsize,
                             stars = stars,
                             symbol = symbol.screen, ...)
                      if (format=="tex") {
                        tb.tex <- texreg(m,
                               file = filepath,
                               override.se = alt.se,
                               override.pvalues = alt.pval,
                               booktabs=booktabs,
                               dcolumn=dcolumn,
                               use.packages=use.packages,
                               digits=digits,
                               custom.model.names = m.names,
                               custom.coef.names = newvarnames,
                               omit.coef="OMITTED",
                               caption = caption,
                               include.deviance=include.deviance,
                               include.smooth=include.smooth,
                               #reorder.coef = c(2:11,1),
                               custom.note = footnote.text.tex,
                               caption.above = caption.above,
                               single.row = single.row,
                               float.pos = float.pos,
                               fontsize = fontsize,
                               stars = stars,
                               symbol = symbol.tex, ...)
                        tb <- tb.tex
                      } else if (format %in% c("html","doc")) {
                        tb.html <- htmlreg(m,
                               file = filepath,
                               override.se = alt.se,
                               override.pvalues = alt.pval,
                               booktabs=booktabs,
                               dcolumn=dcolumn,
                               use.packages=use.packages,
                               digits=digits,
                               custom.model.names = m.names,
                               custom.coef.names = newvarnames,
                               omit.coef="OMITTED",
                               caption = caption,
                               include.deviance=include.deviance,
                               include.smooth=include.smooth,
                               #reorder.coef = c(2:11,1),
                               custom.note = footnote.text.html,
                               caption.above = caption.above,
                               single.row = single.row,
                               fontsize = fontsize,
                               stars = stars,
                               symbol = symbol.html, ...)
                        tb <- tb.html
                      } else if (format == "screen") {
                        tb <- tb.screen
                      } else {
                        stop("Invalid format assigned.")
                      }

                      if (show.table==TRUE){
                        print(tb.screen)
                      }

                      return(tb)
}
