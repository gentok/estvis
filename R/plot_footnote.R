#' Adding footnote to the \code{ggplot} object
#'
#' @description This function adds footnote to the \code{ggplot} object and returns \code{gtable} object.
#'
#' @param p \code{ggplot} object.
#' @param note The content of footnote (character). Line break is allowed by using \code{\\n}.
#' @param fontsize The size of font. If \code{NULL} (default), the size is set to the the font size in \code{text} setting of \code{p} - 1.
#' @param fontcol The color of the font. The default is \code{"black"}.
#' @param align The alignment of the footnote text. Either \code{"right"} or \code{"left"}.
#' @param distance.from.side The horizontal distance of notes from the edge of graph space by the proportion of graph width (numeric: 0-1). The default is \code{0.05}. The distance is measured from the side specified in \code{align}.
#' @param distance.from.bottom The vertical distance of notes from the bottom of graph bottom by the proportion of bottom graph margin height (numeric: 0-1). The default is \code{0.75}.
#' @param bottom.expand.rate The expansion rate of the bottom margin of the graph to incorporate footnote (numeric). The value of \code{1} indicates no expansion. If \code{NULL} (default), it is set to the number of lines in \code{note} + 1.
#' @param show.plot Print the plot at the end of function (boulean). The default is \code{TRUE}.
#'
#' @return \code{gtable} object. It is impossible to add aditional ggplot elements to this object. Plot can be viewed by using either \code{\link[grid]{grid.draw}()} or \code{\link[graphics]{plot}()} function.
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
#'
#' ## Add Footnote
#' p_wfoot <- plot_footnote(p,"Votes for Ross Perot are eliminated from analysis.")
#' p_wfoot
#'
#' @importFrom ggplot2 ggplotGrob
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom gtable gtable_add_grob
#' @importFrom graphics plot
#'
#' @export
plot_footnote <- function(p,
                         note,
                         fontsize = NULL,
                         fontcol = "black",
                         align = "right",
                         distance.from.side = 0.05,
                         distance.from.bottom = 0.75,
                         bottom.expand.rate = NULL,
                         show.plot = TRUE) {

                           # Assign p to g
                           g <- p

                           # Default Bottom Expand Rate
                           if (is.null(bottom.expand.rate)==TRUE) {
                             bottom.expand.rate <- lengths(regmatches(note, gregexpr("\n", note))) + 2
                           }

                           # Expand Bottom Plot Margin
                           g$theme$plot.margin[3] <- g$theme$plot.margin[3]*bottom.expand.rate

                           # Set default font size
                           if (is.null(fontsize)==TRUE){
                             fontsize = g$theme$text$size - 1
                           }

                           # Reverse Distance from Side Measure if Alginment is Right
                           if (align=="right") {
                             distance.from.side <- 1 - distance.from.side
                           }

                           # Convert the object to gtable object
                           g <- ggplotGrob(g)

                           # Footnote location
                           vloc <- g$layout[which(g$layout$name == "background"), "b"]
                           rloc <- g$layout[which(g$layout$name == "background"), "r"]

                           # Prepare Footnote Grob
                           foot = textGrob(note, x = distance.from.side, y = distance.from.bottom,
                                           just = c(align, "top"), gp = gpar(fontsize = fontsize, col =  fontcol))
                           labs.foot = gTree("LabsFoot", children = gList(foot))

                           # Add Footnote to the Plot
                           g <- gtable_add_grob(g, labs.foot, t = vloc, l = 1, r = rloc)

                           # Display New Plot
                           if(show.plot == TRUE){
                             plot(g)
                           }

                           return(g)
                         }
