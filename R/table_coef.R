#' Plotting Coefficients
#'
#' @description Drawing the coefficient table from model object(s). The wrapper function of \code{texreg}.
#' 
#' @examples
#'  
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
#' vote92$w <- abs(rnorm(nrow(vote92)))
#'
#' ## Estimate Logistic Regression
#' fm <- formula(voteBush ~ dem + rep +
#'                clintondis + bushdis +
#'                persfinance + natlecon)
#' m <- glm(fm, data = na.omit(vote92[,c(all.vars(fm),"w","perotdis")]),
#'          family = binomial("logit"), weights=w)
#' 
#' ## Basic Table
#' table_coef(m)
#' 
#' ## Export in Tex
#' table_coef(m, format = "tex")
#' 
#' ## Show in Single Row
#' table_coef(m, single.row = TRUE)
#' 
#' ## Try Different Standard Errors
#' table_coef(list(m,m,m,m), 
#'            m.names = c("Standard","Robust(HC1)","Cluster Robust","Bootstrapped"),
#'            vcov.est = list(NULL,"robust","cluster","boot"),
#'            cluster.var = na.omit(vote92[,c(all.vars(fm),"w","perotdis")])$perotdis)
#' 
#' ## Estimate Model by Male and Female Subset
#' m_male <- glm(fm, data = vote92[vote92$female==0,],
#'               family = binomial("logit"))
#' m_female <- glm(fm, data = vote92[vote92$female==1,],
#'                 family = binomial("logit"))
#' 
#' ## Table with Two Models
#' table_coef(list(m_male,m_female))
#' 
#' ## Add Custom Variable Names
#' vn <- c("(Intercept)",
#'         "Democrat","Republican",
#'         "Ideological Distance from Clinton",
#'         "Ideological Distance from Bush",
#'         "Retrospective Personal Finance",
#'         "Retrospective National Economy")
#' table_coef(list(m_male, m_female), 
#'            m.names = c("Male", "Female"),
#'            custom.variable.names = vn)
#' 
#' ## Add Title (Only Tex/HTML)
#' table_coef(list(m_male, m_female), format="tex",
#'            m.names = c("Male", "Female"),
#'            caption = "Vote for Bush (1992)",
#'            custom.variable.names = vn)
#' 
#' ## Omit Some Variables
#' table_coef(list(m_male, m_female), 
#'            m.names = c("Male", "Female"),
#'            drop.variable.names = c("dem","natlecon"),
#'            custom.variable.names = vn[-c(2,7)])
#' 
#' ## Reorder Variables So That Intercept Comes at the end
#' table_coef(list(m_male, m_female), 
#'            m.names = c("Male", "Female"),
#'            drop.variable.names = c("dem","natlecon"),
#'            order.variable = c(5, seq(1,4,1)),
#'            custom.variable.names = vn[-c(2,7)])
#' 
#' 
#' ## Interaction Model
#' fm2 <- formula(voteBush ~ dem + rep +
#'                  clintondis + bushdis +
#'                  persfinance + natlecon + 
#'                  dem*clintondis)
#' m2 <- glm(fm2, data = vote92,
#'           family = binomial("logit"), weights=w)
#' table_coef(list(m,m2), 
#'            custom.variable.names = c(vn,"Democrat * Distance from Clinton"),
#'            order.variable=c(1,2,4,5,6,7,8,3))
#' 
#' @param m Single or the list of model object.
#' @param format The format for the table output (characater). The default is \code{"screen"} (The R Output. Cannot be saved.). Other options are \code{"tex"} (LaTex table), \code{"html"} (HTML table) and \code{"doc"} (HTML table saved as Microsof Word file).
#' @param file.name The name of the file to be saved (character). The default is \code{NULL} (The output not saved to file). If extension is provided, it will automatically overwrite \code{format}.
#' @param dir The directory of the file to be saved. Only applicable when file.name is not \code{NULL}.
#' @param m.names The set of names that identifies each element in \code{m}. Considered if \code{m} is a list of models. The length of the vector must correspond with the length of \code{m}. If \code{NULL} (default), each element \code{i} is temporarily named as \code{Model i}.
#' @param order.variable Order of coefficients in the plot(character/numeric vector). 
#' \code{"original"} (default) preserves the original order of the variables. 
#' Alternatively, you can specify the order of variables by numeric vector 
#' (applied after \code{drop.intercept} and \code{drop.variable.names} are applied, 
#' thus you don't need the names for dropped variables).
#' @param vcov.est Single or a list of alternative variance-covariance matrix. Each element must be one of raw variance-covariance matrix, \code{"robust"}, \code{"boot"}, or \code{NULL} (default).
#' If \code{"robust"}, robust standard error (also see \code{robust.type}).
#' If \code{"cluster"}, cluster robust standard error (also see \code{cluster.var}).
#' if \code{"boot"}, bootstrapped standard error calculated by \code{\link[car]{Boot}} function is used.
#' Ignored if \code{NULL} (default) or the \code{m} is not model object. Must have the same length as \code{m} if it is a list.
#' @param robust.type The type of robust standard error (applied only when \code{vcov.est=="robust"}).
#' @param cluster.var Single or a list of \code{vector}, \code{matrix}, or \code{data.frame} of cluster variables, where each column is a separate variable. Alternatively, a formula specifying the cluster variables to be used (see details in \code{\link[multiwayvcov]{cluster.vcov}}. Applied only when \code{vcov.est=="cluster"}.)
#' @param boot.sims Number of iterations if bootstrap is used. 
#' @param boot.seed Random number seed if bootstrap is used.
#' @param boot.ncores Number of cores to parallelize bootstrap. The default is \code{1}. Use \code{"auto"} to automatically detect number of cores in computer.
#' @param digits Rounding decimal points for values in the table.
#' @param drop.intercept Drop the intercept from the plot (boulean). If \code{FALSE} (default), intercept included in the plot.
#' @param drop.intercept.names The name(s) of intercept (character/character vector). Needed if \code{drop.intercept} is \code{TRUE}. This is used to identify and eliminate intercept variables from the output. Default value is \code{'(Intercept)'}.
#' @param drop.variable.names The name(s) of additional variables to drop (character/character vector) from the ouput. The default is \code{NULL}.
#' @param caption Table title caption (character). The default is to include no title caption. (Not applied if \code{format=="screen"})
#' @param custom.variable.names List of alternative variable names in the output (character vector). The default is \code{NULL}. This is applied AFTER \code{drop.intercept} and \code{drop.variable.names} are applied, thus you don't need the names for dropped variables.
#' @param footnote.psymbol Whether to add p-value symbols in the footnote.
#' @param custom.footnote Custom footnote (character). The default is \code{NULL}. If assigned, footnote are added to the plot by \code{\link{plot_footnote}} function, and the function exports \code{gtable} object. Note that \code{gtable} object is less customizable than \code{ggplot} object. If it is also the case that \code{footnote.gof == TRUE}, custom footnote will be added as the new line after the GOF footnote.
#' @param show.table Return the "screen" table at the end of function (boulean). Always returned if \code{format=="screen"}. The default is \code{TRUE}.
#' @param booktabs See \code{\link[texreg]{texreg}}.
#' @param dcolumn See \code{\link[texreg]{texreg}}.
#' @param use.packages See \code{\link[texreg]{texreg}}.
#' @param stars See \code{\link[texreg]{texreg}}.
#' @param symbol See \code{\link[texreg]{texreg}}.
#' @param caption.above See \code{\link[texreg]{texreg}}.
#' @param single.row See \code{\link[texreg]{texreg}}.
#' @param float.pos See \code{\link[texreg]{texreg}}.
#' @param fontsize See \code{\link[texreg]{texreg}}.
#' @param encoding_from If encoding conversion is required, what is the orignal encoding? (Only used when \code{format=="tex"}.)
#' @param encoding_to If encoding conversion is required, what is the encoding to be coverted to? (Only used when \code{format=="tex"}.) 
#' @param include.deviance See \code{\link[texreg]{extract}}.
#' @param include.smooth See \code{\link[texreg]{extract}}.
#' @param ... Other options in \code{\link[texreg]{texreg}} or \code{\link[texreg]{extract}} function.
#'
#' @importFrom lmtest coeftest
#' @importFrom texreg texreg
#' @importFrom texreg htmlreg
#' @importFrom texreg screenreg
#'
#' @return Regression table to be shown in console if \code{format=="screen"} or \code{show.table==TRUE}, \code{NULL} if else.
#'
#' @export
table_coef<-function(m,
                    format = "screen",
                    file.name = NULL,
                    dir = NULL,
                    m.names = NULL,
                    order.variable = "original",
                    vcov.est = NULL, # or "robust" or "cluster" or "boot" or raw vcov 
                    robust.type = "HC1",
                    cluster.var = NULL,
                    boot.sims = 300,
                    boot.seed = 578,
                    boot.ncores = 1,
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
                    float.pos = "ht!!",
                    fontsize = "footnotesize",
                    # If encoding conversion is required (only tex)
                    encoding_from = NULL,
                    encoding_to = NULL,
                    # Other settings for extract method
                    include.deviance = FALSE,
                    include.smooth = FALSE,
                    ... # Extra options for texreg function
                    )
{
  # Convert to a list if target is a model object
  if (class(m)[1] != "list") m <- list(m)

  # (alt.vcov) Convert to a list if target is not list
  if (class(vcov.est)[1]=="list") {
    alt.vcov <- vcov.est
  } else {
    alt.vcov <- list(vcov.est)
  }
  # (alt.vcov) If length==1, replicate by the length of m
  if (length(m)>1 & length(alt.vcov)==1) {
    tmp <- vcov.est
    if (is.null(vcov.est)) tmp <- NA
    if (class(tmp)=="matrix") {
      stop("m and vcov.est must have the same length if
           class(vcov.est[[1]])=='matrix'")
    } else {
      for (i in 1:length(m))  alt.vcov[[i]] <- tmp
    }
  }
  
  # (cluster.list) Convert to a list if target is not list
  if (class(cluster.var)[1]=="list") {
    cluster.list <- cluster.var
  } else {
    cluster.list <- list(cluster.var)
  }
  # (cluster.list) If length==1, replicate by the length of m
  if (length(alt.vcov)>1 & length(cluster.list)==1) {
    tmp <- cluster.list[[1]]
    if (is.null(tmp)) tmp <- NA
    for (i in 1:length(m)) cluster.list[[i]] <- tmp
  }
  
  # Check Length
  if (length(m)!=length(alt.vcov)) {
    stop("m and vcov.est must have the same length.")
  }
  if (length(m)!=length(cluster.list)) {
    stop("m and cluster.var must have the same length.")
  }
  
  
  # Set alternative Standard Errors and P-values
  if (is.null(vcov.est)==FALSE) {
    alt.se <- list()
    alt.pval <- list()
    # For Each Model
    for (i in 1:length(alt.vcov)){
      # Put NULL Back into vcov
      vcov_i <- alt.vcov[[i]]
      if (!is.null(vcov_i)) {
        if (!"matrix" %in% class(vcov_i)) {
          if (is.na(vcov_i)) vcov_i <- NULL
        }
      }
      # Put NULL Back into cluster
      cluster_i <- cluster.list[[i]]
      if (!is.null(cluster_i)) {
        if (class(cluster_i)=="formula") {
        } else if (is.na(cluster_i[1])) {
          cluster_i <- NULL
        }
      }
      # Estimate Coefficient and SE
      ct <- matrix_coeftest(m[[i]], vcov.est = vcov_i, 
                            robust.type = robust.type, 
                            cluster.var = cluster_i, 
                            boot.sims = boot.sims, 
                            boot.seed = boot.seed,
                            ncores = boot.ncores)
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
      footnote.text.tex <- paste("\\parbox{1.0\\linewidth}{\\vspace{2pt}%stars. \\\\", custom.footnote, "}")
      footnote.text.html <- paste("%stars <br> ", custom.footnote)
      footnote.text.screen <- paste("%stars", custom.footnote)
    }
  } else {
    footnote.text.tex <- footnote.text.html <- footnote.text.screen <- ""
    if (is.null(custom.footnote)==FALSE){
      footnote.text.tex <- paste("\\parbox{1.0\\linewidth}{\\vspace{2pt}", custom.footnote, "}")
      footnote.text.html <- footnote.text.screen <- custom.footnote
    }
  }
  
  # Override File Format if Extension is Already Provided
  if (!is.null(file.name)) {
    if(grepl("\\.tex$", file.name)) {
      file.name <- gsub("\\.tex$","", file.name)
      format <- "tex"
    }
    if(grepl("\\.html$", file.name)) {
      file.name <- gsub("\\.html$","", file.name)
      format <- "html"
    }
    if(grepl("\\.doc$", file.name)) {
      file.name <- gsub("\\.doc$","", file.name)
      format <- "doc"
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
  varnames <- unique(varnames) # Original Variable Names
  newvarnames <- as.character(varnames) # New Variable Names
  omitvarloc <- NULL # Location of Omitted variables
  # New Variable Names as "OMITTED" if omitted &
  # Assign custom variable names if assigned.
  if (!is.null(omitvarnames)) {
    omitvarloc <- which(varnames %in% omitvarnames)
    newvarnames[omitvarloc] <- "OMITTED"
    if (!is.null(custom.variable.names)) {
      newvarnames[-omitvarloc] <- custom.variable.names
    }
  } else {
    if (is.null(custom.variable.names)==FALSE) {
      newvarnames <- custom.variable.names
    }
  }
  
  ## Print Variable Conversion
  convres <- data.frame(Omitted = "KEPT",
                        Original = varnames,
                        stringsAsFactors = FALSE)
  if (length(omitvarloc)==0) {
    convres$Final <- newvarnames
  } else {
    convres$Final <- as.character("")
    convres$Final[-omitvarloc] <- as.character(newvarnames[-omitvarloc])
    convres$Omitted <- as.character(convres$Omitted)
    convres$Omitted[omitvarloc] <- "OMITTED"
  }
  cat("Variable Manipulations: \n  ")
  print(convres, row.names=FALSE)
  if (format!="screen") cat("\n")
  
  if (length(omitvarloc)>0) {
    mapcoefs <- as.list(newvarnames[-omitvarloc])
    names(mapcoefs) <- varnames[-omitvarloc]
  } else {
    mapcoefs <- as.list(newvarnames)
    names(mapcoefs) <- varnames
  }
  
  if (is.numeric(order.variable)) {
    if (length(order.variable)==length(mapcoefs)) {
      #mapcoefs <- mapcoefs[order(order.variable)]
      mapcoefs <- mapcoefs[order.variable]
    } else {
      warning("Length of order.variable and kept unique variable names differ.
           Order kept as original.")
    }
  }
  
  # Draw Table
  tb.screen <- screenreg(m,
                         # file = filepath,
                         override.se = alt.se,
                         override.pvalues = alt.pval,
                         booktabs=booktabs,
                         dcolumn=dcolumn,
                         use.packages=use.packages,
                         digits = digits,
                         custom.model.names = m.names,
                         custom.coef.map = mapcoefs,
                         # custom.coef.names = newvarnames,
                         # omit.coef="OMITTED",
                         caption = caption,
                         include.deviance=include.deviance,
                         include.smooth=include.smooth,
                         custom.note = footnote.text.screen,
                         caption.above = caption.above,
                         single.row = single.row,
                         fontsize = fontsize,
                         stars = stars,
                         symbol = symbol.screen, ...)
  
  if (format=="tex") {
    texreg(m,
           file = filepath,
           override.se = alt.se,
           override.pvalues = alt.pval,
           booktabs=booktabs,
           dcolumn=dcolumn,
           use.packages=use.packages,
           digits=digits,
           custom.model.names = m.names,
           custom.coef.map = mapcoefs,
           # custom.coef.names = newvarnames,
           # omit.coef="OMITTED",
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
    if (!is.null(filepath)) {
      if (symbol=="dagger"|(!is.null(encoding_from)&!is.null(encoding_to))) {
        tmp <- readLines(filepath)
        if (symbol=="dagger") {
          tmp <- sub("\\{dagger", "{\\\\\ dagger", tmp)
          tmp <- sub(" dagger", 'dagger',  tmp)
        }
        if (!is.null(encoding_from)&!is.null(encoding_to)) {
          tmp <- iconv(tmp, from=encoding_from, to=encoding_to)
          writeLines(tmp, filepath, useBytes=TRUE)
        } else {
          writeLines(tmp, filepath)
        }
      }
    }
  } else if (format %in% c("html","doc")) {
    if (is.null(caption)==TRUE) caption <- "No Title"
    htmlreg(m,
            file = filepath,
            override.se = alt.se,
            override.pvalues = alt.pval,
            booktabs=booktabs,
            dcolumn=dcolumn,
            use.packages=use.packages,
            digits=digits,
            custom.model.names = m.names,
            custom.coef.map = mapcoefs,
            # custom.coef.names = newvarnames,
            # omit.coef="OMITTED",
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
  } else if (format!="screen") {
    stop("Invalid format assigned.")
  }
  
  if (format == "screen") {
    return(tb.screen)
  } else if (show.table == TRUE) {
    return(tb.screen)
  }

}
