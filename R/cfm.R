#' @include dfm-classes.R

#' @title Virtual class "cfm" for a context-feature matrix
#' 
#' @description The cfm class of object is a special type of \link{dfm-class}.
#'   
#' @slot context the context definition
#' @slot window the size of the window, if \code{context = "window"}
#' @slot weights context weighting for distance from target feature, equal in length to \code{window}
#' @seealso \link{cfm}
#' @export
#' @import methods
#' @docType class
#' @name cfm-class
setClass("cfm",
         slots = c(context = "character", window = "integer", weights = "numeric"),
         # prototype = list(Dimnames = list(contexts = NULL, features = NULL)),
         contains = c("dfm", "dgCMatrix"))


#' create a context-feature co-occurrence matrix
#' 
#' Create a sparse context-feature co-occurrence matrix, measuring
#' co-occurrences of features within a user-defined context. The context can be
#' defined as a document or a window within a collection of documents, with an
#' optional vector of weights applied to the co-occurrence counts.
#' @param x character vector, corpus, or tokenized texts from which to generate 
#'   the context-feature co-occurrence matrix.
#' @param ... additional arguments passed to \link{tokenize}, which can include 
#'   for instance \code{ngrams} and \code{concatenator} for tokenizing 
#'   multi-token sequences
#' @author Kenneth Benoit (R) and Kohei Watanabe (C++)
#' @import Matrix
#' @export
cfm <- function(x, ...) {
    UseMethod("cfm")
}

#' @rdname cfm
#' @param context the context in which to consider term co-occurrence: 
#'   \code{"document"} for co-occurrence counts within document; \code{"window"}
#'   for co-occurrence within a defined window of words, which requires a 
#'   postive integer value for \code{window}
#' @param window positive integer value for the size of a window on either side 
#'   of the target feature, default is 5, meaning 5 words before and after the 
#'   target feature
#' @param weights a vector of weights applied to each distance from
#'   \code{1:window}, strictly decreasing and of the same length as
#'   \code{length(weights)}
#' @param span_sentence if \code{FALSE}, then word windows will not span
#'   sentences
#' @param tri if \code{TRUE} return only upper triangle (including diagonal)
#' @param verbose if \code{TRUE} print status messages to the console
#' @examples
#' # see http://bit.ly/29b2zOA
#' txt <- "A D A C E A D F E B A C E D"
#' cfm(txt, context = "window", window = 2)
#' 
#' txts <- c("a a b b c", "a c e", "e f g")
#' cfm(txts, context = "document")
#' 
#' txt <- c("The quick brown fox jumped over the lazy dog.",
#'          "The dog jumped and ate the fox.")
#' cfm(toks, context = "document")
#' cfm(toks, context = "window", window = 3)
#' cfm(toks, context = "window", window = 2)
#' @import data.table
#' @import Matrix
#' @export
cfm.tokenizedTexts <- function(x, context = c("document", "window"), window = 5L,
                               weights = rep(1, length(window)),
                               span_sentence = TRUE, tri = TRUE, ...) {
    context <- match.arg(context)
    feature <- V1 <- NULL  # to avoid no visible binding errors in CHECK
    # could add a warning if not roundly coerced to integer
    window <- as.integer(window)
    
    if (!span_sentence) 
        warning("spanSentence = FALSE not yet implemented")
    
    if (context == "document") {
        
    #     window <- max(lengths(x))
    # }        
        x <- tf(dfm(x, toLower = FALSE, verbose = FALSE), "boolean")
        # get co-occurrence counts
        result <- t(x) %*% x
        # remove 1 from the diagonal so that target features are not counted as their own context
        # Matrix::diag(result) <- ifelse(Matrix::diag(result) > 0, Matrix::diag(result) - 1, 0)
        # return triangular result only if tri == TRUE
        if (tri)
            result[lower.tri(result, diag = FALSE)] <- 0
        window = 0L
        weights = 1
        # make sure that zeros are sparse
        # result[result==0] <- 0

    } else if (context == "window") {
        
        # x <- ngrams(unlist(x), n = window, skip = 0:(window - 1)) #, ...)
        # x <- strsplit(x, "_")
        # # create a data.table of tokens
        # x <- data.table(do.call(rbind, x))
        # setnames(x, 1:2, c("context", "feature"))
        # x <- x[, paste(feature, collapse = " "), by = context]
        # # make into a character vector
        # contexts <- x[, V1]
        # names(contexts) <- x[, context]
        # rm(x)
        # result <- dfm(contexts, toLower = FALSE, verbose = FALSE)
        
        types <- unique(unlist(x, use.names = FALSE))
        n <- sum(lengths(x)) * window * 2
        y <- fcm_cpp(x, types, window, n)
        
        # if (verbose) message("Making sparseMatrix\n")
        result <- Matrix::sparseMatrix(i = y$target, 
                                       j = y$collocate, 
                                       x = 1L,
                                       dimnames = list(contexts = types, features = types))
    }

    # order the features alphabetically
    result <- result[order(rownames(result)), order(colnames(result))]
    result <- new("cfm", as(result, "dgCMatrix"), context = context, window = window, weights = weights)
    names(result@Dimnames) <- c("contexts", "features")
    result
}     



#' @rdname cfm
#' @export
cfm.corpus <- function(x, ...) {
    cfm(texts(x), ...)
}

#' @rdname cfm
#' @export
cfm.character <- function(x, ...) {
    cfm(tokenize(x), ...)
}


#' @rdname print.dfm
#' @export
setMethod("print", signature(x = "cfm"), 
          function(x, show.values = FALSE, show.settings = FALSE, show.summary = TRUE, nfeature = 20L, ...) {
              ndoc <- nfeature
              if (show.summary) {
                  cat("Context-feature matrix of: ",
                      format(ndoc(x), , big.mark = ","), " context",
                      ifelse(ndoc(x) > 1 | ndoc(x) == 0, "s, ", ", "),
                      format(nfeature(x), big.mark = ","), " feature",
                      ifelse(nfeature(x) > 1 | nfeature(x) == 0, "s", ""),
                      ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep = ""), ""),
                      ".\n", sep = "")
              }
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x) <= ndoc & ncol(x) <= nfeature)) {
                  Matrix::printSpMatrix2(x[1:min(ndoc, ndoc(x)), 1:min(nfeature, nfeature(x))], 
                                         col.names = TRUE, zero.print = 0, ...)
              }
          })

#' @rdname print.dfm
setMethod("show", signature(object = "cfm"), function(object) print(object))


# # C++ wrapper for cfm()
# # Author: Kohei Watanabe
# cfmCpp_tokenizedTexts <- function(x, context = c("document", "window"), window = 5L, verbose = FALSE) {
#     context <- match.arg(context)
#     if (context == 'document') {
#         window <- max(lengths(x))
#     }
#     types <- unique(unlist(x, use.names=FALSE))
#     n <- sum(lengths(x)) * window * 2
#     y <- fcm_cpp(x, types, window, n)
#     
#     if (verbose) message("Making sparseMatrix\n")
#     mx <- Matrix::sparseMatrix(i = y$target, 
#                                j = y$collocate, 
#                                x = 1L,
#                                dimnames = list(context = types, features = types))
#     
#     return(mx)
# }
# 

 







      

