#' cfm performance test
#' 
#' @param x tokenizedTexts
#' @export
#' @examples 
#' txt <- subset(inaugCorpus, Year > 1900)
#' data(SOTUCorpus, package = "quantedaData")
#' txt<-SOTUCorpus
#' microbenchmark::microbenchmark(
#'                              text2vecPfm(txt),
#'                              cfm3Pfm(txt),
#'                              cfmPfm(txt),
#'                              times = 1,
#'                              unit = 'relative') 
#' microbenchmark::microbenchmark(cfm3Pfm(toks),
#'                              text2vecPfm(tokens,vectorizer),
#'                              cfmPfm(toks),
#'                              times = 1,
#'                              unit = 'relative')                              
tokens <- txt %>% tolower %>% word_tokenizer
it <- itoken(tokens)
v <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)

toks <- tokenize(toLower(txt), removePunct = TRUE)
text2vecPfm <- function(tokens, vectorizer){
    #tokens <- txt %>% tolower %>% word_tokenizer
    #it <- itoken(tokens)
    #v <- create_vocabulary(it)
    #vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
    tcm <- create_tcm(itoken(tokens), vectorizer)
    return(tcm)
}

cfmPfm <- function(txt){
    #toks <- tokenize(toLower(txt), removePunct = TRUE)
  #toks_index <- lapply(toks, function(x, y) match(x, y), types)
    cfm <- cfm(toks, context = "window", count = "weighted", window = 3)
    return(cfm)
}

cfmPfmB <- function(txt){
    
    cfm <- cfm(toks, context = "window", count = "boolean", window = 3)
    return(cfm)
}
cfm3Pfm <- function(toks){
    #toks <- tokenize(toLower(txt), removePunct = TRUE)
    cfm3 <- cfm3(toks, context = "window", count = "weighted", ordered = TRUE, window = 3)
    return(cfm3)
}

cfm4Pfm <- function(toks){
    toks <- tokenize(toLower(txt), removePunct = TRUE)
    cfm4 <- cfm4(toks, context = "window", count = "weighted", window = 3)
    return(cfm3)
}


#|=====================================================================================================| 100%Unit: relative
#quantedaData
#expr      min       lq     mean   median       uq      max neval
#cfm3Pfm(toks) 8.273356 8.273356 8.273356 8.273356 8.273356 8.273356     1
#text2vecPfm(tokens, vectorizer) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000     1
#cfmPfm(toks) 8.133162 8.133162 8.133162 8.133162 8.133162 8.133162     1

#inaugCorpus
#|=====================================================================================================| 100%Unit: relative
#expr      min       lq     mean   median       uq      max neval
#cfm3Pfm(toks) 8.695910 8.695910 8.695910 8.695910 8.695910 8.695910     1
#text2vecPfm(tokens, vectorizer) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000     1
#cfmPfm(toks) 8.530173 8.530173 8.530173 8.530173 8.530173 8.530173     1