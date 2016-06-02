

## internal function to perform unicode normalization
## called from other functions as quanteda:::unicodeNorm(x)
##
unicodeNorm <- function(x, type = c("nfc", "nfd", "nfkd", "nfkc_casefold")) {
    if (!is.character(x)) stop("input must be character")
    type <- match.arg(type)
     
    switch(type,
           nfc = stringi::stri_trans_nfc(x),
           nfd = stringi::stri_trans_nfd(x),
           nfkd = stringi::stri_trans_nfkd(x),
           nfkc = stringi::stri_trans_nfkc(x),
           nfkc_casefold = stringi::stri_trans_nfkc_casefold(x))
}