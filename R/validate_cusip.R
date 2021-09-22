#' @title Checks for CUSIP validity.
#'
#' @description Using the CUSIP Global services methodology, this function checks
#' the validity of CUSIPs. The check relies on the Modulus 10 Double Add
#' Double algorithm. It does not query to check the assignment of any identifier
#' only its compositional validity. This is useful for checking hand or OCR
#' generated input. All non-9 digit inputs will return as FALSE (i.e. invalid
#' cusip).
#'
#' @return Returns a logical vector of CUSIP validity
#' @param cusips A character vector of presumed CUSIP identifiers
#' @export
#' @examples
#'
#' # check vector of CUSIPs for validity
#' x <- c("037833100", "0378331O0")
#' validate_cusip(x)
#'

validate_cusip <- function(cusips){
  stopifnot(is.character(cusips))
  idx <- is.na(cusips) | nchar(cusips) != 9L | !.Internal(match(.Internal(substr(x = cusips, start = 9L, stop = 9L)), 0L:9L, nomatch = 0L, incomparables = NULL)) > 0L
  c1 <- .Internal(toupper(.Internal(unlist(strsplit(.Internal(substr(x = cusips[!idx], start = 1L, stop =  8L)), split = "", fixed = T), use.names = F, recursive = F))))
  c1[c1 == "*"] <- 36L; c1[c1 == "@"] <- 37L ; c1[c1 == "#"] <- 38L
  idx1 <- .Internal(match(c1, LETTERS, nomatch = 0L, incomparables = NULL)) > 0L
  c1[idx1] <- match(c1[idx1], LETTERS) + 9L
  c1 <- as.character(as.integer(c1) * 1L:2L)
  L <- .Internal(split(as.integer(.Internal(unlist(strsplit(c1, split = "", fixed = T), use.names = F, recursive = F))), as.factor(rep(cumsum(seq_along(c1) %% 8L == 1L), nchar(c1)))))
  oL <- integer(length = length(L))
  for(i in seq_along(L)) oL[i] <- (10L - (sum(L[[i]]) %% 10L)) %% 10L
  lO <- logical(length = length(cusips))
  lO[!idx] <- as.integer(.Internal(x = substr(cusips[!idx], start = 9L, stop = 9L))) == oL
  lO[idx] <- F
  lO
}
