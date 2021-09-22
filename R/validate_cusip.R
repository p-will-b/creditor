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
  idx <- is.na(cusips) | nchar(cusips) != 9L | !grepl("[0-9]", substr(cusips, 9, 9))
  c0 <- .Internal(substr(as.character(cusips[!idx]), 1L, 8L))
  c1 <- toupper(unlist(strsplit(c0, split = "", fixed = T), use.names = F))
  c1[c1 == "*"] <- 36
  c1[c1 == "@"] <- 37
  c1[c1 == "#"] <- 38
  idx1 <- c1 %in% LETTERS
  c1[idx1] <- match(c1[idx1], LETTERS) + 9L
  idx2 <- seq_along(c1) %% 2L == 0L
  c1[idx2] <- as.numeric(c1[idx2]) * 2L
  c2 <- nchar(c1)
  names(c2) <- cumsum(seq_along(c2) %% 8L == 1L)
  c3 <- as.numeric(unlist(strsplit(c1, "", fixed = T), use.names = F))
  names(c3) <- rep(names(c2), c2)
  L <- .Internal(split(c3, factor(names(c3), levels = unique(names(c3)))))
  cusips[!idx] <- as.numeric(substr(cusips[!idx], 9, 9)) == vapply(L, \(x) (10 - (sum(x) %% 10)) %% 10, numeric(1), USE.NAMES = FALSE)
  cusips[idx] <- F
  as.logical(cusips)
}
