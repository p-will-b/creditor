#' @title Converts a vector of bond credit ratings to numeric values
#'
#' @description This function takes a vector of bond credit ratings and
#' returns a vector of their numeric equivalents. The package currently
#' supports long-term credit ratings from Moody's, S&P, Fitch, Kroll, &
#' DBRS/Morningstar. For computing portfolio or universe summary stats,
#' please see creditor::avg_rtg and creditor::median_rtg.
#'
#' @param credit_rating A character vector of bond credit ratings
#' @return Returns a vector of numeric credit rating equivalents
#' @examples
#'
#' # convert bond credit ratings to numeric values
#' x <- c("AAA, "Baa2", "A(low)")
#' rating_to_numeric(x)
#'

rating_to_numeric <- function(credit_rating) {

  if (class(credit_rating) != "character") {
    stop("rating_to_numeric() requires a character vector input")
  }

  # clean input data
  x <- gsub("—", "-", credit_rating)
  x <- gsub("\\s+", "", x)
  x <- trimws(x)

}


