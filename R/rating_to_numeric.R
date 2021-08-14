#' @title Converts a vector of bond credit ratings to numeric values
#'
#' @description This function takes a vector of bond credit ratings and
#' returns a vector of their numeric equivalents. The package currently
#' supports long-term credit ratings from Moody's, S&P, Fitch, Kroll, &
#' DBRS/Morningstar. For computing portfolio or universe summary stats,
#' please see creditor::avg_rtg and creditor::median_rtg. All mappings
#' are stored in creditor:::cr_imp. By default, NA and unrecognized ratings
#' are assumed unrated and are assigned the highest numeric value (23).
#'
#' @return Returns a vector of numeric credit rating equivalents.
#' @param credit_rating A character vector of bond credit ratings
#' @param na_as_nr Assign missing or unrecognized ratings highest numeric value,
#' defaults to TRUE
#' @export
#' @examples
#'
#' # convert bond credit ratings to numeric values
#' x <- c("AAA, "Baa2", "A(low)")
#' rating_to_numeric(x)
#'

rating_to_numeric <- function(credit_rating, na_as_nr = TRUE) {

  stopifnot(is.character(credit_rating))

  # clean input data
  x <- gsub("—", "-", toupper(credit_rating), fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- trimws(x)

  if(!na_as_nr) {
    rtg_out <- creditor:::cr_imp$numeric_value[match(x, creditor:::cr_imp$char_value)]
    rtg_out[is.na(credit_rating)] <- NA_character_
    return(rtg_out)

  } else {

    creditor:::cr_imp$numeric_value[match(x, creditor:::cr_imp$char_value)]
  }

}
