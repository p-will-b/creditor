#' @title Converts a vector of bond credit ratings to numeric values
#'
#' @description This function takes a vector of bond credit ratings and
#' returns a vector of their numeric equivalents or, optionally, a statistical
#' summary of the translated values. The package currently supports
#' long-term credit ratings from Moody's, S&P, Fitch, Kroll, &
#' DBRS/Morningstar. All mappings are stored in creditor:::cr_imp.
#' By default, NA and unrecognized ratings are assumed
#' unrated and are assigned the highest numeric value (23).
#'
#' @return Returns a vector of numeric credit rating equivalents.
#' @param credit_rating A character vector of bond credit ratings
#' @param na_as_nr Assign missing or unrecognized ratings highest numeric value,
#' defaults to TRUE
#' @param summary_fun Function input to summarize vector
#' @param ... Additional arguments passed to summary_fun
#' @param summarize_numeric Return a numeric summary value.
#' Defaults to FALSE to return letter rating equivalent.
#' @export
#' @examples
#'
#' # convert bond credit ratings to numeric values
#' x <- c("AAA, "Baa2", "A(low)")
#' rating_to_numeric(x)
#'
#' # summarize ratings
#' x <- c("AAA", "Baa2", "A(low)")
#' rating_to_numeric(x, summary_fun = weighted.mean, w = c(0.15, 0.60, 0.25))

rating_to_numeric <- function(credit_rating,
                              na_as_nr = TRUE,
                              summary_fun = NULL,
                              ...,
                              summarize_numeric = FALSE) {

  stopifnot(is.character(credit_rating))

  # clean input data
  x <- gsub("—", "-", toupper(credit_rating), fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- trimws(x)

  if(!na_as_nr) {
    rtg_out <- creditor:::cr_imp$numeric_value[match(x, creditor:::cr_imp$char_value)]
    rtg_out[is.na(credit_rating)] <- NA_real_

  } else {

    rtg_out <- creditor:::cr_imp$numeric_value[match(x, creditor:::cr_imp$char_value)]
  }

  if(is.null(summary_fun)) {

    return(rtg_out)

  } else {

    .f <- match.fun(summary_fun)

    if(summarize_numeric) {

      return(.f(rtg_out, ...))

    } else {

      creditor:::cr_imp$char_value[match(round(.f(rtg_out, ...), 0), creditor:::cr_imp$numeric_value)]

    }
  }

}

