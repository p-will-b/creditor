#' @title Converts a vector of numeric ratings produced by rating_to_numeric function
#' to a character vector
#'
#' @description This function takes a vector of numeric credit ratings and
#' returns a vector of their character equivalents. The package currently supports
#' long term credit ratings from Moody's, S&P, Fitch, Kroll, &
#' DBRS/Morningstar and defaults to return the S/F/K rating values for character equivalents.
#' All mappings are stored in creditor:::cr_imp. Short term
#' ratings are assigned the floor rounded value of their long term equivalents.
#'
#' @return Returns a vector of numeric credit rating equivalents.
#' @param numeric_rating A numeric vector of bond credit ratings
#' @param agency_convention Defaults to "sfk" (e.g. BBB+), but can be "m" (e.g. Baa1)
#' @export
#' @examples
#'
#' # convert bond credit ratings to numeric values
#' x <- c(1, 2, 4)
#' rating_to_char(x)
#'

rating_to_char <- function(numeric_rating,
                           agency_convention = "sfk") {

  stopifnot("numeric_rating must be numeric" = is.numeric(numeric_rating))
  stopifnot("agency_convention must be one of sfk or m" = agency_convention %in% c("sfk", "m"))

  rtg_dict <- switch(
    agency_convention,
    sfk = creditor:::cr_imp[cr_imp$agencies %in% "sfk", ],
    m = creditor:::cr_imp[cr_imp$agencies %in% "m", ]
  )

  rtg_dict$char_value[match(numeric_rating, rtg_dict$numeric_value)]
}
