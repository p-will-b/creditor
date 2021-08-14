#' @title Converts a vector of fractional bond prices to decimal.
#'
#' @description This function is designed to handle all flavors of
#' fixed income fractional price quotes and to return decimal equivalents
#' for use in your scripts. Requires a character vector input.
#'
#' @param bond_px A character vector of fractional bond prices, can include decimal prices too
#' @return Returns a vector of decimalized bond prices
#' @export
#' @examples
#'
#' # convert fractional bond prices to decimal prices
#' x <- c("$101-28", "101-2 1/8", "99+")
#' px_to_decimal(x)
#'

px_to_decimal <- function(bond_px) {

  stopifnot(is.character(bond_px))
  x <- gsub("$", "", bond_px, fixed = TRUE)
  split_px <- strsplit(x, "-", fixed = TRUE)
  dp <- sapply(split_px, `[`, 1)
  frac <- sapply(split_px, `[`, 2)

  # check first piece, if contains '+' add 1/64 to number for edge cases

  dp <- ifelse(grepl("+", dp, fixed = TRUE), as.character(as.numeric(gsub("+", "", dp, fixed = TRUE)) + 1/64), dp)

  # now handle fractional, +, NNN-nnn, simple digit, frac+decimal

  frac <- ifelse(grepl("/", frac, fixed = TRUE), gsub(" ", "/32 ", paste0(frac, "*1/32"), fixed = TRUE), frac)
  frac <- ifelse(grepl("\\+$", frac), gsub("\\+$", "/32 1/64", frac), frac)
  frac <- ifelse(nchar(frac) == 3 & !grepl("[^0-9]", frac), paste0(substr(frac, 1, 2), "/32 ", substr(frac, 3, 3), "/8*1/32"), frac)
  frac <- ifelse(!grepl("[^0-9]", frac), paste0(frac, "/32"), frac)
  frac <- sub(" ", "+", ifelse(grepl("\\d\\.\\d", frac), gsub(".", "/32 .", paste0(frac, "*1/32"), fixed = TRUE), frac), fixed = TRUE)
  pfrac <- parse(text = frac, keep.source = FALSE)

  # return value, first check for edge cases with only decimal & +, then if NA convert to numeric
  dp <- ifelse(grepl("NA", frac, fixed = TRUE), as.numeric(dp), as.numeric(dp) + as.numeric(lapply(pfrac, eval)))
  return(dp)
}
