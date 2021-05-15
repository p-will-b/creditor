#' @title Converts a vector of fractional bond prices to decimal.
#'
#' @description This function is designed to handle all flavors of
#' fixed income fractional price quotes and to return decimal equivalents
#' for use in your scripts.
#'
#' @param frac_px A vector of fractional bond prices, can include decimal prices too
#' @return Returns a vector of decimalized bond prices
#' @examples
#'
#' # convert fractional bond prices to decimal prices
#' x <- c("$101-28", "101-2 1/8", "99+")
#' px_to_decimal(x)
#'

px_to_decimal <- function(bond_px) {

  # ensure input vector is character
  x <- as.character(bond_px)
  x <- gsub("\\$", "", x)

  # split into pieces
  split_px <- strsplit(x, "-")
  dp <- sapply(split_px, `[`, 1)
  frac <- sapply(split_px, `[`, 2)

  # now clean...fractional, +, NNN-nnn, simple dig, frac+decimal
  frac <- ifelse(grepl("\\/", frac), gsub(" ", "/32 ", paste0(frac, "*1/32")), frac)
  frac <- ifelse(grepl("\\+$", frac), gsub("\\+$", "/32 1/64", frac), frac)
  frac <- ifelse(nchar(frac) == 3 & !grepl("[^0-9]", frac), paste0(substr(frac, 1, 2), "/32 ", substr(frac, 3, 3), "/8*1/32"), frac)
  frac <- ifelse(!grepl("[^0-9]", frac), paste0(frac, "/32"), frac)
  frac <- ifelse(grepl("\\d\\.\\d", frac), gsub("\\.", "/32 .", paste0(frac, "*1/32")), frac)

  # return value, if NA try convert to numeric
  nums_parsed <- as.numeric(dp) + as.numeric(sapply(sub(" ", "+", frac), function(x) eval(parse(text = x))))
  nums_parsed[which(is.na(nums_parsed))] <- as.numeric(x[which(is.na(nums_parsed))])
  nums_parsed <- as.numeric(sprintf("%.5f", nums_parsed))

  return(nums_parsed)
}



