#' @title Converts a vector of fractional bond prices to decimal.
#'
#' @description This function is designed to handle all flavors of
#' fixed income fractional price quotes and to return decimal equivalents
#' for use in your scripts. Requires a character vector input.
#'
#' @return Returns a vector of decimalized bond prices
#' @param bond_px A character vector of fractional bond prices, can include decimal prices too
#' @export
#' @examples
#'
#' # convert fractional bond prices to decimal prices
#' x <- c("$101-28", "101-2 1/8", "99+")
#' bond_to_decimal(x)
#'

bond_to_decimal <- function(bond_px) {
  stopifnot(is.character(bond_px))
  x <- gsub(pattern = "$", replacement = "", x = bond_px, fixed = TRUE)
  x <- gsub(pattern = " ", replacement = "", x = x, fixed = TRUE)
  split_px <- strsplit(x = x, split = "-", fixed = TRUE)
  dp <- .Internal(unlist(x = lapply(X = split_px, FUN = `[`, 1L), recursive = FALSE, use.names = FALSE))
  frac <- .Internal(unlist(x = lapply(X = split_px, FUN = `[`, 2), recursive = FALSE, use.names = FALSE))
  idx0 <- which(x = grepl(pattern = "+", x = dp, fixed = TRUE))
  dp[idx0] <- as.character(x = as.numeric(x = gsub(pattern = "+", replacement = "", x = dp[idx0], fixed = TRUE)) + 0.015625)
  fractest <- is.na(x = frac) | frac == "00"
  frac <- frac[!fractest]
  idx1 <- which(grepl(pattern = "/", x = frac, fixed = TRUE))
  frac[idx1] <- gsub(pattern = " ", replacement = "/32 ", x = paste0(frac[idx1], "*1/32"), fixed = TRUE)
  idx2 <- grepl(pattern = "\\+$", x = frac)
  frac[idx2] <- gsub(pattern = "\\+$", replacement = "/32 1/64", x = frac[idx2])
  idx3 <- which(nchar(x = frac) == 3L & !grepl(pattern = "[^0-9]", x = frac))
  frac[idx3] <- paste0(.Internal(substr(x = frac[idx3], start = 1L, stop = 2L)), "/32 ", .Internal(substr(x = frac[idx3], start = 3L, stop = 3L)), "/8*1/32")
  idx4 <- which(x = !grepl(pattern = "[^0-9]", x = frac))
  frac[idx4] <- paste0(frac[idx4], "/32")
  idx5 <- which(x = grepl(pattern = "\\d\\.\\d", x = frac))
  frac[idx5] <- gsub(pattern = ".", replacement = "/32 .", x = paste0(frac[idx5], "*1/32"), fixed = TRUE)
  frac <- sub(pattern = " ", replacement = "+", x = frac, fixed = TRUE)
  pfrac <- str2expression(text = frac)
  idx6 <- which(x = fractest == T)
  if(sum(x = idx6) == 0L) {
    as.numeric(x = dp) + as.numeric(x = vapply(X = pfrac, FUN = eval, numeric(length = 1L)))
  } else {
    fractest[idx6] <- as.numeric(x = dp[idx6])
    fractest[-idx6] <- as.numeric(x = dp[-idx6]) + as.numeric(x = vapply(X = pfrac, FUN = eval, numeric(length = 1L)))
    return(fractest)
  }
}
