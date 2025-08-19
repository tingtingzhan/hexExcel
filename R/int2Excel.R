
#' @title \link[base]{integer} to Excel-Style Hexavigesimal (`A-Z`)
#' 
#' @description
#' Convert \link[base]{integer} to Excel-style hexavigesimal (`A-Z`).
#' 
#' @param x \link[base]{integer} scalar or \link[base]{vector}
#'  
#' @returns 
#' 
#' Function [int2Excel()] returns a 
#' \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export
int2Excel <- function(x) {
  xok <- !is.na(x)
  x_ <- x[xok]
  ret <- rep(NA_character_, times = length(x))
  ret_ <- character(length = length(x_))
  repeat {
    id <- x_ %% 26
    id[(x_ != 0L) & (id == 0L)] <- 26L # because I had `-1L`
    j <- (id == 0L)
    if (all(j)) break
    ret_[!j] <- paste0(LETTERS[id[!j]], ret_[!j])
    x_ <- pmax(0L, (x_ - 1L)) %/% 26 # -1L is so smart!!!
  }
  ret[xok] <- ret_
  return(ret)
}

