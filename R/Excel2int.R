


#' @title Excel-Style Hexavigesimal (`A-Z`) to \link[base]{integer}
#' 
#' @description
#' Convert Excel-style hexavigesimal (`A-Z`) to \link[base]{integer}.
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector},
#' only letters `A-Z` and `a-z`.
#'  
#' @returns 
#' 
#' Function [Excel2int()] returns an 
#' \link[base]{integer} \link[base]{vector}.
#' 
#' @keywords internal
#' @export
Excel2int <- function(x) {
  xok <- !is.na(x)
  z <- x |>
    Excel2C() |>
    strtoi(base = 26L)
  if (any(is.na(z[xok]) | z[xok] < 0)) stop('should not happen')
  return(z)
}




