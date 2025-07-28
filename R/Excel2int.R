


#' @title Excel-Style Hexavigesimal (A to Z)
#' 
#' @description
#' Convert between decimal, C-style hexavigesimal (`0` to `9`, `A` to `P`), and Excel-style hexavigesimal (`A` to `Z`).
#' 
#' @param x \link[base]{integer} scalar or \link[base]{vector} for function [int2Excel]. 
#' \link[base]{character} scalar or \link[base]{vector} for functions [Excel2C] and [Excel2int],
#' which consists of (except missingness)
#' only letters `A` to `Z` and `a` to `z`.
#'  
#' @returns 
#' 
#' Function [Excel2int()] returns an 
#' \link[base]{integer} \link[base]{vector}.
#' 
#' Function [Excel2C()] returns a 
#' \link[base]{character} \link[base]{vector}.
#' 
#' Function [int2Excel()] returns a 
#' \link[base]{integer} \link[base]{vector}.
#' 
#' @keywords internal
#' @name hexavigesimalExcel
#' @export
Excel2int <- function(x) {
  xok <- !is.na(x)
  z <- strtoi(x = Excel2C(x), base = 26L)
  if (any(is.na(z[xok]) | z[xok] < 0)) stop('should not happen')
  return(z)
}



#' @rdname hexavigesimalExcel
#' @export
Excel2C <- function(x) {
  ret <- rep(NA_character_, time = length(x))
  
  xok <- !is.na(x)
  x1 <- toupper(x[xok])
  if (any(id <- grepl(pattern = '[^A-Z]', x = x1))) stop(paste0(sQuote(x1[id]), collapse = ', '), ' contains characters that are not A-Z')
  
  x2 <- strsplit(x1, split = '', fixed = TRUE)
  
  excel2c <- function(x_) {
    # x_ = x2[[1L]]
    id <- match(x_, table = LETTERS)
    if (anyNA(id)) stop('wont happen')
    id <- c(0L, id) 
    while (length(i26 <- which(id == 26L))) {
      id[i26] <- 0L
      id[i26 - 1L] <- id[i26 - 1L] + 1L
    }
    tmp <- character(length(id))
    i9 <- (id <= 9L)
    tmp[i9] <- as.character(id[i9])
    tmp[!i9] <- letters[id[!i9] - 9L]
    out <- paste(tmp, collapse = '')
    return(gsub(pattern = '^0+', replacement = '', x = out))
    # ?base::strtoi handles preceeding '0' correctly; i.e., 
    # stopifnot(identical(strtoi('01', base = 26L), strtoi('1', base = 26L)))
    # I remove leading 0's just for pretty output
  }
  
  ret[xok] <- vapply(x2, FUN = excel2c, FUN.VALUE = '')
  return(ret)
}


#' @name hexavigesimalExcel
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


