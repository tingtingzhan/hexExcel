
#' @title Excel-Style Hexavigesimal (`A-Z`) to C-Style (`0-9; A-P`)
#' 
#' @description
#' Convert Excel-style hexavigesimal (`A-Z`) to C-style (`0-9; A-P`).
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector},
#' only letters `A-Z` and `a-z`.
#'  
#' @returns 
#' 
#' Function [Excel2C()] returns a 
#' \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export
Excel2C <- function(x) {
  ret <- rep(NA_character_, time = length(x))
  
  xok <- !is.na(x)
  x1 <- toupper(x[xok])
  if (any(id <- grepl(pattern = '[^A-Z]', x = x1))) stop(paste0(sQuote(x1[id]), collapse = ', '), ' contains characters that are not A-Z')
  
  excel2c <- \(x_) {
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
  
  ret[xok] <- x1 |> 
    strsplit(split = '', fixed = TRUE) |>
    vapply(FUN = excel2c, FUN.VALUE = '')
  return(ret)
}

