

#' @title Row-Bind Unbalanced Vectors
#' 
#' @param ... two or more \link[base]{vector}s of the same \link[base]{storage.mode}
#' 
#' @examples
#' rbind_by_name(c(a = 1, b = 2), c(a = 11, c = 13))
#' rbind_by_name(A = c(a = 1, b = 2), B = c(a = 11, c = 13))
#' @export
rbind_by_name <- \(...) {
  
  dots <- list(...)
  
  dots <- dots[lengths(dots) > 0L]
  if (!length(dots)) return(invisible())
  
  nms <- dots |>
    lapply(FUN = names)
  
  nms |>
    lapply(FUN = \(i) {
      if (!length(i)) stop('each element must be named')
      if (anyNA(i) || !all(nzchar(i))) stop('each element must be named')
    })
  
  m <- dots |>
    vapply(FUN = storage.mode, FUN.VALUE = '') |>
    unique.default()
  if (length(m) != 1L) stop('all element should be one mode')
  
  rnm <- names(dots)
  if (length(rnm)) {
    if (anyNA(rnm) || !all(nzchar(rnm))) stop()
  }
  
  cnm <- nms |>
    unname() |> # in case `length(rnm)`
    do.call(what = c, args = _) |>
    unique.default() # union of names
  
  z <- switch(m, double = NA_real_, character = NA_character_, integer = NA_integer_) |>
    array(dim = c(length(dots), length(cnm)), dimnames = list(rnm, cnm))
  
  for (i in seq_along(dots)) {
    z[i, nms[[i]]] <- dots[[i]]
  }
  
  return(z)
  
}



#' @title Summation by Name
#' 
#' @description ..
#' 
#' @param ... \strong{named} \link[base]{numeric} \link[base]{vector}s
#' 
#' @return 
#' The function [sum_by_name()] returns a \link[base]{numeric} \link[base]{vector}.
#' 
#' @examples
#' x1 = c(a = 2, b = 3)
#' x2 = c(b = 2, c = 1)
#' x3 = c(a = 1, b = -2, c = -1)
#' sum_by_name(x1, x2, x3)
#' @export
sum_by_name <- \(...) {
  xs <- list(...)
  xs <- xs[lengths(xs) > 0L]
  if (!length(xs)) return(numeric())
  xs |>
    do.call(what = 'rbind_by_name', args = _) |> 
    colSums(na.rm = TRUE)
}


