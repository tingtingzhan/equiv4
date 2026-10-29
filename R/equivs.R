

#' @title equivs
#' 
#' @param ... one or more \linkS4class{equiv} objects with unique `@caption` slots
#' 
#' @returns
#' The function [equivs()] returns an `S3` object of class `'equivs'`.
#' 
#' @seealso [equivlist()]
#' 
#' @export
equivs <- \(...) {
  
  x <- list(...)
  x <- x[lengths(x) > 0L]
  if (!length(x)) return(invisible())
  
  xcap <- x |>
    vapply(FUN = \(i) i@caption, FUN.VALUE = '') |>
    duplicated.default()
  if (any(xcap)) stop('all `equiv` elements must have unique `@caption`')
  
  class(x) <- c('equivs', 'list')
  return(x)
  
}



#' @export
print.equivs <- \(x, ...) {
  
  x |>
    lapply(FUN = \(i) { # do not want to print list names
      print.equiv(i, ...)
      cat('\n')
    })
  
  return(invisible())
  
}