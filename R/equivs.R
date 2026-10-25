

#' @title equivs
#' 
#' @param ... one or more \linkS4class{equiv} objects
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
      print(i, ...) # [print.equiv]
      cat('\n')
    })
  
  return(invisible())
  
}