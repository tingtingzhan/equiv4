
#' @import methods


#' @title \linkS4class{equiv}
#' 
#' @description
#' An `S4` object to determine the equivalence at a margin.
#' 
#' @slot current,target \link[base]{numeric} scalars, named after the function \link[base]{all.equal.numeric}
#' 
#' @slot margin \link[base]{numeric} scalar, the acceptance margin of the ratio `current/target`, default value is 1.1.  In other words, `current` is considered equivalent to `target`, if `current/target` \eqn{\in} `(1/margin, margin)`.
#' 
#' @slot tol \link[base]{numeric} scalar, default value is `.Machine$double.eps`
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Bioequivalence}
#' 
#' @examples 
#' new('equiv', current = .6)
#' new('equiv', current = .6, target = 1)
#' new('equiv', current = 1.3, target = 1)
#' 
#' @name equiv-class
#' @export
setClass(Class = 'equiv', slots = c(
  current = 'numeric',
  target = 'numeric',
  margin = 'numeric',
  tol = 'numeric'
), prototype = prototype(
  current = NA_real_,
  target = NA_real_,
  margin = 1.1,
  tol = .Machine$double.eps
))






setMethod(f = initialize, signature = 'equiv', definition = \(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)
  
  if (!length(x@current)) x@current <- NA_real_
  
  return(x)
  
})






#' @rdname equiv-class
#' @param object an \linkS4class{equiv} object
#' @export
setMethod(f = show, signature = 'equiv', definition = \(object) {
  z <- object |> 
    format.equiv()
  paste0(names(z), ': ', z) |>
    cat(sep = '\n')
})

