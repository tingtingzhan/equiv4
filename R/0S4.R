
#' @import methods


#' @title \linkS4class{equiv}
#' 
#' @description
#' An `S4` object to determine the equivalence(s) at a margin.
#' 
#' @slot current named \link[base]{numeric} \link[base]{vector} \eqn{x} without missing value.
#' 
#' @slot target \link[base]{numeric} \link[base]{vector} \eqn{x_0} of the same \link[base]{length} as the parameter `current`
#' 
#' @slot margin \link[base]{numeric} scalar, the acceptance margin \eqn{m} of the ratio \eqn{x/x_0}, default value is 1.1.  In other words, \eqn{x} is considered equivalent to \eqn{x_0}, if \eqn{x/x_0\in (1/m, m)}.
#' 
#' @slot tol \link[base]{numeric} scalar, the tolerance, default value is `.Machine$double.eps`
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Bioequivalence}
#' 
#' @note
#' The parameters `'current'` and `'target'` are named after the function \link[base]{all.equal.numeric}.
#' 
#' @examples 
#' new('equiv')
#' 
#' new('equiv', current = c(a = .6, b = 1.3))
#' 
#' new('equiv', current = c(a = .6, b = 1.3) / 0)
#' 
#' new('equiv', current = c(a = .6, b = 1, c = 1.3), 
#'  target = c(a = NA_real_, b = NA_real_, c = 1))
#' 
#' new('equiv', current = c(a = .6, b = 1.3, d = .9), 
#'  target = c(b = 1, e = 1))
#'
#' cur = c(a=2e-3, b=2e-2, c=2e-1, d=2, e=2e1)
#' targt = rnorm(n = length(cur), mean = cur, sd = cur/10)
#' names(targt) = names(cur)
#' new('equiv', current = cur)
#' new('equiv', current = cur, target = targt)
#'  
#' @name equiv-class
#' @export
setClass(Class = 'equiv', slots = c(
  current = 'numeric',
  target = 'numeric',
  margin = 'numeric',
  tol = 'numeric'
), prototype = prototype(
  margin = 1.1,
  tol = .Machine$double.eps
))


if (FALSE) {
  setClass(Class = 'equiv0', contains = 'numeric', slot = c(names = 'character', any = 'ANY'))
  x = c(a = .6, b = 1.3) 
  x |>
    attr(which = 'names')
  
  # inherit directly from an atomic type via `contains`
  new('equiv0', x)@.Data # names not retained!!!
  
  # explicit slot
  new('equiv0', rnorm(3), any = x)@any # names are retained
}




setMethod(f = initialize, signature = 'equiv', definition = \(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)

  if (any(id <- is.na(x@current))) x@current <- x@current[!id]
  
  if (all(is.infinite(x@current))) {
    # divided by a total of 0
    x@current <- numeric()
    return(x)
  }
  
  if (any(is.infinite(x@current))) stop('does not allow Inf in @current')
  
  if ((length(x@tol) != 1L) || is.na(x@tol)) stop('@tol must be scalar')
  if (any(id <- (abs(x@current) < x@tol))) x@current <- x@current[!id]
  
  if (any(id <- (x@current < 0))) x@current <- x@current[!id] # exception handling
  
  nc <- length(x@current)
  if (!nc) return(x) # len-0 `@current`
  
  nmc <- names(x@current)
  if (!length(nmc) || anyNA(nmc) || !all(nzchar(nmc))) {
    stop('@current must be fully named')
  } 
  
  if (!length(x@target)) return(x)
  
  nmt <- names(x@target)
  if (!length(nmt) || anyNA(nmt) || !all(nzchar(nmt))) stop('@target must be fully named')
  
  if (!identical(nmt, nmc)) {
    z <- x@current * NA_real_
    nm <- intersect(nmc, nmt)
    z[nm] <- x@target[nm]
    x@target <- z
  }
  
  if (any(id <- (abs(x@current - 1) < .Machine$double.eps) & is.na(x@target))) {
    x@current <- x@current[!id]
    x@target <- x@target[!id]
  }
  
  return(x)
  
})




#' @rdname equiv-class
#' @param object,x an \linkS4class{equiv} object
#' @importFrom charwidth row_fmt_matrix
#' @export
setMethod(f = show, signature = 'equiv', definition = \(object) {
  fmt <- object |> 
    format.equiv()
  if (!length(fmt)) return(invisible()) # exception handling
  fmt |>
    row_fmt_matrix() |>
    cat(sep = '\n')
  # cli_verbatim() # sep by '\n' by default
})

