
#' @import methods
#' @import cli


#' @title \linkS4class{equiv}
#' 
#' @description
#' An `S4` object to determine the equivalence(s) at a margin.
#' 
#' @slot caption \link[base]{character} scalar
#' 
#' @slot current named \link[base]{numeric} \link[base]{vector} \eqn{x} without missing value.
#' 
#' @slot target (optional) \link[base]{numeric} \link[base]{vector} \eqn{x_0} of the same \link[base]{length} as the slot `@current`
#' 
#' @slot tol \link[base]{numeric} scalar, the tolerance, default value is `.Machine$double.eps`
#' 
#' @param object,x an \linkS4class{equiv} object
#' 
#' @param value see the function \link[base]{names<-}
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Bioequivalence}
#' 
#' @note
#' 
#' The parameter/slot `'caption'` is named after the function \link[flextable]{set_caption}. Another option is to use `title` as in the function \link[ggplot2]{labs}.
#' 
#' The parameters/slots `'current'` and `'target'` are named after the function \link[base]{all.equal.numeric}.
#' 
#' @examples 
#' equiv()
#' equiv(current = c(a = .6, b = 1.3) / 0)
#' 
#' equiv(current = c(a = .6, b = 1.3))
#' 
#' equiv(caption = 'Title', current = c(a = .6, b = 1.3))
#' 
#' equiv(current = c(a = .6, b = 1, c = 1.3), target = c(c = 1))
#' 
#' equiv(current = c(a = .6, b = 1.3, d = .9), target = c(b = 1, e = 1))
#'
#' cur = c(a=2e-3, b=2e-2, c=2e-1, d=2, e=2e1)
#' targt = rnorm(n = length(cur), mean = cur, sd = cur/10)
#' names(targt) = names(cur)
#' equiv(current = cur)
#' equiv(current = cur, target = targt)
#'
#' @export
setClass(Class = 'equiv', slots = c(
  caption = 'character',
  current = 'numeric',
  target = 'numeric',
  tol = 'numeric'
), prototype = prototype(
  tol = .Machine$double.eps
))


if (FALSE) {
  setClass(Class = 'newcls', contains = 'numeric', slot = c(names = 'character', any = 'ANY'))
  x = c(a = .6, b = 1.3) 
  x |>
    attr(which = 'names')
  
  # inherit directly from an atomic type via `contains`
  new('newcls', x)@.Data # names not retained!!!
  
  # explicit slot
  new('newcls', any = x)@any # names are retained
  
  # therefore, must use an explicit slot `@current`
}


#' @rdname equiv-class
#' @param caption,current,target,tol see **Slots**
#' @param ... additional parameters, currently of no use
#' @export
equiv <- \(
  caption = character(),
  current, 
  target = double(),
  tol = .Machine$double.eps,
  ...
) {
  
  if (length(caption) > 1L) stop('`caption` length must <=1L')
  
  if (missing(current)) return(new(Class = 'equiv'))
  
  if (any(id <- is.na(current))) current <- current[!id]
  
  if (all(is.infinite(current))) return(new(Class = 'equiv')) # divided by a total of 0

  if (any(is.infinite(current))) stop('does not allow Inf in `current`')
  
  if ((length(tol) != 1L) || is.na(tol)) stop('`tol` must be scalar')
  if (any(id <- (abs(current) < tol))) current <- current[!id]
  
  if (any(id <- (current < 0))) current <- current[!id] # exception handling
  
  nc <- length(current)
  if (!nc) return(new(Class = 'equiv'))
  
  nmc <- names(current)
  if (!length(nmc) || anyNA(nmc) || !all(nzchar(nmc))) {
    stop('`current` must be fully named')
  } 
  
  if (!length(target)) return(new(
    Class = 'equiv', 
    caption = caption, 
    current = current
  ))
  
  nmt <- names(target)
  if (!length(nmt) || anyNA(nmt) || !all(nzchar(nmt))) stop('@target must be fully named')
  
  if (!identical(nmt, nmc)) {
    z <- current * NA_real_
    nm <- intersect(nmc, nmt)
    z[nm] <- target[nm]
    target <- z
  }
  
  if (any(id <- (abs(current - 1) < .Machine$double.eps) & is.na(target))) {
    current <- current[!id]
    target <- target[!id]
  }
  
  return(new(
    Class = 'equiv', 
    caption = caption, 
    current = current, 
    target = target,
    tol = tol
  ))
  
}




