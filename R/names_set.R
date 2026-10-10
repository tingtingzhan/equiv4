


#' @rdname equiv-class
#' 
#' @details
#' The \link[base]{names} method returns an \link[base]{character} \link[base]{vector}, which is the \link[base]{names} of the slot `@current`.
#' 
#' @export
setMethod(f = names, signature = 'equiv', definition = \(x) {
  x@current |> 
    names()
})



#' @rdname equiv-class
#' 
#' @details
#' The \link[base]{names<-} method returns an \linkS4class{equiv} object.
#' 
#' @export
setMethod(f = 'names<-', signature = 'equiv', definition = \(x, value) {
  names(x@current) <- value
  return(x)
})


