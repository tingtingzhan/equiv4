


#' @title \link[base]{length} of \linkS4class{equiv} Object
#' 
#' @param x a \linkS4class{equiv} object
#' 
#' @export
setMethod(f = length, signature = 'equiv', definition = \(x) {
  x@current |> 
    length()
})
