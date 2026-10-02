


#' @rdname equiv-class
#' 
#' @details
#' The \link[base]{length} method returns an \link[base]{integer}, which is the \link[base]{length} of the slot `@current`.
#' 
#' @export
setMethod(f = length, signature = 'equiv', definition = \(x) {
  x@current |> 
    length()
})
