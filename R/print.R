#' @export
print.equiv <- \(x, fmt_caption = '%s\n', ...) {
  
  fmt <- x |> 
    format.equiv()
  if (!length(fmt)) return(invisible()) # exception handling
  
  x@caption |> # len0 compatible
    sprintf(fmt = fmt_caption) |> 
    bg_br_yellow() |> style_bold() |>
    cat()
  
  fmt |>
    row_fmt_matrix() |>
    cat(sep = '\n')
  # cli_verbatim() # sep by '\n' by default
}


#' @rdname equiv-class
#' @importFrom charwidth row_fmt_matrix
#' @export
setMethod(f = show, signature = 'equiv', definition = \(object) print.equiv(object))