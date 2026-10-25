

#' @title equivlist
#' 
#' @param ... one or more \linkS4class{equiv} objects
#' 
#' @export
equivlist <- \(...) {
  
  x <- list(...)
  x <- x[lengths(x) > 0L]
  if (!length(x)) return(invisible())
  
  xcap <- x |> 
    vapply(FUN = \(i) i@caption, FUN.VALUE = '') |>
    duplicated.default()
  if (!all(xcap[-1L])) stop('all `equiv` elements must have the same `@caption`')
  
  class(x) <- c('equivlist', 'list')
  return(x)
  
}



#' @importFrom charwidth row_fmt_matrix
#' @export
print.equivlist <- \(x, fmt_caption = '%s', ...) {
  
  y1 <- x |>
    lapply(FUN = \(i) i@current) |>
    do.call(what = rbind_unbalanced, args = _)
  
  if (!length(y1)) return(invisible())
  
  y2 <- y1[, colMeans(is.na(y1)) != 1L, drop = FALSE]
  #y3 <- y2[rowMeans(is.na(y2)) != 1L, , drop = FALSE]
  y3 <- y2
  if (!length(y3)) return(invisible())
  if (all(is.na(y3))) return(invisible())
  if (all(abs(y3) < .Machine$double.eps, na.rm = TRUE)) return(invisible())
  
  y <- y3 |>
    apply(MARGIN = 2L, FUN = median_binlabel, accuracy = .1, simplify = FALSE) |>
    do.call(what = cbind) # to make sure not getting a 'vector'
  
  x[[1L]]@caption |> 
    sprintf(fmt = fmt_caption) |> 
    style_bold() |> bg_br_yellow() |> 
    cat('\n')
  y |>
    row_fmt_matrix() |>
    cat(sep = '\n')
  cat('\n')
  return(invisible(y))
}





#' @importFrom stats median.default
#' @importFrom equiv4 binlabel
median_binlabel <- \(x, ...) {
  x |>
    binlabel(median.default(x, na.rm = TRUE), ...)()
}
