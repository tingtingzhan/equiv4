
#' @importFrom cli col_br_blue col_grey col_br_red style_bold
#' @export
format.equiv <- \(x, accuracy = .1, ...) {
  if (!(n <- length(x@current))) return(invisible()) # exception handling
  current <- sum(x@current)
  if (is.na(current)) return(invisible())
  if (abs(current) < x@tol) return(invisible()) # exception handling
  
  if (current < 0) return(invisible())
  # I do not have `@water` for all puree, yet
  
  .label <- min(current, x@target, na.rm = TRUE) |> 
    .binlabel(accuracy = accuracy)
  
  current <- .label(current)
  
  if (!length(x@target) || is.na(x@target)) {
    return(c(Current = current, Target = '-'))
  }
  
  rel <- .bincode(
    x@current/x@target, 
    breaks = c(0, 1/x@margin, x@margin, Inf)
  )
  
  current <- switch(as.character(rel), '1' = { # current < target
    current |> col_br_blue() |> style_bold()
  }, 'NA' =, '2' = { # current == target
    current |> col_grey()
  }, '3' = { # current > target
    current |> col_br_red() |> style_bold()
  })
  
  return(c(Current = current, Target = .label(x@target)))
  
}



