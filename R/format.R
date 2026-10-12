
# @param margin \link[base]{numeric} scalar, the acceptance margin \eqn{m} of the ratio \eqn{x/x_0}, default value is 1.1.  In other words, \eqn{x} is considered equivalent to \eqn{x_0}, if \eqn{x/x_0\in (1/m, m)}.
#' 


#' @export
format.equiv <- \(x, margin = 1.1, accuracy = .1, ...) {
  
  n <- length(x@current)
  if (!n) return(invisible()) # exception handling
  
  # [binlabel] by column!!!
  
  fn <- \(..., accuracy) {
    min(..., na.rm = TRUE) |>
      binlabel(accuracy = accuracy)
  }

  z <- array('-', dim = c(2L, n), dimnames = list(
    c('Current', 'Target'), 
    names(x@current)
  ))
  
  fn_label <- if (length(x@target)) {
    mapply(FUN = fn, x@current, x@target, accuracy = accuracy, SIMPLIFY = FALSE)
  } else {
    mapply(FUN = fn, x@current, accuracy = accuracy, SIMPLIFY = FALSE)
  }
  
  fmt_current <- mapply(
    FUN = \(fn, x) fn(x), 
    fn = fn_label, x = x@current, 
    SIMPLIFY = TRUE
  )

  if (length(x@target)) {
    
    rel <- (x@current/x@target) |> 
      .bincode(breaks = c(0, 1/margin, margin, Inf))
    
    z[1L, ] <- fmt_current |>
      mapply(FUN = \(x, rel) {
        switch(as.character(rel), '1' = { # current < target
          x |> col_br_blue() |> style_bold()
        }, 'NA' =, '2' = { # current == target
          x |> col_grey()
        }, '3' = { # current > target
          x |> col_br_red() |> style_bold()
        })
      }, x = _, rel = rel)
    
    z[2L, ] <- mapply(
      FUN = \(fn, x) fn(x), 
      fn = fn_label, x = x@target, 
      SIMPLIFY = TRUE
    )
    
  } else {

    z[1L, ] <- fmt_current
    
  }
  
  return(z)

}



