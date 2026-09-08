

#' @title Label a Numeric Vector by Bin
#' 
#' @param x a \link[base]{numeric} scalar
#' 
#' @param ... additional parameters of the function \link[scales]{label_number}
#' 
#' @returns
#' The function [binlabel()] returns a \link[base]{function}.
#' 
#' @note
#' The function [binlabel()] is named after \link[base]{.bincode}.
#' 
#' @importFrom scales label_number
#' @export
binlabel <- \(x, ...) { # accuracy = .1
  
  if ((length(x) != 1L) || !is.numeric(x) || is.na(x)) stop('illegal input')
  
  x |> 
    .bincode(
      breaks = c(0, .001, .01, 1, Inf),
      right = FALSE # important!!
    ) |> 
    switch('1' = { # (0, .001)
      \(newx) {
        z <- label_number(scale = 1e4, suffix = '\u2031', ...)(newx)
        z[is.na(newx) | (newx < 1e-5)] <- '-'
        return(z)
      }
    }, '2' = { # [.001, .01)
      \(newx) {
        z <- label_number(scale = 1e3, suffix = '\u2030', ...)(newx)
        z[is.na(newx) | (newx < 1e-4)] <- '-'
        return(z)
      }
    }, '3' = { # [.01, 1)
      \(newx) {
        z <- label_number(scale = 1e2, suffix = '%', ...)(newx)
        z[is.na(newx) | (newx < 1e-3)] <- '-'
        return(z)
      }
    }, '4' = { # [1, Inf) 
      \(newx) {
        z <- label_number(...)(newx)
        z[is.na(newx) | (newx < 1e-2)] <- '-'
        return(z)
      }
    }, stop('shouldnt come here'))
  
}



if (FALSE) {
  binlabel_self <- \(x, FUN, ...) {
    x |> 
      binlabel(x = FUN(x), ...)()
    # `...` is for [binlabel], **not** for `FUN`
    # HOWEVER!!
    # this is not a good idea!
    # the 2nd pipe cannot return a function, without `x` !!!
  }
}


