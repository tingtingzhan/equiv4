
#' @title Label a Numeric Vector by Bin
#' 
#' @param x a \link[base]{numeric} scalar
#' 
#' @param ... additional parameters to be passed into the function \link[scales]{label_number}, e.g., `accuracy`, etc.
#' 
#' @returns
#' The function [binlabel()] returns a \link[base]{function}.
#' 
#' @note
#' The function [binlabel()] is named after \link[base]{.bincode}.
#' 
#' @examples
#' binlabel(.03)(.873)
#' binlabel(.03, accuracy = .1)(.873)
#' binlabel(.003)(.87)
#' 
#' @importFrom scales label_number
#' @export
binlabel <- \(x, ...) {
  
  if ((length(x) != 1L) || !is.numeric(x) || is.na(x)) stop('input `x` must be numeric scalar')
  
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





