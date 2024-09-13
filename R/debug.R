#' Debug print
#' 
#' @param x An object to be printed
#' @export
#' @examples
#' 
#' misc::debug_print(1:10)
#' misc::debug_print("Hello, world!")
#' 
debug_print <- function(x) {
  cat("\n")
  print(paste0(deparse(substitute(x)), "'s value:"))
  print(x)
  cat("\n")
}