#' Winkler score for probabilistic forecasts
#' 
#' @param actual numeric vector of actual values
#' @param lower numeric vector of lower bounds
#' @param upper numeric vector of upper bounds
#' @param level numeric level of confidence
#' @param scale logical, if TRUE, the score is scaled by the range of the bounds
#' @return numeric score
#' @export
#' @examples
#' 
#' actual <- c(1, 2, 3, 4, 5)
#' lower <- c(0, 1, 2, 3, 4)
#' upper <- c(2, 3, 4, 5, 6)
#' winkler_score(actual, lower, upper)
#' winkler_score(actual, lower, upper, scale = TRUE)
#' winkler_score(actual, lower, upper, level = 99)
#' winkler_score(actual, lower, upper, level = 99, scale = TRUE)
#' 
winkler_score <- function(actual, lower, upper, level = 95, scale = FALSE) {
  alpha <- 1 - level / 100
  lt <- lower
  ut <- upper
  n_points <- length(actual)
  diff_lt <- lt - actual
  diff_bounds <- ut - lt
  diff_ut <- actual - ut
  score <-
    diff_bounds + (2 / alpha) * (pmax(diff_lt, 0) + pmax(diff_ut, 0))
  if (!scale)
  {
    return(mean(score))
  } else {
    return(mean(score/diff_bounds))
  }
}