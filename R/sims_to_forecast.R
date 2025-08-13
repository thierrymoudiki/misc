#' Convert Simulation Matrix to Forecast Object
#'
#' Transforms a matrix of simulated future paths into a \code{forecast} class object
#' containing point forecasts (means) and prediction intervals, while preserving
#' time series properties.
#'
#' @param y The original time series (of class \code{ts}) used to generate simulations.
#' @param sims A matrix of simulated future paths where each column represents one
#'             simulation path and each row represents a time period. Must have same
#'             frequency as \code{y}.
#' @param level The confidence level for prediction intervals (default: 95).
#' @param keep_first_row Logical indicating whether to retain the first row of \code{sims}.
#'                      If \code{FALSE} (default), first row is dropped (assumed to be
#'                      present-day value).
#'
#' @return An object of class \code{forecast} containing:
#' \itemize{
#'   \item \code{x}: Original time series
#'   \item \code{mean}: Point forecasts (row means of simulations)
#'   \item \code{lower}: Lower prediction bounds
#'   \item \code{upper}: Upper prediction bounds
#'   \item \code{sims}: Full simulation matrix (as time series)
#'   \item \code{level}: Confidence level used
#' }
#' All time series outputs maintain the original frequency and start at the next
#' observation after \code{y}.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Calculates point forecasts as row means of \code{sims}
#'   \item Computes prediction intervals using quantiles at \code{(1-level)/2} and \code{1-(1-level)/2}
#'   \item Converts all outputs to \code{ts} objects with correct temporal alignment
#'   \item Optionally handles initial condition rows via \code{keep_first_row}
#' }
#'
#' @examples
#' \dontrun{
#' library(forecast)
#' y <- ts(rnorm(20), frequency=4)
#' sims <- matrix(rnorm(200), nrow=10, ncol=20) # 10 steps, 20 simulations
#' fc <- sims_to_forecast_object(y, sims, level=80)
#' autoplot(fc)  # Requires ggplot2 and forecast packages
#' }
#'
#' @seealso
#' \code{\link[forecast]{forecast}} for standard forecast objects,
#' \code{\link[stats]{quantile}} for interval calculation method
#'
#' @importFrom stats frequency ts quantile
#' @export
sims_to_forecast_object <- function(y, sims, level=95,
                                    keep_first_row=TRUE) {
  alpha <- 1 - level/100
  obj <- list()
  freq_y <- frequency(y)
  stopifnot(freq_y == frequency(sims))
  obj$x <- y
  obj$level <- level
  if (keep_first_row)
    sims <- sims[-1, ]
  obj$mean <- as.numeric(rowMeans(sims))
  obj$lower <- as.numeric(apply(sims, 1, function(x) quantile(x, probs=alpha)))
  obj$upper <- as.numeric(apply(sims, 1, function(x) quantile(x, probs=1-alpha)))
  start_next_date <- misc::get_next_date(y)$end
  obj$mean <- ts(obj$mean, start=start_next_date, frequency=freq_y)
  obj$lower <- ts(obj$lower, start=start_next_date, frequency=freq_y)
  obj$upper <- ts(obj$upper, start=start_next_date, frequency=freq_y)
  obj$sims <- ts(sims, start=start_next_date, frequency=freq_y)
  class(obj) <- "forecast"
  return(obj)
}
