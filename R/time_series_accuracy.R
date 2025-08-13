#' Calculate Coverage Score for Prediction Intervals
#'
#' Computes the proportion of actual values that fall within the prediction
#' intervals, expressed as a percentage. This measures how well the prediction
#' intervals capture the true values.
#'
#' @param obj A forecast object containing prediction intervals. Should have either
#'   \code{lower} and \code{upper} components or an \code{intervals} matrix with
#'   lower bounds in column 1 and upper bounds in column 2.
#' @param actual Numeric vector of actual observed values to compare against
#'   the prediction intervals.
#'
#' @return Numeric value representing the coverage percentage (0-100).
#'   Higher values indicate better interval coverage.
#'
#' @details
#' The function first checks if the forecast object has \code{lower} and
#' \code{upper} components. If not, it assumes the intervals are stored in
#' a matrix format where column 1 contains lower bounds and column 2 contains
#' upper bounds.
#'
#' @examples
#' \dontrun{
#' # Example with lower/upper components
#' forecast_obj <- list(lower = c(10, 15, 20), upper = c(20, 25, 30))
#' actual_values <- c(12, 18, 25)
#' coverage_score(forecast_obj, actual_values)
#'
#' # Example with intervals matrix
#' forecast_obj2 <- list(intervals = matrix(c(10, 15, 20, 20, 25, 30), ncol = 2))
#' coverage_score(forecast_obj2, actual_values)
#' }
#'
#' @export
coverage_score <- function(obj, actual) {
  if (is.null(obj$lower)) {
    return(mean((obj$intervals[, 1] <= actual) * (actual <= obj$intervals[, 2]), na.rm = TRUE) * 100)
  }
  return(mean((obj$lower <= actual) * (actual <= obj$upper), na.rm = TRUE) * 100)
}
#coverage_score <- memoise::memoise(coverage_score)

#' Calculate Winkler Score for Prediction Intervals
#'
#' Computes the Winkler score, which evaluates the quality of prediction intervals
#' by penalizing both the width of intervals and coverage failures. Lower scores
#' indicate better performance.
#'
#' @param obj A forecast object containing prediction intervals. Should have either
#'   \code{lower} and \code{upper} components or an \code{intervals} matrix with
#'   lower bounds in column 1 and upper bounds in column 2.
#' @param actual Numeric vector of actual observed values to compare against
#'   the prediction intervals.
#' @param level Numeric value specifying the confidence level of the prediction
#'   intervals (default: 95). Used to calculate the penalty factor alpha.
#'
#' @return Numeric value representing the mean Winkler score. Lower values
#'   indicate better interval forecasts.
#'
#' @details
#' The Winkler score is calculated as:
#' \deqn{S = (U - L) + \frac{2}{\alpha} \max(L - y, 0) + \frac{2}{\alpha} \max(y - U, 0)}
#' where:
#' \itemize{
#'   \item U and L are the upper and lower bounds of the prediction interval
#'   \item y is the actual observed value
#'   \item alpha = 1 - level/100 is the significance level
#' }
#'
#' The score penalizes wide intervals (first term) and coverage failures
#' (second and third terms). The penalty for coverage failures increases
#' as the confidence level increases.
#'
#' @references
#' Winkler, R. L. (1972). A decision-theoretic approach to interval estimation.
#' Journal of the American Statistical Association, 67(337), 187-191.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' forecast_obj <- list(lower = c(10, 15, 20), upper = c(20, 25, 30))
#' actual_values <- c(12, 18, 35)  # Note: 35 is outside the last interval
#' winkler_score2(forecast_obj, actual_values, level = 95)
#'
#' # With 80% confidence intervals
#' winkler_score2(forecast_obj, actual_values, level = 80)
#' }
#'
#' @export
winkler_score2 <- function(obj, actual, level = 95) {
  alpha <- 1 - level / 100
  lt <- try(obj$lower, silent = TRUE)
  ut <- try(obj$upper, silent = TRUE)
  actual <- as.numeric(actual)

  if (is.null(lt) || is.null(ut)) {
    lt <- as.numeric(obj$intervals[, 1])
    ut <- as.numeric(obj$intervals[, 2])
  }

  n_points <- length(actual)
  stopifnot((n_points == length(lt)) && (n_points == length(ut)))

  diff_lt <- lt - actual
  diff_bounds <- ut - lt
  diff_ut <- actual - ut

  score <- diff_bounds
  score <- score + (2 / alpha) * (pmax(diff_lt, 0) + pmax(diff_ut, 0))

  return(mean(score, na.rm = TRUE))
}
#winkler_score2 <- memoise::memoise(winkler_score2)

#' Calculate Multiple Accuracy Metrics for Forecasts
#'
#' Computes a comprehensive set of accuracy metrics to evaluate forecast
#' performance, including error measures, scaled metrics, and interval
#' evaluation scores.
#'
#' @param obj A forecast object containing at minimum a \code{mean} component
#'   with point forecasts and an \code{x} component with the original time series.
#'   Should also contain prediction intervals (either \code{lower}/\code{upper}
#'   or \code{intervals} matrix) for coverage and Winkler score calculations.
#' @param actual Numeric vector of actual observed values to compare against
#'   the forecasts.
#' @param level Numeric value specifying the confidence level of the prediction
#'   intervals (default: 95). Used for Winkler score calculation.
#'
#' @return Named numeric vector containing the following accuracy metrics:
#' \describe{
#'   \item{me}{Mean Error - average of forecast errors}
#'   \item{rmse}{Root Mean Square Error - square root of average squared errors}
#'   \item{mae}{Mean Absolute Error - average of absolute forecast errors}
#'   \item{mpe}{Mean Percentage Error - average of percentage errors}
#'   \item{mape}{Mean Absolute Percentage Error - average of absolute percentage errors}
#'   \item{mase}{Mean Absolute Scaled Error - MAE scaled by seasonal naive forecast MAE}
#'   \item{coverage}{Coverage score - percentage of actuals within prediction intervals}
#'   \item{winkler}{Winkler score - penalized interval width and coverage failures}
#' }
#'
#' @details
#' The function calculates both point forecast accuracy metrics (ME, RMSE, MAE,
#' MPE, MAPE, MASE) and interval forecast metrics (coverage, Winkler score).
#'
#' The MASE (Mean Absolute Scaled Error) uses seasonal naive forecasts as the
#' benchmark, with the scaling factor determined by the frequency of the original
#' time series. For seasonal data (frequency > 1), it uses seasonal differences;
#' for non-seasonal data, it uses first differences.
#'
#' @examples
#' \dontrun{
#' # Example with a forecast object
#' library(forecast)
#' ts_data <- ts(rnorm(50), frequency = 12)
#' fit <- auto.arima(ts_data[1:40])
#' fc <- forecast(fit, h = 10)
#' actual_test <- ts_data[41:50]
#'
#' # Calculate all accuracy metrics
#' metrics <- accuracy(fc, actual_test)
#' print(metrics)
#' }
#'
#' @seealso \code{\link{coverage_score}}, \code{\link{winkler_score2}}
#'
#' @export
time_series_accuracy <- function(obj, actual, level = 95) {
  actual <- as.numeric(actual)
  mean_prediction <- as.numeric(obj$mean)

  me <- mean(mean_prediction - actual)
  rmse <- sqrt(mean((mean_prediction - actual)**2, na.rm = TRUE))
  mae <- mean(abs(mean_prediction - actual), na.rm = TRUE)
  mpe <- mean(mean_prediction / actual - 1, na.rm = TRUE)
  mape <- mean(abs(mean_prediction / actual - 1), na.rm = TRUE)

  m <- frequency(obj$x)  # e.g., 12 for monthly data

  # Compute scaling factor (MAE of in-sample seasonal naive forecasts)
  if (m > 1) {
    scale <- mean(abs(diff(obj$x, lag = m)), na.rm = TRUE)
  } else {
    scale <- mean(abs(diff(obj$x, lag = 1)), na.rm = TRUE)
  }

  # MASE = mean(|test - forecast|) / scale
  mase <- mean(abs(actual - obj$mean), na.rm = TRUE) / max(scale, 1e-6)

  coverage <- as.numeric(coverage_score(obj, actual))
  winkler <- winkler_score2(obj, actual, level = level)

  res <- c(me, rmse, mae, mpe, mape, mase, coverage, winkler)
  names(res) <- c("me", "rmse", "mae", "mpe", "mape", "mase", "coverage", "winkler")

  return(res)
}
