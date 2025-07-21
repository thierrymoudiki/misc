


#' Plot Prediction Interval With Optional Past Observations
#'
#' This function creates a time series plot showing prediction intervals, including
#' optional past observations. The forecast is visualized with a confidence interval
#' band and mean prediction line.
#'

#' @param mean_pred Numeric vector of mean forecasted values. Required.
#' @param lower_pred Numeric vector of lower bounds of the prediction interval. Required.
#' @param upper_pred Numeric vector of upper bounds of the prediction interval. Required.
#' @param col_past Color for past observations. Default is `"black"`.
#' @param col_mean Color for the mean forecast line. Default is `"red"`.
#' @param col_ci Color for the confidence interval polygon. Default is `rgb(0.8, 0.8, 0.8, 0.5)`.
#' @param col_future Color for true future value. Default is `"blue"`.
#' @param legend_position Position of the legend. Passed to the `legend()` function.
#'   Default is `"topleft"`.
#' @param x_past Numeric vector of past observations. If `NULL`, the plot will contain
#'   only forecasted values. Default is `NULL`.
#' @param x_future Numeric vector of future observations. 
#' @param ... Additional arguments passed to the `plot()` function.
#'
#' @return No return value. Called for side effects (generates a plot).
#'
#' @examples
#' 
#' # Without past observations:
#' plot_prediction_interval(
#'   mean_pred = sin(1:10/3) + 1,
#'   lower_pred = sin(1:10/3) + 1 - 0.5,
#'   upper_pred = sin(1:10/3) + 1 + 0.5,
#'   main = "Forecast Without History"
#' )
#'
#' # With past observations:
#' plot_prediction_interval(
#'   x_past = sin(1:20/6),
#'   mean_pred = sin(21:30/6) + 0.1 * (1:10),
#'   lower_pred = sin(21:30/6) + 0.1 * (1:10) - 0.3,
#'   upper_pred = sin(21:30/6) + 0.1 * (1:10) + 0.3,
#'   main = "Forecast With History"
#' )
#'
#' # With past observations:
#' set.seed(123)
#' mean_pred <- sin(21:30/6) + 0.1 * (1:10)
#' plot_prediction_interval(
#'   x_past = sin(1:20/6),
#'   mean_pred = mean_pred,
#'   x_future = mean_pred + runif(length(mean_pred)),
#'   lower_pred = sin(21:30/6) + 0.1 * (1:10) - 0.3,
#'   upper_pred = sin(21:30/6) + 0.1 * (1:10) + 0.3,
#'   main = "Forecast With History"
#' )
#' 
#' @export
plot_prediction_interval <- function(    
    mean_pred,         # Mean predictions (required)
    lower_pred,        # Prediction lower bounds (required)
    upper_pred,        # Prediction upper bounds (required)
    col_past = "black", # Color for past observations
    col_mean = "red",  # Color for mean prediction
    col_future = "blue", # Color for true future value
    col_ci = rgb(0.8, 0.8, 0.8, 0.5), # Color for confidence interval
    legend_position = "topleft",
    x_past = NULL,          # Past observations (optional)
    x_future = NULL,     # Future observations (optional)
    ...                # Additional plot arguments
) {
  stopifnot((length(mean_pred) == length(lower_pred)) & (length(mean_pred) == length(upper_pred)))
  # Determine the x-axis indices
  n_pred <- length(mean_pred)
  if (!is.null(x_past)) {
    n_past <- length(x_past)
    x_vals <- 1:(n_past + n_pred)
  } else {
    x_vals <- 1:n_pred
    n_past <- 0
  }
  # Set up plot area
  y_range <- range(c(x_past, lower_pred, upper_pred), na.rm = TRUE)
  plot(NA, 
       xlim = range(x_vals), 
       ylim = y_range,
       xlab = ifelse("xlab" %in% names(list(...)), list(...)$xlab, "Time"),
       ylab = ifelse("ylab" %in% names(list(...)), list(...)$ylab, "Value"),
       ...)
  grid()
  # Plot past observations if provided
  if (!is.null(x_past)) {
    lines(1:n_past, x_past, col = col_past, lwd = 2)
    # Add vertical separator between past and future
    abline(v = n_past + 0.5, lty = 2, col = "gray")
  }
  # Plot prediction intervals (always)
  polygon(
    c(n_past + 1:n_pred, n_past + n_pred:1),
    c(lower_pred, rev(upper_pred)),
    col = col_ci, border = NA
  )
  # Plot mean prediction (always)
  lines(n_past + 1:n_pred, mean_pred, col = col_mean, lwd = 2)
  # Plot past observations if provided
  if (!is.null(x_future)) {
    stopifnot(length(mean_pred) == length(x_future))
    lines(n_past + 1:n_pred, x_future, col = col_future, lwd = 2)
  }
  # Add legend
  legend_items <- c("Mean prediction", "Prediction interval")
  legend_cols <- c(col_mean, col_ci)
  if (!is.null(col_future))
  {
    legend_items <- c(legend_items, "True future value")
    legend_cols <- c(legend_cols, col_future)
  }
  legend_lwds <- c(2, 10)
  if (!is.null(x_past)) {
    legend_items <- c("Past observations", legend_items)
    legend_cols <- c(col_past, legend_cols)
    legend_lwds <- c(2, legend_lwds)
  }  
  legend(legend_position, 
         legend = legend_items,
         col = legend_cols,
         lwd = legend_lwds,
         lty = 1,
         bty = "n")
}
