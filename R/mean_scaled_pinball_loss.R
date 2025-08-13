#' Mean Scaled Pinball Loss for Quantile Forecasts
#'
#' Calculates the Mean Scaled Pinball Loss (MSPL) for quantile forecasts,
#' scaled by the Mean Absolute Error (MAE) of the median forecast.
#'
#' @param y_true Numeric vector of observed values
#' @param y_pred Numeric matrix or data.frame of predicted quantiles
#'   (rows = observations, columns = quantiles)
#' @param probs Numeric vector of quantile levels (must be between 0 and 1)
#'  Default \code{c(0.005, 0.025, 0.165, 0.25, 0.5, 0.75, 0.835, 0.975, 0.995)}
#' @return Numeric scalar representing the MSPL
#' @export
#'
#' @examples
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- matrix(c(0.5, 1.5, 2.5, 3.5, 4.5,
#'                    1.0, 2.0, 3.0, 4.0, 5.0,
#'                    1.5, 2.5, 3.5, 4.5, 5.5), ncol = 3)
#' mspl(y_true, y_pred, probs=c(0.25, 0.5, 0.75))
mspl <- function(y_true, y_pred, probs = c(0.005, 0.025, 0.165,
                                           0.25, 0.5, 0.75,
                                           0.835, 0.975, 0.995)) {
  # Enhanced input validation
  if (!is.numeric(y_true) || !is.numeric(y_pred) || !is.numeric(probs)) {
    stop("All inputs must be numeric")
  }
  if (length(y_true) != nrow(y_pred)) {
    stop("Length of y_true (", length(y_true),
         ") must match rows in y_pred (", nrow(y_pred), ")")
  }
  if (length(probs) != ncol(y_pred)) {
    stop("Length of probs (", length(probs),
         ") must match columns in y_pred (", ncol(y_pred), ")")
  }
  if (any(probs < 0 | probs > 1)) {
    stop("All probs must be between 0 and 1")
  }
  if (!is.matrix(y_pred) && !is.data.frame(y_pred)) {
    stop("y_pred must be a matrix or data.frame")
  }

  # Convert to matrix if data.frame
  y_pred <- as.matrix(y_pred)

  # Find median column - optimized for both sorted and unsorted probs
  median_col <- which.min(abs(probs - 0.5))

  # Vectorized pinball loss in single operation
  diff <- y_true - y_pred
  pinball_loss <- diff * ifelse(diff >= 0, probs, probs - 1)

  # Calculate baseline MAE with error handling
  mae_baseline <- tryCatch(
    mean(abs(y_true - y_pred[, median_col])),
    error = function(e) {
      warning("MAE calculation failed: ", conditionMessage(e))
      NA_real_
    }
  )

  # Robust scaling handling
  if (is.na(mae_baseline) || mae_baseline < sqrt(.Machine$double.eps)) {
    warning("Invalid baseline MAE (", mae_baseline, ") - returning unscaled loss")
    return(mean(pinball_loss))
  }

  # Final scaled result
  mean(pinball_loss) / mae_baseline
}
