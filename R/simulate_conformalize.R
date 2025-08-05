#' simulate method for conformalize objects
#' 
#' This function generates simulations for new data using the fitted conformal model.
#' 
#' @param object A object of class \code{conformalize}.
#' @param newdata A data frame or matrix of new data for prediction.
#' @param method The method to use for prediction intervals. Options are "split" or "simulation".
#' @param n_sim The number of simulations to perform if using the "simulation" method. Default is 1000.
#' @param seed An optional seed for reproducibility.
#' @param ... Additional arguments to pass to the predict function.
#' @return A matrix with predictions and prediction intervals.
#' @export
#' 
simulate.conformalize <- function(object, 
                                  newdata, 
                                  method = c("kde", 
                                             "surrogate", 
                                             "bootstrap"), 
                                  n_sim = 250L, 
                                  seed = NULL, 
                                  ...) {
  # Ensure the object is of class "conformalize"
  if (!inherits(object, "conformalize")) {
    stop("The object must be of class 'conformalize'.")
  }
  method <- match.arg(method)
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  # Extract components from the conformalize object
  fit <- object$fit
  residuals <- object$residuals
  sd_residuals <- object$sd_residuals
  scaled_residuals <- object$scaled_residuals
  # Simulation-based prediction intervals
  n_preds <- nrow(newdata)
  # Generate predictions using the provided predict_func
  predictions <- predict(fit, newdata, ...)
  sim_residuals <- matrix(misc::direct_sampling(data = residuals, 
                                                n = n_sim*n_preds,
                                                method = method,
                                                seed = seed), 
                          nrow = n_preds,
                          ncol = n_sim)
  return(predictions + sim_residuals)
}
