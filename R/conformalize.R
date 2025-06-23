#' Conformal Prediction for Regression Models
#' 
#' This function implements conformal prediction for regression models using a split conformal approach.
#' 
#' @param formula A formula specifying the model to be fitted.
#' @param x A numeric matrix of predictors.
#' @param y A numeric vector of responses.
#' @param data A data frame containing the variables in the formula.
#' @param fit_func A function to fit the model. It should take a formula and data as arguments.
#' @param predict_func A function to make predictions. It should take a fitted model and new data as arguments.
#' @param split_ratio The proportion of data to use for training. Default is 0.5.
#' @param seed An optional seed for reproducibility.
#' @return A list containing the fitted model, residuals, and other relevant information.
#' @export
#' @examples
#' # Example usage with a linear model
#' library(MASS)
#' data(Boston)
#' set.seed(123)
#' fit_func <- function(formula, data) {
#'   lm(formula, data = data)
#' }
#' predict_func <- function(fit, newdata) {
#'   predict(fit, newdata)
#' }
#' conformalize(formula = medv ~ ., data = Boston, fit_func = fit_func,
#' predict_func = predict_func, split_ratio = 0.5, seed = 123)
#'
conformalize <- function(formula = NULL, x = NULL, y = NULL, data = NULL, 
                         fit_func, predict_func = predict, 
                         split_ratio = 0.5, seed = NULL, ...) {

  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)

  # Split the data into two parts
  if (!is.null(formula)) {
    if (is.null(data)) stop("Data must be provided when using formula interface.")
    if (!inherits(data, "data.frame")) stop("Data must be a data frame.")
  } else if (!is.null(x) && !is.null(y)) {
    # Remove the incorrect data check for matrix interface
    if (!is.matrix(x) || !is.numeric(y)) stop("x must be a numeric matrix and y must be a numeric vector.")
  } else {
    stop("Either formula or x and y must be provided.")
  }

  n <- if (!is.null(formula)) nrow(data) else nrow(x)
  split_index <- sample(seq_len(n), size = floor(split_ratio * n))  
  # Handle formula interface
  if (!is.null(formula)) {
    train_data <- data[split_index, ]
    cal_data <- data[-split_index, ]    
    # Fit the model on the training data
    fit <- fit_func(formula, data = train_data, ...)    
    # Predict on the calibration data
    cal_y <- cal_data[[all.vars(formula)[1]]]
    cal_pred <- predict_func(fit, cal_data, ...)
  } else if (!is.null(x) && !is.null(y)) {
    # Handle matrix interface
    train_x <- x[split_index, , drop = FALSE]
    train_y <- y[split_index]
    cal_x <- x[-split_index, , drop = FALSE]
    cal_y <- y[-split_index]    
    # Fit the model on the training data
    fit <- fit_func(train_x, train_y, ...)    
    # Predict on the calibration data
    cal_pred <- predict_func(fit, cal_x, ...)
  } else {
    stop("Either formula or x and y must be provided.")
  }  
  # Calculate residuals
  #misc::debug_print(cal_pred)
  #misc::debug_print(cal_y)
  residuals <- cal_y - cal_pred
  sd_residuals <- sd(residuals)
  scaled_residuals <- mean(residuals)/sd_residuals  
  # Return the model fit and residuals
  out <- list(fit = fit, 
  residuals = residuals, 
  sd_residuals = sd_residuals, 
  scaled_residuals = scaled_residuals)
  class(out) <- "conformalize"
  return(out)
}

#' Predict method for conformalize objects
#' 
#' This function generates prediction intervals for new data using the fitted conformal model.
#' 
#' @param object A conformalize object.
#' @param newdata A data frame or matrix of new data for prediction.
#' @param level The confidence level for the prediction intervals. Default is 0.95.
#' @param method The method to use for prediction intervals. Options are "split" or "simulation".
#' @param n_sim The number of simulations to perform if using the "simulation" method. Default is 1000.
#' @param seed An optional seed for reproducibility.
#' @param ... Additional arguments to pass to the predict function.
#' @return A matrix with predictions and prediction intervals.
#' @export
#' # Define fit and predict functions
#' fit_func <- function(formula, data, ...) stats::glm(formula, data = data, ...)
#' predict_func <- function(fit, newdata, ...) predict(fit, newdata, ...)
#' 
#' # Apply conformalize using the training data
#' conformal_model_boston <- misc::conformalize(
#'   formula = medv ~ .,
#'   data = train_data,
#'   fit_func = fit_func,
#'   predict_func = predict_func,
#'   seed = 123
#' )
#' 
#' # Predict with split conformal method on the test data
#' predictions_boston <- predict(
#'   conformal_model_boston,
#'   newdata = test_data,
#'   level = 0.95,
#'   method = "split"
#' )
#' 
#' head(predictions_boston)
predict.conformalize <- function(object, newdata, 
                                 predict_func = predict,
                                 level = 0.95, 
                                 method = c("splitconformal", "kde",
                                            "surrogate", "bootstrap"), 
                                 n_sim = 250L, seed = 123L, ...) {
  if (is.null(dim(newdata)))
  {
    newdata <- matrix(newdata, nrow = 1)
  }
  # Ensure the object is of class "conformalize"
  if (!inherits(object, "conformalize")) {
    stop("The object must be of class 'conformalize'.")
  }
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  # Extract components from the conformalize object
  fit <- object$fit
  residuals <- object$residuals
  sd_residuals <- object$sd_residuals
  scaled_residuals <- object$scaled_residuals
  # Generate predictions using the provided predict_func
  predictions <- predict_func(fit, newdata, ...)
  # Calculate prediction intervals
  method <- match.arg(method)
  misc::debug_print(object)
  if (method == "splitconformal") {
    # Split conformal prediction intervals
    alpha <- 1 - level
    q <- quantile(abs(residuals), probs = 1 - alpha)
    lower <- predictions - q
    upper <- predictions + q
  } else {
    # Simulation-based prediction intervals
    n_preds <- nrow(newdata)
    sim_residuals <- matrix(misc::direct_sampling(data = residuals, 
                                           n = n_sim*n_preds,
                                           method = method,
                                           seed = seed), 
                            nrow = n_preds,
                            ncol = n_sim)
    sim_predictions <- predictions + sim_residuals
    lower <- apply(sim_predictions, 1, quantile, probs = (1 - level) / 2)
    upper <- apply(sim_predictions, 1, quantile, probs = 1 - (1 - level) / 2)
  }
  # Return a data frame with predictions and intervals
  return(as.matrix(data.frame(
    fit = predictions,
    lwr = lower,
    upr = upper
  )))
}

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

#' Residuals method for conformalize objects
#' 
#' This function extracts the residuals from a conformalize object.
#' 
#' @param object A conformalize object.
#' @param ... Additional arguments (not used).
#' @return A numeric vector of residuals.
#' @export
#' 
residuals.conformalize <- function(object, ...) {
  # Ensure the object is of class "conformalize"
  if (!inherits(object, "conformalize")) {
    stop("The object must be of class 'conformalize'.")
  }
  # Return the residuals
  return(object$residuals)
}