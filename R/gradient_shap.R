#' Gradient-Based SHAP for Any Differentiable Function
#' 
#' This implements SHAP-like explanations using finite differences to approximate
#' gradients for any function f, extending the linear SHAP concept to nonlinear models.

#' Compute gradient-based SHAP values for any function
#'
#' @param f Function to explain (should accept matrix input, return vector output)
#' @param X_train Training data (for computing baseline and feature means)
#' @param X_new New observations to explain
#' @param h Step size for finite differences (default: 1e-5)
#' @param baseline_method Method for computing baseline: "mean", "median", "zero", or custom vector
#' @param scale_by_range Whether to scale gradients by feature ranges
#' @return List with gradient-SHAP values, baseline, and predictions
gradient_shap <- function(f, X_train, X_new, h = 1e-5, 
                          baseline_method = "mean", 
                          scale_by_range = FALSE) {
  
  # Convert to matrices if needed
  if (is.data.frame(X_train)) X_train <- as.matrix(X_train)
  if (is.data.frame(X_new)) X_new <- as.matrix(X_new)
  
  n_obs <- nrow(X_new)
  n_features <- ncol(X_new)
  
  # Compute baseline
  if (is.character(baseline_method)) {
    baseline <- switch(baseline_method,
                       "mean" = colMeans(X_train),
                       "median" = apply(X_train, 2, median),
                       "zero" = rep(0, n_features),
                       stop("Unknown baseline method")
    )
  } else {
    baseline <- baseline_method  # Custom baseline vector
  }
  
  # Get baseline prediction
  baseline_pred <- f(matrix(baseline, nrow = 1))
  
  # Get predictions for new observations
  predictions <- f(X_new)
  
  # Compute gradients using finite differences
  gradients <- compute_finite_difference_gradients(f, X_new, h)
  
  # Optional: Scale gradients by feature ranges for better attribution
  if (scale_by_range) {
    feature_ranges <- apply(X_train, 2, function(x) diff(range(x)))
    feature_ranges[feature_ranges == 0] <- 1  # Avoid division by zero
    gradients <- sweep(gradients, 2, feature_ranges, "*")
  }
  
  # Compute SHAP-like attributions: gradient * (x - baseline)
  attributions <- sweep(X_new, 2, baseline) * gradients
  
  # Validate decomposition (will be approximate due to nonlinearity)
  reconstructed <- baseline_pred + rowSums(attributions)
  residuals <- predictions - reconstructed
  
  list(
    attributions = attributions,
    gradients = gradients,
    baseline = baseline,
    baseline_prediction = baseline_pred,
    predictions = predictions,
    residuals = residuals,
    max_residual = max(abs(residuals)),
    rmse_residual = sqrt(mean(residuals^2))
  )
}

#' Compute finite difference gradients
compute_finite_difference_gradients <- function(f, X, h) {
  n_obs <- nrow(X)
  n_features <- ncol(X)
  gradients <- matrix(0, nrow = n_obs, ncol = n_features)
  
  for (i in 1:n_features) {
    # Create perturbed matrices
    X_plus <- X_minus <- X
    X_plus[, i] <- X_plus[, i] + h
    X_minus[, i] <- X_minus[, i] - h
    
    # Compute finite difference: (f(x+h) - f(x-h)) / (2*h)
    f_plus <- f(X_plus)
    f_minus <- f(X_minus)
    gradients[, i] <- (f_plus - f_minus) / (2 * h)
  }
  
  colnames(gradients) <- colnames(X)
  gradients
}

#' Integrated Gradients for Model Explanations
#'
#' Computes feature attributions using the Integrated Gradients method.
#' This integrates gradients along the straight-line path from a baseline to each input.
#'
#' @param f Function to explain (should accept matrix input, return vector output)
#' @param X_train Training data (for computing baseline)
#' @param X_new New observations to explain
#' @param baseline_method Method for computing baseline: "mean", "median", or "zero"
#' @param n_steps Number of steps for the integration path (default: 50)
#' @param h Step size for finite differences (default: 1e-5)
#' @return List with attributions, integrated gradients, baseline, predictions, and residual diagnostics
#' @examples
#' # integrated_gradients(f, X_train, X_new)
#' @export
integrated_gradients <- function(f, X_train, X_new, 
                                 baseline_method = "mean", 
                                 n_steps = 50, 
                                 h = 1e-5) {
  
  if (is.data.frame(X_train)) X_train <- as.matrix(X_train)
  if (is.data.frame(X_new)) X_new <- as.matrix(X_new)
  
  # Compute baseline
  baseline <- switch(baseline_method,
                     "mean" = colMeans(X_train),
                     "median" = apply(X_train, 2, median),
                     "zero" = rep(0, ncol(X_train))
  )
  
  n_obs <- nrow(X_new)
  n_features <- ncol(X_new)
  
  # Initialize integrated gradients
  integrated_grads <- matrix(0, nrow = n_obs, ncol = n_features)
  
  # Integrate gradients along straight line path from baseline to input
  for (step in seq(0, 1, length.out = n_steps)) {
    # Interpolated point along path
    X_interp <- sweep(X_new, 2, baseline) * step + 
      matrix(baseline, nrow = n_obs, ncol = n_features, byrow = TRUE)
    
    # Compute gradients at interpolated point
    grads <- compute_finite_difference_gradients(f, X_interp, h)
    
    # Accumulate (integrate)
    integrated_grads <- integrated_grads + grads / n_steps
  }
  
  # Scale by path length: (x - baseline)
  attributions <- sweep(X_new, 2, baseline) * integrated_grads
  
  # Get predictions
  baseline_pred <- f(matrix(baseline, nrow = 1))
  predictions <- f(X_new)
  reconstructed <- baseline_pred + rowSums(attributions)
  residuals <- predictions - reconstructed
  
  list(
    attributions = attributions,
    integrated_gradients = integrated_grads,
    baseline = baseline,
    baseline_prediction = baseline_pred,
    predictions = predictions,
    residuals = residuals,
    max_residual = max(abs(residuals)),
    rmse_residual = sqrt(mean(residuals^2))
  )
}

#' LIME-style local linear approximation using gradients
local_linear_shap <- function(f, X_train, X_new, 
                              n_samples = 1000, 
                              sigma = 0.1,
                              h = 1e-5) {
  
  if (is.data.frame(X_train)) X_train <- as.matrix(X_train)
  if (is.data.frame(X_new)) X_new <- as.matrix(X_new)
  
  n_obs <- nrow(X_new)
  n_features <- ncol(X_new)
  
  # For each observation, create local linear model
  attributions <- matrix(0, nrow = n_obs, ncol = n_features)
  baselines <- numeric(n_obs)
  
  for (i in 1:n_obs) {
    x_focal <- X_new[i, ]
    
    # Generate samples around focal point
    X_samples <- matrix(rnorm(n_samples * n_features, 
                              mean = rep(x_focal, each = n_samples), 
                              sd = sigma), 
                        nrow = n_samples, ncol = n_features)
    
    # Get predictions for samples
    y_samples <- f(X_samples)
    
    # Fit local linear model
    X_samples_centered <- sweep(X_samples, 2, x_focal)
    local_model <- lm(y_samples ~ X_samples_centered)
    
    # Extract coefficients (gradients)
    coefs <- coef(local_model)
    baselines[i] <- coefs[1]  # Intercept is prediction at focal point
    local_gradients <- coefs[-1]  # Slopes are local gradients
    
    # Compute attributions relative to training mean
    baseline_global <- colMeans(X_train)
    attributions[i, ] <- local_gradients * (x_focal - baseline_global)
  }
  
  predictions <- f(X_new)
  baseline_pred <- mean(baselines)  # Average local baseline
  reconstructed <- baseline_pred + rowSums(attributions)
  residuals <- predictions - reconstructed
  
  list(
    attributions = attributions,
    local_baselines = baselines,
    baseline_prediction = baseline_pred,
    predictions = predictions,
    residuals = residuals,
    max_residual = max(abs(residuals)),
    rmse_residual = sqrt(mean(residuals^2))
  )
}