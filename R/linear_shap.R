#' Fit a linear model and compute SHAP values with response centering
#'
#' @param X Training feature matrix or data frame
#' @param y Training response vector
#' @param newx New feature matrix or data frame for prediction/explanation
#' @param model_type Type of model: "lm", "rlm", "rq", "glmnet"
#' @param center_response Whether to center the response variable
#' @param account_correlations Whether to use correlation-aware SHAP
#' @param n_samples Number of samples for correlation-aware computation
#' @param lambda Regularization parameter for glmnet
#' @param tau Quantile for quantile regression
#' @param alpha Elastic net mixing parameter for glmnet
#' @return List with model, predictions, SHAP values, and validation results
#' @export
fit_predict_shap <- function(X, y, newx, 
                             model_type = "lm",
                             center_response = TRUE,
                             account_correlations = FALSE,
                             n_samples = 100,
                             lambda = 0.01,
                             tau = 0.5,
                             alpha = 1) {
  
  # Convert to data frames if needed
  if (is.matrix(X)) X <- as.data.frame(X)
  if (is.matrix(newx)) newx <- as.data.frame(newx)
  
  # Ensure column names match
  if (!all(names(newx) %in% names(X))) {
    stop("Column names in newx must match those in X")
  }
  newx <- newx[, names(X), drop = FALSE]
  
  # Center response if requested
  y_mean <- 0
  y_centered <- y
  if (center_response) {
    y_mean <- mean(y, na.rm = TRUE)
    y_centered <- y - y_mean
  }
  
  # Fit the model
  model <- fit_linear_model(X, y_centered, model_type, lambda, tau, alpha)
  
  # Compute SHAP values
  shap_result <- compute_linear_shap(model, X, newx, y_centered, 
                                     account_correlations, n_samples)
  
  # Get predictions (add back response mean if centered)
  predictions <- shap_result$predictions + y_mean
  
  # Adjust base value for response centering
  base_value <- shap_result$base_value + y_mean
  
  # Validate SHAP decomposition
  validation <- validate_shap_decomposition(shap_result$shap_values, 
                                            base_value, predictions)
  
  list(
    model = model,
    predictions = predictions,
    shap_values = shap_result$shap_values,
    base_value = base_value,
    feature_means = shap_result$feature_means,
    response_mean = y_mean,
    model_type = model_type,
    validation = validation,
    account_correlations = account_correlations
  )
}

#' Fit linear model based on specified type
fit_linear_model <- function(X, y, model_type, lambda, tau, alpha) {
  switch(model_type,
         "lm" = {
           df <- cbind(y = y, X)
           stats::lm(y ~ ., data = df)
         },
         "rlm" = {
           df <- cbind(y = y, X)
           MASS::rlm(y ~ ., data = df)
         },
         "rq" = {
           df <- cbind(y = y, X)
           quantreg::rq(y ~ ., data = df, tau = tau)
         },
         "glmnet" = {
           X_matrix <- model.matrix(~ . - 1, data = X)
           model <- glmnet::glmnet(X_matrix, y, alpha = alpha, lambda = lambda)
           # Store lambda in model for later use
           model$used_lambda <- lambda
           model
         },
         stop(paste("Unsupported model type:", model_type))
  )
}

#' Compute SHAP values for linear models
#' @export
compute_linear_shap <- function(model, X, newx, y, account_correlations, n_samples) {
  model_type <- get_model_type(model)
  
  # Extract coefficients and intercept
  coef_info <- extract_coefficients(model, model_type)
  
  # Calculate feature means from training data
  feature_means <- colMeans(X)
  
  # Calculate base value: expected prediction at feature means
  # For linear models: base_value = intercept + sum(coef[i] * mean[i])
  base_value <- coef_info$intercept + sum(coef_info$coefs * feature_means)
  
  # Center new data
  newx_centered <- sweep(as.matrix(newx), 2, feature_means)
  
  # Get predictions first
  predictions <- get_model_predictions(model, newx, model_type)
  
  # For linear models, SHAP values should be exact
  if (account_correlations && model_type != "glmnet") {
    shap_values <- correlation_aware_shap(coef_info$coefs, newx_centered, 
                                          X, feature_means, n_samples)
  } else {
    # Standard interventional SHAP: coef[i] * (x[i] - mean[i])
    shap_values <- sweep(newx_centered, 2, coef_info$coefs, "*")
  }
  
  # Ensure column names are preserved
  colnames(shap_values) <- names(coef_info$coefs)
  
  list(
    shap_values = shap_values,
    base_value = base_value,
    predictions = predictions,
    feature_means = feature_means
  )
}

#' Extract coefficients and intercept from various model types
#' @export
extract_coefficients <- function(model, model_type) {
  switch(model_type,
         "lm" = ,
         "rlm" = ,
         "rq" = {
           coefs <- coef(model)
           intercept <- if("(Intercept)" %in% names(coefs)) coefs["(Intercept)"] else 0
           coefs <- coefs[names(coefs) != "(Intercept)"]
           list(coefs = coefs, intercept = as.numeric(intercept))
         },
         "glmnet" = {
           coefs <- as.numeric(coef(model, s = model$used_lambda))
           coef_names <- rownames(coef(model, s = model$used_lambda))
           names(coefs) <- coef_names
           intercept <- if("(Intercept)" %in% names(coefs)) coefs["(Intercept)"] else 0
           coefs <- coefs[names(coefs) != "(Intercept)"]
           list(coefs = coefs, intercept = as.numeric(intercept))
         }
  )
}

#' Get model type string
#' @export
get_model_type <- function(model) {
  if ("glmnet" %in% class(model)) return("glmnet")
  class(model)[1]
}

#' Get predictions from various model types
#' @export
get_model_predictions <- function(model, newx, model_type) {
  switch(model_type,
         "lm" = ,
         "rlm" = ,
         "rq" = as.numeric(predict(model, newdata = newx)),
         "glmnet" = {
           newx_matrix <- model.matrix(~ . - 1, data = newx)
           as.numeric(predict(model, newx = newx_matrix, s = model$used_lambda))
         }
  )
}

#' Correlation-aware SHAP computation using sampling approach
#' @export
correlation_aware_shap <- function(coefs, newx_centered, X_train, feature_means, n_samples) {
  n_obs <- nrow(newx_centered)
  n_features <- ncol(newx_centered)
  
  # Initialize SHAP matrix
  shap_values <- matrix(0, nrow = n_obs, ncol = n_features)
  colnames(shap_values) <- names(coefs)
  
  # Compute covariance matrix
  cov_matrix <- cov(X_train)
  
  # For each observation
  for (i in 1:n_obs) {
    current_x <- newx_centered[i, ] + feature_means
    
    # Sample-based approximation of conditional expectations
    feature_contributions <- rep(0, n_features)
    
    for (s in 1:n_samples) {
      # Sample a random training observation
      train_idx <- sample(nrow(X_train), 1)
      z <- as.numeric(X_train[train_idx, ])
      
      # For each feature, compute its contribution given correlations
      for (j in 1:n_features) {
        # Compute conditional expectation E[X_j | X_-j = z_-j]
        # This is approximated using the multivariate normal assumption
        other_features <- setdiff(1:n_features, j)
        
        if (length(other_features) > 0) {
          # Conditional mean: mu_j + Sigma_j,-j * Sigma_-j,-j^-1 * (z_-j - mu_-j)
          sigma_j_others <- cov_matrix[j, other_features, drop = FALSE]
          sigma_others <- cov_matrix[other_features, other_features, drop = FALSE]
          
          # Handle potential singularity
          tryCatch({
            sigma_others_inv <- solve(sigma_others)
            conditional_mean <- feature_means[j] + 
              sigma_j_others %*% sigma_others_inv %*% (z[other_features] - feature_means[other_features])
          }, error = function(e) {
            conditional_mean <<- feature_means[j]
          })
        } else {
          conditional_mean <- feature_means[j]
        }
        
        # SHAP contribution: coefficient * (actual_value - conditional_expectation)
        feature_contributions[j] <- feature_contributions[j] + 
          coefs[j] * (current_x[j] - conditional_mean) / n_samples
      }
    }
    
    shap_values[i, ] <- feature_contributions
  }
  
  shap_values
}

#' Validate SHAP decomposition
#' @export
validate_shap_decomposition <- function(shap_values, base_value, predictions) {
  shap_sums <- rowSums(shap_values)
  reconstructed <- base_value + shap_sums
  residuals <- predictions - reconstructed
  
  list(
    passed = all(abs(residuals) < 1e-6),  # More lenient for numerical precision
    max_residual = max(abs(residuals)),
    mean_residual = mean(abs(residuals)),
    residuals = residuals,
    shap_sums = shap_sums,
    reconstructed = reconstructed
  )
}

#' Print summary of SHAP results
#' @export
print_shap_summary <- function(result) {
  cat("=== SHAP Analysis Summary ===\n")
  cat("Model type:", result$model_type, "\n")
  cat("Response centered:", !is.null(result$response_mean) && result$response_mean != 0, "\n")
  cat("Correlation-aware:", result$account_correlations, "\n")
  cat("Base value:", round(result$base_value, 4), "\n")
  cat("Validation passed:", result$validation$passed, "\n")
  cat("Max residual:", format(result$validation$max_residual, scientific = TRUE), "\n")
  
  # Show decomposition check
  if (nrow(result$shap_values) <= 3) {
    cat("\nDecomposition check (first few obs):\n")
    cat("Predictions:", round(result$predictions[1:min(3, length(result$predictions))], 4), "\n")
    cat("Base + SHAP:", round(result$validation$reconstructed[1:min(3, length(result$validation$reconstructed))], 4), "\n")
    cat("Residuals:", round(result$validation$residuals[1:min(3, length(result$validation$residuals))], 4), "\n")
  }
  
  cat("\nFeature means:\n")
  print(round(result$feature_means, 4))
  cat("\nSHAP values (first few observations):\n")
  print(round(head(result$shap_values), 4))
}