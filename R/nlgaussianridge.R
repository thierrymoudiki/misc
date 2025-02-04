#' @export
ridgemodel <- function(X, y, workhorse=stats::lm, lambda=0.01, 
level=95, seed=123, ...) {
    set.seed(seed)  
    n_train <- floor(0.5 * nrow(X))
    y_mean <- mean(y)
    X_mean <- colMeans(X)
    X_sd <- apply(X, 2, sd)
    X_scaled <- scale(X, center=X_mean, scale=X_sd)
    y <- y - y_mean
    p <- ncol(X)
    sqrt_lambda <- sqrt(lambda)
    Z <- rbind(X_scaled, diag(sqrt_lambda, p, p))
    y_aug <- c(y, rep(0, p))
    # Create data frame with augmented data
    df <- data.frame(y = y_aug, as.data.frame(Z))
  # If workhorse is lm or glm, use ridge regression with augmented matrices
  if (identical(workhorse, stats::lm) || identical(workhorse, stats::glm)) {
        # Original code for other workhorse functions 
        model <- try(workhorse(y ~ . -1, data=df, ...), silent=TRUE)
        if (inherits(model, "try-error")) {
            model <- workhorse(X = as.matrix(X), y = y, ...)
        }
    }
  model$y_mean <- y_mean
  model$X_mean <- X_mean
  model$X_sd <- X_sd
  model$level <- level
  class(model) <- c("ridgemodel", "lm")
  return(model)
}

#' @export
predict.ridgemodel <- function(object, newdata, ...) {
  # Convert newdata to matrix if it isn't already
  if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
  # Check dimensions
  if (length(object$X_mean) != ncol(newdata)) {
    stop("Number of variables in newdata (", ncol(newdata), 
         ") must match the training data (", length(object$X_mean), ")")
  }
  # Scale new data using training scaling parameters
  newdata_scaled <- scale(newdata, 
                         center = object$X_mean,
                         scale = object$X_sd)
  # Make predictions
  pred <- stats::predict.lm(object, as.data.frame(newdata_scaled), ...) + object$y_mean
  return(drop(pred))
  
}

#' @export
simulate.ridgemodel <- function(object, newdata, nsim = 100L, seed = 123, ...) {
  set.seed(seed)  
  # Get predictions for new data
  fitted_values <- predict.ridgemodel(object, newdata = newdata)
  # Get the residual standard error from the model
  sigma <- sqrt(sum(object$residuals^2) / object$df.residual)
  # Generate random normal errors
  errors <- matrix(rnorm(length(fitted_values) * nsim, 
                        mean = 0, 
                        sd = sigma), 
                  nrow = length(fitted_values), 
                  ncol = nsim)
  # Add errors to fitted values to create simulations
  result <- errors + fitted_values
  # Convert to data.frame to match simulate.lm output format
  result <- as.data.frame(result)
  names(result) <- paste0("sim_", 1:nsim)
  return(result)
}

#' @export
ridgemodel.formula <- function(formula, data, workhorse=stats::lm, 
lambda=0.1, seed=123, ...) {
  # Extract X matrix and y vector from formula and data
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)[,-1, drop=FALSE]  # Remove intercept column  
  # Call the original caliblm function
  result <- misc::ridgemodel(X, y, workhorse=workhorse, 
  lambda=lambda, seed=seed, ...)  
  # Add formula-related attributes
  result$call <- match.call()
  result$terms <- terms(formula, data=data)
  result$model <- mf  
  return(result)
}

#' @export
nlgaussianridge <- function(X, y, lambda, n_hidden_features=5L, 
                           activation_function=c("relu", "sigmoid", "tanh"),
                           coeff=NULL, workhorse=NULL) {
    activation_function <- match.arg(activation_function)
    scaled_X <- scale(X)
    y_mean <- mean(y)
    centered_y <- y - y_mean    
    # Create augmented features if needed
    if (n_hidden_features > 0) {
        if (activation_function == "relu") {
            hidden_features <- pmax(scaled_X, 0)
        }
        else if (activation_function == "sigmoid") {
            hidden_features <- ifelse(scaled_X >= 0,
                                    1 / (1 + exp(-scaled_X)),
                                    exp(scaled_X) / (1 + exp(scaled_X)))
        }
        else if (activation_function == "tanh") {
            hidden_features <- tanh(scaled_X)            
        }
        scaled_X <- cbind(scaled_X, hidden_features)
    }    
    # If workhorse is lm or glm, use ridge regression formula
    if (is.null(workhorse) || inherits(workhorse, "lm") || inherits(workhorse, "glm")) {
        # Ridge regression solution: (X'X + λI)^(-1)X'y
        XtX <- crossprod(scaled_X)
        p <- ncol(XtX)
        ridge_term <- lambda * diag(p)
        Xty <- crossprod(scaled_X, centered_y)
        
        # Solve ridge regression
        coeff <- solve(XtX + ridge_term, Xty)
        
        # Create a list with necessary components
        result <- list(
            coefficients = coeff,
            x = scaled_X,
            y = centered_y,
            fitted.values = scaled_X %*% coeff,
            lambda = lambda,
            y_mean = y_mean,
            scaling = list(
                center = attr(scaled_X, "scaled:center"),
                scale = attr(scaled_X, "scaled:scale")
            )
        )
        class(result) <- c("nlgaussianridge", "lm")
        return(result)
    } else {
        # Use provided workhorse function
        df_covariates <- data.frame(X=scaled_X, y=centered_y)
        return(workhorse(X=scaled_X, y=centered_y))
    }
}

#' @export
predict.nlgaussianridge <- function(object, newdata, ...) {
    # Scale new data using training scaling parameters
    if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
    scaled_newdata <- scale(newdata, 
                          center = object$scaling$center,
                          scale = object$scaling$scale)
    
    # Make predictions
    pred <- scaled_newdata %*% object$coefficients + object$y_mean
    return(drop(pred))
}
