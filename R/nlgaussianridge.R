#' @export
calibmodel <- function(X, y, workhorse=stats::lm, lambda=0.1, seed=123) {
  set.seed(seed)  
  n_train <- floor(0.5 * nrow(X))
  y_mean <- mean(y)
  y <- y - y_mean
  train_idx <- sample(nrow(X), size=n_train)  
  train_set <- X[train_idx, ]
  cal_set <- X[-train_idx, ]
  y_train <- y[train_idx]
  y_cal <- y[-train_idx]
  
  # If workhorse is lm or glm, use ridge regression with augmented matrices
  if (identical(workhorse, stats::lm) || identical(workhorse, stats::glm)) {
    # Create augmented matrices for training set
    p <- ncol(train_set)
    sqrt_lambda <- sqrt(lambda)
    Z_train <- rbind(train_set, diag(sqrt_lambda, p, p))
    y_aug_train <- c(y_train, rep(0, p))
    Z_cal <- rbind(cal_set, diag(sqrt_lambda, p, p))
    y_aug_cal <- c(y_cal, rep(0, p))
    
    # Create data frames with augmented data
    df_train <- data.frame(y = y_aug_train, as.data.frame(Z_train))
    df_cal <- data.frame(y = y_aug_cal, as.data.frame(Z_cal))
    
    # Fit models using lm
    model_train <- lm(y ~ . - 1, data = df_train)  # -1 to remove intercept
    model_cal <- lm(y ~ . - 1, data = df_cal)
    
    # Store original data
    model_train$x <- train_set
    model_train$y <- y_train
    model_cal$x <- cal_set
    model_cal$y <- y_cal
    
  } else {
    # Original code for other workhorse functions
    df_train <- data.frame(train_set, y=y_train)
    df_cal <- data.frame(cal_set, y=y_cal)  
    model_train <- try(workhorse(y ~ ., data=df_train), silent=TRUE)
    if (inherits(model_train, "try-error")) {
      model_train <- workhorse(X = as.matrix(train_set), y = y_train)
    }
    model_cal <- try(workhorse(y ~ ., data=df_cal), silent=TRUE)
    if (inherits(model_cal, "try-error")) {
      model_cal <- workhorse(X = as.matrix(cal_set), y = y_cal)
    }
  }
  
  # Predict on calibration set
  pred_cal <- try(predict(model_train, newdata=cal_set, interval="prediction"), silent=TRUE)
  if (inherits(pred_cal, "try-error") || any(is.nan(pred_cal))) {
    # Fall back to point predictions with empirical intervals
    pred_fit <- as.vector(cal_set %*% model_train$coefficients)
    pred_cal <- matrix(0, nrow=length(pred_fit), ncol=3)
    colnames(pred_cal) <- c("fit", "lwr", "upr")
    pred_cal[,"fit"] <- pred_fit
    sigma <- sqrt(mean(model_train$residuals^2))
    pred_cal[,"lwr"] <- pred_fit - 2 * sigma
    pred_cal[,"upr"] <- pred_fit + 2 * sigma
  }
  
  # Calculate calibrated residuals
  calibrated_residuals <- list(
    lower = y_cal - pred_cal[,"lwr"],
    upper = y_cal - pred_cal[,"upr"]
  )
  model_cal$calibrated_residuals <- calibrated_residuals
  model_cal$y_mean <- y_mean
  class(model_cal) <- c("calibmodel", "lm")
  return(model_cal)
}

# Prediction method
#' @export
predict.calibmodel <- function(object, newdata, interval="prediction", ...) {
  # Convert matrix/array to data frame if necessary
  if (!is.data.frame(newdata)) {
    newdata <- as.data.frame(newdata)
    colnames(newdata) <- colnames(object$model)[-1]  # Exclude response column name
  }
  
  # Standard prediction
  pred <- NextMethod("predict", object, newdata=newdata, interval=interval, ...)
  pred <- pred + object$y_mean
  # If prediction interval is requested and calibrated residuals exist
  if (interval == "prediction" && !is.null(object$calibrated_residuals)) {
    # Adjust prediction intervals
    mean_lower_res <- mean(object$calibrated_residuals$lower, na.rm=TRUE)
    mean_upper_res <- mean(object$calibrated_residuals$upper, na.rm=TRUE)    
    pred[,"lwr"] <- pred[,"lwr"] + mean_lower_res + object$y_mean
    pred[,"upr"] <- pred[,"upr"] + mean_upper_res + object$y_mean
  }  
  return(pred)
}

#' @export
calibmodel.formula <- function(formula, data, seed=123) {
  # Extract X matrix and y vector from formula and data
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)[,-1, drop=FALSE]  # Remove intercept column
  
  # Call the original caliblm function
  result <- calibmodel(X, y, seed=seed)
  
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
