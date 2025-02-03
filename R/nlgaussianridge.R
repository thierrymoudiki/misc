calibmodel <- function(X, y, workhorse=stats::lm, seed=123) {
  set.seed(seed)  
  n_train <- floor(0.5 * nrow(X))
  train_idx <- sample(nrow(X), size=n_train)  
  train_set <- X[train_idx, ]
  cal_set <- X[-train_idx, ]
  y_train <- y[train_idx]
  y_cal <- y[-train_idx]
  
  df_train <- data.frame(train_set, y=y_train)
  df_cal <- data.frame(cal_set, y=y_cal)  
  # Train model 
  model_train <- try(workhorse(y ~ ., data=df_train), silent=TRUE)
  if (inherits(model_train, "try-error")) {
    model_train <- workhorse(X = as.matrix(train_set), y = y_train)
  }
  model_cal <- try(workhorse(y ~ ., data=df_cal), silent=TRUE)
  if (inherits(model_cal, "try-error")) {
    model_cal <- workhorse(X = as.matrix(cal_set), y = y_cal)
  }
  # Predict on calibration set with prediction interval
  pred_cal <- predict(model_train, newdata=df_cal, interval="prediction")  
  # Calculate calibrated residuals
  calibrated_residuals <- list(
    lower = y_cal - pred_cal[,"lwr"],
    upper = y_cal - pred_cal[,"upr"]
  )
  model_cal$calibrated_residuals <- calibrated_residuals
  class(model_cal) <- c("calibmodel", "lm")
  return(model_cal)
}

# Prediction method
predict.calibmodel <- function(object, newdata, interval="prediction", ...) {
  # Standard prediction
  pred <- NextMethod("predict", object, newdata=newdata, interval=interval, ...)  
  # If prediction interval is requested and calibrated residuals exist
  if (interval == "prediction" && !is.null(object$calibrated_residuals)) {
    # Adjust prediction intervals
    mean_lower_res <- mean(object$calibrated_residuals$lower, na.rm=TRUE)
    mean_upper_res <- mean(object$calibrated_residuals$upper, na.rm=TRUE)    
    pred[,"lwr"] <- pred[,"lwr"] + mean_lower_res
    pred[,"upr"] <- pred[,"upr"] + mean_upper_res
  }  
  return(pred)
}

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

nlgaussianridge <- function(X, y, lambda, n_hidden_features=5L, 
activation_function=c("relu", "sigmoid", "tanh"),
coeff=NULL, workhorse=NULL) {
    activation_function <- match.arg(activation_function)
    scaled_X <- scale(X)
    y_mean <- mean(y)
    centered_y <- y - y_mean
    if (n_hidden_features > 0) 
    {
        if (activation_function == "relu") {
            hidden_features <- pmax(scaled_X, 0)
        }
        else if (activation_function == "sigmoid") {
            # Use numerically stable sigmoid to prevent overflow
            hidden_features <- ifelse(scaled_X >= 0,
                                    1 / (1 + exp(-scaled_X)),
                                    exp(scaled_X) / (1 + exp(scaled_X)))
        }
        else if (activation_function == "tanh") {
            hidden_features <- tanh(scaled_X)            
        }
        scaled_X <- cbind(scaled_X, hidden_features)
    }
    df_covariates <- data.frame(X=scaled_X, y=centered_y)    
    
}
