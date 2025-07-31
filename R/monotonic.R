#' Monotonic Regression using XGBoost, LightGBM, or SCAM
#'
#' @param x Numeric vector or matrix of predictor variables
#' @param y Numeric vector of response variable
#' @param method Either "xgboost", "lightgbm", or "scam"
#' @param monotonicity 1 for non-decreasing, -1 for non-increasing
#' @param prediction A boolean; obtain predictions on training set (TRUE) or return model object
#' @param strict If TRUE, enforces strict monotonicity (for xgboost/lightgbm only; scam is always strict)
#' @param ... Additional parameters passed to the underlying algorithm
#' @return A list with fitted values and model object
#' @export
#' @examples
#' # Strict decreasing example with xgboost
#' x <- 1:5
#' y <- c(0.98, 0.99, 0.97, 0.95, 0.8)
#' result <- monotonic_regression(x, y, method = "xgboost", monotonicity = -1)
#' print(result$fitted)
monotonic_regression <- function(x, y, method = c("xgboost", "lightgbm", "scam"), 
                                 monotonicity = 1, prediction = TRUE, strict = FALSE, ...) {
  method <- match.arg(method)
  
  if (!monotonicity %in% c(-1, 1)) {
    stop("monotonicity must be 1 (non-decreasing) or -1 (non-increasing)")
  }
  
  # Get common parameters from ... or use defaults
  dots <- list(...)
  max_depth <- if ("max_depth" %in% names(dots)) dots$max_depth else 3
  
  if (method %in% c("xgboost", "lightgbm")) {
    eta <- if ("eta" %in% names(dots)) dots$eta else 0.1
    
    if (method == "xgboost") {
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop("xgboost package required but not installed")
      }
      
      dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x), label = y)
      
      params <- list(
        objective = "reg:squarederror",
        monotone_constraints = monotonicity,
        eta = eta,
        max_depth = max_depth
      )
      params <- c(params, dots[!names(dots) %in% c("eta", "max_depth")])
      
      model <- xgboost::xgb.train(params, dtrain, nrounds = 100)
      if (prediction) fitted <- predict(model, dtrain)
      
    } else { # lightgbm
      if (!requireNamespace("lightgbm", quietly = TRUE)) {
        stop("lightgbm package required but not installed")
      }
      
      dtrain <- lightgbm::lgb.Dataset(data = as.matrix(x), label = y)
      
      params <- list(
        objective = "regression",
        monotone_constraints = monotonicity,
        learning_rate = eta,
        max_depth = max_depth
      )
      params <- c(params, dots[!names(dots) %in% c("eta", "max_depth")])
      
      model <- lightgbm::lgb.train(params, dtrain, nrounds = 100)
      if (prediction) fitted <- predict(model, as.matrix(x))
    }
    
    # Post-hoc strict enforcement for tree methods
    if (prediction && strict) {
      fitted <- if (monotonicity == 1) cummax(fitted) else cummin(fitted)
    }
    
  } else { # SCAM method
    if (!requireNamespace("scam", quietly = TRUE)) {
      stop("scam package required but not installed")
    }
    
    df <- data.frame(x = x, y = y)
    bs_type <- if (monotonicity == 1) "mpi" else "mpd"  # monotonic increasing/decreasing
    
    formula <- if (is.matrix(x)) {
      # Multivariate case (not fully supported by scam)
      stop("SCAM method currently only supports univariate x")
    } else {
      stats::as.formula(paste("y ~ s(x, bs = '", bs_type, "')", sep = ""))
    }
    
    model <- scam::scam(formula, data = df, ...)
    if (prediction) fitted <- predict(model, df)
  }
  
  # Verify monotonicity if predictions were made
  if (prediction) {
    if (monotonicity == 1) {
      if (!all(diff(fitted) >= 0)) {
        if (method == "scam") warning("SCAM fit failed to enforce monotonicity - check model")
        else if (!strict) warning("Non-decreasing constraint not fully enforced - set strict=TRUE")
      }
    } else {
      if (!all(diff(fitted) <= 0)) {
        if (method == "scam") warning("SCAM fit failed to enforce monotonicity - check model")
        else if (!strict) warning("Non-increasing constraint not fully enforced - set strict=TRUE")
      }
    }
    return(list(fitted = fitted, model = model))
  }
  return(model)
}