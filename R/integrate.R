#' Numerical Integration Using Trapezoidal or Simpson's Rule
#'
#' Computes the definite integral of a function using either the trapezoidal rule 
#' or Simpson's rule for numerical approximation.
#'
#' @param f A function to be integrated (must be vectorized)
#' @param a Lower bound of integration (numeric)
#' @param b Upper bound of integration (numeric, must be > a)
#' @param n Number of subdivisions (default = 10000, must be ≥ 2)
#' @param method Integration method: "trapezoidal" or "simpson" (default)
#' @return The approximated value of the integral (numeric)
#' @export
#' @examples
#' # Integrate x^2 from 0 to 1 (exact answer = 1/3)
#' integrate(function(x) x^2, 0, 1)
#' 
#' # Compare methods
#' integrate(sin, 0, pi, method = "trapezoidal")
#' integrate(sin, 0, pi, method = "simpson")
#' 
#' # Vectorized function example
#' integrate(dnorm, -1.96, 1.96) # Approx 0.95
integrate <- function(f, a, b, n = 10000, method = "simpson") {
  # Validate inputs
  if (a >= b) stop("Integration bounds must satisfy a < b")
  if (n < 2) stop("n must be at least 2")
  
  # Create evaluation points
  x <- seq(a, b, length.out = n + 1)
  y <- f(x)
  
  # Select integration method
  if (method == "trapezoidal") {
    # Trapezoidal rule: (b-a)/2n * [f(x0) + 2f(x1) + ... + 2f(xn-1) + f(xn)]
    h <- (b - a)/n
    integral <- h * (0.5 * y[1] + sum(y[2:n]) + 0.5 * y[n + 1])
  } else if (method == "simpson") {
    # Simpson's rule requires even number of intervals
    if (n %% 2 != 0) n <- n + 1
    h <- (b - a)/n
    # Coefficients pattern: 1, 4, 2, 4, ..., 4, 1
    coef <- rep(c(4, 2), length.out = n - 1)
    coef <- c(1, coef, 1)
    integral <- h/3 * sum(coef * y)
  } else {
    stop("Method must be 'trapezoidal' or 'simpson'")
  }
  
  return(integral)
}