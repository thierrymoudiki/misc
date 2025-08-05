#' Sequential or parallel for loop with robust error handling
#'
#' A flexible parallel/sequential loop implementation with progress tracking,
#' proper argument passing, and comprehensive error handling.
#'
#' @param what A function to apply to each element.
#' @param args A vector or list of arguments to iterate over. For functions with multiple
#'        arguments, provide a list where each element contains the arguments for one call.
#' @param cl Number of cores to use. If `NULL` (default), runs sequentially.
#'           If -1, uses all available cores. Positive integers specify exact core count.
#' @param combine A function to combine results (default: `c`). Must be a function.
#' @param errorhandling How to handle errors ("stop", "remove", or "pass").
#' @param verbose Logical; if `TRUE`, prints debugging information.
#' @param show_progress Logical; if `TRUE`, shows progress bar.
#' @param export Character vector of object names to export to workers (parallel only).
#' @param packages Character vector of package names to load on workers (parallel only).
#' @param cl_type Cluster type ("PSOCK", "FORK", etc). Default is "PSOCK" for cross-platform compatibility.
#' @param use_do_call Logical; if `TRUE`, treats each element of args as a list of arguments 
#'        to pass via do.call. If `FALSE`, passes each element as a single argument.
#' @param ... Additional arguments to pass to `what`.
#'
#' @return Results combined according to `combine` function.
#' @export
#'
#' @examples
#' # Basic usage - single arguments
#' parfor(function(x) x^2, 1:10)
#' 
#' # Parallel with 2 cores
#' parfor(function(x) x^2, 1:10, cl = 2)
#' 
#' # Multiple arguments using do.call style
#' args_list <- list(list(x=1, y=2), list(x=3, y=4), list(x=5, y=6))
#' parfor(function(x, y) x + y, args_list, use_do_call = TRUE)
#' 
#' # With additional fixed arguments
#' parfor(function(x, power) x^power, 1:5, power = 3)
#' 
#' # With error handling
#' parfor(function(x) if(x==3) stop("error") else x^2, 1:5, 
#'       errorhandling = "remove")
parfor <- function(what,
                   args,
                   cl = NULL,
                   combine = c,
                   errorhandling = c("stop", "remove", "pass"),
                   verbose = FALSE,
                   show_progress = TRUE,
                   export = NULL,
                   packages = NULL,
                   cl_type = "PSOCK",
                   use_do_call = FALSE,
                   ...) {
  
  # Validate inputs
  if (!is.function(what)) stop("'what' must be a function")
  if (!is.function(combine)) stop("'combine' must be a function")
  errorhandling <- match.arg(errorhandling)
  
  n_iter <- length(args)
  if (n_iter == 0) {
    warning("No arguments to iterate over")
    return(combine())
  }
  
  # Setup parallel backend if requested
  if (!is.null(cl)) {
    if (!requireNamespace("parallel", quietly = TRUE) || 
        !requireNamespace("doSNOW", quietly = TRUE)) {
      stop("Packages 'parallel' and 'doSNOW' required for parallel execution")
    }
    
    # Validate cluster parameters
    if (!is.numeric(cl) || length(cl) != 1 || cl < -1 || cl == 0) {
      stop("'cl' must be NULL, -1, or a positive integer")
    }
    
    # Determine core count
    available_cores <- parallel::detectCores()
    if (cl == -1) {
      cl <- available_cores
    } else if (cl > available_cores) {
      warning("Requested cores (", cl, ") exceeds available (", available_cores, ")")
      cl <- available_cores
    }
    
    # Create and register cluster
    cl_obj <- tryCatch({
      parallel::makeCluster(cl, type = cl_type)
    }, error = function(e) {
      stop("Failed to create cluster: ", e$message)
    })
    
    # Cleanup cluster on exit
    on.exit({
      try(parallel::stopCluster(cl_obj), silent = TRUE)
      if (exists("pb") && !is.null(pb)) try(close(pb), silent = TRUE)
    })
    
    doSNOW::registerDoSNOW(cl_obj)
    `%op%` <- foreach::`%dopar%`
    
    # Setup parallel progress bar
    if (show_progress) {
      pb <- utils::txtProgressBar(max = n_iter, style = 3)
      opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
    } else {
      opts <- NULL
    }
    
    # Export required packages
    if (!is.null(packages)) {
      parallel::clusterCall(cl_obj, function(pkgs) {
        for (pkg in pkgs) {
          suppressPackageStartupMessages(
            require(pkg, character.only = TRUE, quietly = TRUE)
          )
        }
      }, packages)
    }
    
  } else {
    # Sequential execution
    `%op%` <- foreach::`%do%`
    opts <- NULL
    if (show_progress) {
      pb <- utils::txtProgressBar(max = n_iter, style = 3)
    }
  }
  
  # Main processing loop using index-based iteration
  i <- NULL  # Avoid R CMD check NOTE
  result <- tryCatch({
    foreach::foreach(
      i = seq_len(n_iter),
      .combine = combine,
      .errorhandling = errorhandling,
      .options.snow = opts,
      .verbose = verbose,
      .export = export,
      .packages = packages
    ) %op% {
      # Update progress bar for sequential execution
      if (show_progress && is.null(cl)) {
        utils::setTxtProgressBar(pb, i)
      }
      
      # Apply function with appropriate argument handling
      if (use_do_call) {
        # Treat args[i] as a list of arguments to unpack
        if (is.list(args[[i]])) {
          do.call(what, c(args[[i]], list(...)))
        } else {
          do.call(what, c(list(args[[i]]), list(...)))
        }
      } else {
        # Pass args[i] as single argument
        what(args[[i]], ...)
      }
    }
  }, error = function(e) {
    stop("Loop execution failed: ", e$message)
  })
  
  # Cleanup progress bar for sequential execution
  if (show_progress && is.null(cl) && exists("pb")) {
    close(pb)
  }
  
  return(result)
}