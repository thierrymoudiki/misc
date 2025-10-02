#' Parallel For Loop with Progress Bar
#'
#' Execute a function over a list of arguments in parallel or sequentially,
#' with optional progress tracking and flexible result combination.
#'
#' @param what A function to apply to each element of \code{args}.
#' @param args A list of arguments to iterate over. Each element will be passed
#'   to \code{what}.
#' @param cl Number of cores to use for parallel execution. \code{NULL} (default)
#'   for sequential execution, \code{-1} to use all available cores, or a positive
#'   integer specifying the number of cores.
#' @param combine Method for combining results. Can be:
#'   \itemize{
#'     \item A string: \code{"list"} (default), \code{"c"}, \code{"cbind"},
#'           \code{"rbind"}, or arithmetic operators (\code{"+"}, etc.)
#'     \item A function: custom function taking \code{...} arguments
#'   }
#' @param errorhandling How to handle errors. One of:
#'   \itemize{
#'     \item \code{"stop"}: stop execution on first error (default)
#'     \item \code{"remove"}: remove failed iterations from results
#'     \item \code{"pass"}: return error objects for failed iterations
#'   }
#' @param verbose Logical. If \code{TRUE}, print verbose output from \code{foreach}.
#' @param show_progress Logical. If \code{TRUE} (default), display a progress bar.
#' @param export Character vector of variable names to export to parallel workers.
#' @param packages Character vector of package names to load on parallel workers.
#' @param cl_type Type of cluster to create. One of \code{"PSOCK"} (default),
#'   \code{"FORK"}, \code{"MPI"}, or \code{"NWS"}.
#' @param use_do_call Logical. If \code{TRUE}, use \code{do.call} to apply
#'   \code{what} to arguments. Useful when \code{args} contains lists of arguments.
#' @param ... Additional arguments passed to \code{what}.
#'
#' @return Combined results from applying \code{what} to each element of \code{args}.
#'   The structure depends on the \code{combine} parameter.
#'
#' @details
#' This function provides a unified interface for sequential and parallel iteration
#' with progress tracking. When \code{cl} is \code{NULL}, execution is sequential.
#' Otherwise, a parallel cluster is created using the \code{parallel} and
#' \code{doSNOW} packages.
#'
#' The \code{combine} parameter determines how results are aggregated:
#' \itemize{
#'   \item \code{"list"}: Keep each result as a separate list element (safest default)
#'   \item \code{"c"}: Concatenate results with \code{c()}
#'   \item \code{"rbind"}/\code{"cbind"}: Bind data frames or matrices by row/column
#'   \item Custom function: Provide your own combining logic
#' }
#'
#' @examples
#' # Sequential execution with list output
#' parfor(sqrt, list(1, 4, 9, 16))
#'
#' # Parallel execution on 2 cores, combine with c()
#' parfor(sqrt, list(1, 4, 9, 16), cl = 2, combine = "c")
#'
#' # Use do.call for functions expecting multiple arguments
#' args_list <- list(list(x = 1, y = 2), list(x = 3, y = 4))
#' parfor(function(x, y) x + y, args_list, use_do_call = TRUE)
#'
#' # Custom combine function
#' parfor(sqrt, list(1, 4, 9, 16), combine = sum)
#'
#' # Combine data frames by row
#' parfor(function(x) data.frame(x = x, y = x^2),
#'        list(1, 2, 3), combine = "rbind")
#'
#' @export
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom parallel detectCores makeCluster stopCluster clusterCall
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils txtProgressBar setTxtProgressBar
parfor <- function(what, args, cl = NULL, combine = "list",
                   errorhandling = c("stop", "remove", "pass"),
                   verbose = FALSE, show_progress = TRUE,
                   export = NULL, packages = NULL, cl_type = "PSOCK",
                   use_do_call = FALSE, ...) {

  if (!is.function(what))
    stop("'what' must be a function")

  errorhandling <- match.arg(errorhandling)
  n_iter <- length(args)

  if (n_iter == 0) {
    warning("No arguments to iterate over")
    return(list())
  }

  # Handle combine parameter - convert string to function or validate function
  if (is.character(combine)) {
    combine_func <- switch(combine,
                           "list" = NULL,  # Use foreach's default list behavior
                           "c" = c,
                           "cbind" = cbind,
                           "rbind" = rbind,
                           "+" = function(...) Reduce(`+`, list(...)),
                           "-" = function(...) Reduce(`-`, list(...)),
                           "*" = function(...) Reduce(`*`, list(...)),
                           "/" = function(...) Reduce(`/`, list(...)),
                           stop("Unknown combine method: '", combine,
                                "'. Use 'list', 'c', 'cbind', 'rbind', '+', '-', '*', '/', or provide a function"))
    # For "list", we'll use foreach's default which doesn't need .combine
    if (combine == "list") {
      use_default_combine <- TRUE
    } else {
      use_default_combine <- FALSE
      combine <- combine_func
    }
  } else if (!is.function(combine)) {
    stop("'combine' must be a function or one of: 'list', 'c', 'cbind', 'rbind', '+', '-', '*', '/'")
  } else {
    use_default_combine <- FALSE
  }

  if (!is.null(cl)) {
    if (!requireNamespace("parallel", quietly = TRUE) ||
        !requireNamespace("doSNOW", quietly = TRUE)) {
      stop("Packages 'parallel' and 'doSNOW' required for parallel execution")
    }

    if (!is.numeric(cl) || length(cl) != 1 || cl < -1 || cl == 0) {
      stop("'cl' must be NULL, -1, or a positive integer")
    }

    available_cores <- parallel::detectCores()

    if (cl == -1) {
      cl <- available_cores
    } else if (cl > available_cores) {
      warning("Requested cores (", cl, ") exceeds available (",
              available_cores, ")")
      cl <- available_cores
    }

    cl_obj <- tryCatch({
      parallel::makeCluster(cl, type = cl_type)
    }, error = function(e) {
      stop("Failed to create cluster: ", e$message)
    })

    on.exit({
      try(parallel::stopCluster(cl_obj), silent = TRUE)
      if (exists("pb") && !is.null(pb)) try(close(pb), silent = TRUE)
    })

    doSNOW::registerDoSNOW(cl_obj)
    `%op%` <- foreach::`%dopar%`

    if (show_progress) {
      pb <- utils::txtProgressBar(max = n_iter, style = 3)
      opts <- list(progress = function(n) utils::setTxtProgressBar(pb, n))
    } else {
      opts <- NULL
    }

    if (!is.null(packages)) {
      parallel::clusterCall(cl_obj, function(pkgs) {
        for (pkg in pkgs) {
          suppressPackageStartupMessages(require(pkg, character.only = TRUE,
                                                 quietly = TRUE))
        }
      }, packages)
    }
  } else {
    `%op%` <- foreach::`%do%`
    opts <- NULL

    if (show_progress) {
      pb <- utils::txtProgressBar(max = n_iter, style = 3)
    }
  }

  i <- NULL

  result <- tryCatch({
    if (use_default_combine) {
      # Use foreach without .combine for default list behavior
      foreach::foreach(i = seq_len(n_iter),
                       .errorhandling = errorhandling,
                       .options.snow = opts,
                       .verbose = verbose,
                       .export = export,
                       .packages = packages) %op% {

                         if (show_progress && is.null(cl)) {
                           utils::setTxtProgressBar(pb, i)
                         }

                         if (use_do_call) {
                           if (is.list(args[[i]])) {
                             do.call(what, c(args[[i]], list(...)))
                           } else {
                             do.call(what, c(list(args[[i]]), list(...)))
                           }
                         } else {
                           what(args[[i]], ...)
                         }
                       }
    } else {
      # Use foreach with .combine
      foreach::foreach(i = seq_len(n_iter),
                       .combine = combine,
                       .errorhandling = errorhandling,
                       .options.snow = opts,
                       .verbose = verbose,
                       .export = export,
                       .packages = packages) %op% {

                         if (show_progress && is.null(cl)) {
                           utils::setTxtProgressBar(pb, i)
                         }

                         if (use_do_call) {
                           if (is.list(args[[i]])) {
                             do.call(what, c(args[[i]], list(...)))
                           } else {
                             do.call(what, c(list(args[[i]]), list(...)))
                           }
                         } else {
                           what(args[[i]], ...)
                         }
                       }
    }
  }, error = function(e) {
    stop("Loop execution failed: ", e$message)
  })

  if (show_progress && is.null(cl) && exists("pb")) {
    close(pb)
  }

  return(result)
}
