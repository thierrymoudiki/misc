#' Add together two numbers
#' 
#' @param x A number.
#' @param y A number.
#' @returns A numeric vector.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
parfor <- function(what,
                   args,
                   cl = NULL,
                   combine = c,
                   errorhandling = c("stop",
                                     "remove",
                                     "pass"),
                   verbose = FALSE,
                   show_progress = TRUE,
                   export = NULL,
                   ...)
{
  errorhandling <- match.arg(errorhandling)
  what <- compiler::cmpfun(what)
  
  n_iter <- length(args)
  
  if (!is.null(cl)) {
    # parallel
    stopifnot((floor(cl) == cl) && cl > -2)
    if (cl == -1)
    {
      cl_SOCK <-
        parallel::makeCluster(parallel::detectCores(), type = "SOCK")
    } else {
      cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    }
    doSNOW::registerDoSNOW(cl_SOCK)
    `%op%` <- foreach::`%dopar%`
  } else {
    # sequential
    `%op%` <- foreach::`%do%`
  }
  
  if (show_progress)
  {
    pb <- utils::txtProgressBar(min = 0,
                                max = n_iter,
                                style = 3)
    progress <- function(n) {
      utils::setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)
  } else {
    opts <- NULL
  }
  
  i <- NULL
  res <- foreach::foreach(
    i = 1:n_iter,
    .combine = combine,
    .errorhandling = errorhandling,
    .options.snow = opts,
    .verbose = verbose,
    .export = export
  ) %op% {
    if (identical(show_progress, TRUE))
    {
      utils::setTxtProgressBar(pb, i)
    }
    
    res <- do.call(what = what,
                   args = c(list(args[i]), ...))
    
    as.numeric(res)
  }
  
  if (show_progress)
  {
    close(pb)
  }
  
  if (!is.null(cl))
  {
    snow::stopCluster(cl_SOCK)
  }
  
  return(unlist(res))
}