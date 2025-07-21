
#' Get next date for a time series object
#' 
#' @param y A time series object
#' 
#' @export
#' @examples
#' 
#' get_next_date(AirPassengers)
#' get_next_date(lynx)
#' get_next_date(USAccDeaths)
#' 
get_next_date <- function(y)
{
  out <- list()
  out$start <- start(y)
  out$frequency <- frequency(y)
  out$end <- end(y)
  out$next_date <- tsp(y)[2] + 1 / frequency(y)
  return(out)
}

