% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/winkler_score.R
\name{winkler_score}
\alias{winkler_score}
\title{Winkler score for probabilistic forecasts}
\usage{
winkler_score(actual, lower, upper, level = 95, scale = FALSE)
}
\arguments{
\item{actual}{numeric vector of actual values}

\item{lower}{numeric vector of lower bounds}

\item{upper}{numeric vector of upper bounds}

\item{level}{numeric level of confidence}

\item{scale}{logical, if TRUE, the score is scaled by the range of the bounds}
}
\value{
numeric score
}
\description{
Winkler score for probabilistic forecasts
}
\examples{

actual <- c(1, 2, 3, 4, 5)
lower <- c(0, 1, 2, 3, 4)
upper <- c(2, 3, 4, 5, 6)
winkler_score(actual, lower, upper)
winkler_score(actual, lower, upper, scale = TRUE)
winkler_score(actual, lower, upper, level = 99)
winkler_score(actual, lower, upper, level = 99, scale = TRUE)

}
