\name{lines_strat}
\alias{lines_strat}
\title{Plots a Strategy line in existing plot}
\usage{
lines_strat(ret,col='black',na.action=na.omit, sum=TRUE)
}
\description{
Graphing a strategy returns curve
}
\arguments{
\item{ret}{a vector of trading returns}

\item{col}{colors (\code{default "black"} or any other color \code{"red,blue..."})}

\item{na.action}{default na.omit}

\item{sum}{a bool indicating whether to use \code{default "TRUE"} cumsum function or \code{default "FALSE"} for a cumprod function}
}
\examples{
lines_strat(rnorm(500))
}
