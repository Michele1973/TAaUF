\name{na_clean}
\alias{na_clean}
\title{NAs Cleaner or substitute with desired values}
\usage{
na_clean(x, value = 0)
}
\description{
Function to subsittute the NAs values with a desired new value
}
\arguments{
\item{x}{a vector containing NAs}

\item{value}{value to use for NA substitution}
}

\examples{
ret <- rnorm(10)
na_clean(ret,0)
}
