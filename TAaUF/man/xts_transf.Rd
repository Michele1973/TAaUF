\name{xts_transf}
\alias{xts_transf}
\title{Transform a numeric object into a xts one}
\usage{
xts_transf(x, year = 2018, month = 02, day = 2, hour = 11, minutes = 00, period = 60)
}
\description{
Transform a numeric or a common non xts object into an xts object with specified period, starting from the desired year, month, day
}
\arguments{
\item{x}{a vector, matrix or data.frame object}

\item{year}{starting year}

\item{month}{starting month}

\item{day}{starting day}

\item{hour}{starting hour}

\item{minutes}{starting minutes}

\item{period}{the desired period, 60 indicates a 1 minute interval time, 300 indicates a 5 minutes time frame...}

}
\examples{
# This will create a 5 minutes xts object
x <- rnorm(50)
xts_transf(x, period = 300)
}

