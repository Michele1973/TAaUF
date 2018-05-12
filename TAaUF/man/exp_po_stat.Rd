\name{exp_po_stat}
\alias{exp_po_stat}
\title{Profit Factor and Expected Payoff}
\usage{
exp_po_stat (ret, signals, roll = TRUE, digits = 5, commiss=TRUE, spread=0.0001)
}
\description{
This function is meant to calculate the strategy payoff and expected payoff plus the hit ratio and the total trades occured in a strategy.
}
\arguments{
\item{ret}{a xts object vector of trading returns}

\item{signals}{a xts object vector containing all long/short signals.}

\item{roll}{Setting on "TRUE" for rolling purposes, you are given the extract payoff and expected payoff. Setting on "FALSE" you are given all statistica such as max drawdown, total returns, Profit Factor, Expected Payoff, hit ratio, trades total number and the win percetnage rate. (\code{default "TRUE"}).}

\item{digits}{digits numbers to use in output statistics}

\item{commiss}{Set on "TRUE" to take account of comissions calculated as spread on every single trade.}

\item{spread}{spread commisison to calculate when "commiss"" is set to TRUE.It subtract the spread cost value to each trade.}
}

\examples{
exp_po_stat (rnorm(500), roll=TRUE, digits=5)
}
