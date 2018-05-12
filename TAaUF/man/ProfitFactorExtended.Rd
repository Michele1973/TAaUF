\name{ProfitFactorExtended}
\alias{ProfitFactorExtended}
\title{Calculate Profit Factor extended format and Kelly Ratio}
\usage{
ProfitFactorExtended (ret, signals, kelly=FALSE, commiss=TRUE, spread=0.0001)
}
\description{
This is an extensive Profit Factor calculator that gives back many other coefficients to calculate the good/bad trading strategy profitability.
}
\arguments{
\item{ret}{a xts object vector of trading returns}

\item{signals}{an xts vector object containing Long/Short signals in -1/+1 format}

\item{commiss}{Set on "TRUE" to take account of comissions calculated as spread on every single trade.}

\item{spread}{spread commisison to calculate when commiss is set to TRUE.It subtract the spread cost value to each trade.}

}

\value{
Returns Profit Factor value, percentage of winner trades versus losers, average win/loser ratio, expectancy ratio, average trade, weighted average, profit factor breakeven, optimum win percetnage, expexcted drawdown and the pessimistic return ratio.
}

\references{
Evaluating Trading Systems by John Ehlers and Ric Way (http://www.mesasoftware.com/papers/SystemEvaluation.pdf).

Portfolio Management Formulas, Vince, Ralph (ISBN: 0-471-52756-4), (http://boards.fool.com/performance-measures-pessimistic-return-ratio-10846009.aspx?sort=postdate).  Known as Pessimistic Return Ratio (PRR).

}

\examples{
# buy below EMA sell when prices are above EMA strategy
x<-db[,'eur']
ma1<-TTR::EMA(x,21)
signals<-lag(ifelse(x>ma1,-1,ifelse(x<ma1,1,0)),1)
ret<-signals*TTR::ROC(x)
ProfitFactorExtended(ret, signals, kelly=FALSE, commiss = TRUE, spread = spread_calc(x,0.00007))
}
