\name{extract_trades}
\alias{extract_trades}
\title{Extract all Trades}
\usage{
extract_trades (ret, signals, commiss=TRUE, spread=0.0001)
}
\description{
This function gives back a list object containing all trading strategy trades.
}
\arguments{
\item{ret}{a xts object vector of trading returns}

\item{signals}{a xts object vector containing all long/short signals.}

\item{commiss}{Set on "TRUE" to take account of commissions calculated as spread on every single trade.}

\item{spread}{spread commissions to calculate when "commiss"" is set to TRUE. It subtract the spread cost value to each trade.}
}

\examples{
# buy below EMA sell when prices are above EMA strategy
x<-db[,'eur']
ma1<-TTR::EMA(x,21)
signals<-lag(ifelse(x>ma1,-1,ifelse(x<ma1,1,0)),1)
ret<-signals*TTR::ROC(x)
trades<-extract_trades(ret,signals,commiss = T,0.0001)
}
