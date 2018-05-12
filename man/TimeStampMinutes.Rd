\name{TimeStampMinutes}
\alias{TimeStampMinutes}
\title{Execute functions on Time Interval }
\usage{
TimeStampMinutes(minutes=5, level='TF1')
}
\description{
Useful in streaming and on-line function execution, given a time interval in minutes, it gives back a boolean value if the function can start to work again or false if the interval time is still not reached.
}
\arguments{
\item{minutes}{an integer numeric object indicating desired minutes in which the function has to work}

\item{level}{a character or numeric object naming the interval you are working with}
}

\examples{
## function that have to work every 2 and 5 minutes
check_2<-function(x='do something every 2 min'){print(x)}
check_5<-function(x='do something every 5 min'){print(x)}
## time is 19:00
## both functions print nothing
if(TimeStampMinutes(2,'TF1'))check_2()
if(TimeStampMinutes(5,'TF5'))check_5()
## time is 19:02
## the first functions print "do something every 2 min", the second one print nothing
if(TimeStampMinutes(2,'TF1'))check_2()
if(TimeStampMinutes(5,'TF5'))check_5()
## time is 19:05
## the first functions print "do something every 5 min"
if(TimeStampMinutes(5,'TF5'))check_5()
}
