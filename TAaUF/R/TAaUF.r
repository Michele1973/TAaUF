plot_strat<-function(ret,main='',col='black',na.action=na.omit, sum=TRUE){
				#x[is.na(x)]<-0
				ret <- na.action(ret)
				if(sum){
					plot(ts(cumsum(ret)),main=main,col=col)
					}else{
					plot(ts(cumprod(1+ret)-1),main=main,col=col)
					}
}

lines_strat<-function(ret,col='red',na.action=na.omit, sum=TRUE){
				#x[is.na(x)]<-0
				ret <- na.action(ret)
				if(sum){
					lines(ts(cumsum(ret)),col=col)
					}else{
					lines(ts(cumprod(1+ret)-1),col=col)
					}
}

na_clean<-function(x, value=0){
				x[is.na(x)]<-value
				return(x)
}

xts_transf<-
function (x, year = 2018, month = 02, day = 2, hour = 11, minutes = 00, period = 60)
{
    if (is.matrix(x))
        n <- nrow(x)
    if (is.data.frame(x))
        n <- nrow(x)
    if (is.vector(x) & is.numeric(x))
        n <- length(x)
    z <- ISOdatetime(year, month, day, hour, minutes, 00) + period * (1:n)
    x <- xts(x, order.by = z)
    return(x)
}

exp_po_stat <-
function(ret, signals, roll = TRUE, digits = 5, commiss=TRUE, spread=0.0001)
{
  maxDrawdown <- PerformanceAnalytics::maxDrawdown
		if(!is.xts(ret))return("xts object needed")
		ret <- na_clean(ret)
		trading<-extract_trades(ret, signals, commiss, spread)
		grossProfit<-sum(unlist(trading[trading>0]))
		grossLoss<-sum(unlist(trading[trading<0]))
		n_trades<-len(trading)
		win_trades<-len(unlist(trading[trading>0]))
		win_rate<-win_trades*100/n_trades
		win_rate<-round(win_rate,2)
		if(!roll){
			max_dd <- round(maxDrawdown(na.omit(ret)), digits)
			cat(paste('Max Drawdown:',max_dd,'\n',sep=' '))
			tot_ret <- round(grossProfit+grossLoss, digits)
			cat(paste('Total Returns:', tot_ret,'\n',sep=' '))
			ProfitFactor <- round(abs(grossProfit/grossLoss), digits)
			cat(paste('Profit Factor:',ProfitFactor,'\n',sep=' '))
			exp_payoff <- round((grossProfit+grossLoss)/n_trades, digits)
			cat(paste('Expected Pay Off', exp_payoff, '\n',sep=' '))
			hr <- round(hit_ratio(ret), digits)
			cat(paste('Hit Ratio:',hr,'\n',sep=' '))
			cat(paste('N. Total Trades',n_trades,'\n',sep=' '))
			cat(paste('Win% Rate Trades',win_rate))
			     }
		if(roll){
		  ProfitFactor <- round(abs(grossProfit/grossLoss), digits)
		  exp_payoff <- round((grossProfit+grossLoss)/n_trades, digits)
		  return(cbind(ProfitFactor,exp_payoff))
		}
}

### questa funzione calcola il lato migliore long/short

best_side <-
function(signal,ret)
{
		exp_long<-exp_po_stat(ifelse(signal>0,1,0)*ret)[2]
		exp_short<-exp_po_stat(ifelse(signal<0,1,0)*ret)[2]
	if(is.nan(exp_long) | is.nan(exp_short))return(0)
	if(exp_long>exp_short)return(1)
	if(exp_long<exp_short)return(-1)

}

hit_ratio <-
function(ret)
{
hr <- length(which(ret>0))/length(which(ret!=0))
return(hr)
}

prev <-
function (x, n)
{
  lag.x <- append(rep(0, n), lag(zoo(x), -n))
  return(lag.x)
}

n_trades_calculator <-
function (signals)
{
  signals <- na_clean(signals)
  trades<-ifelse(prev(signals,1)==0 & signals!=0,1,ifelse(prev(signals,1)>0 & signals<0,1,ifelse(prev(signals,1)<0 & signals>0,1,0)))
  if(signals[1]!=0) trades[1] <- 1
  n_trades <- sum(trades, na.rm=TRUE)
return( n_trades )
}

### estrazione trades
extract_trades <-
function(ret, signals, commiss=TRUE, spread=0.0001)
{
	ret[is.na(ret)]<-0
	signals[is.na(signals)]<-0

	if(sum(length(signals[signals>0])+length(signals[signals<0]))==0){
   					  statistic<-0
	                  return(statistic)
					  }
	idx_close<-ifelse(prev(signals,1)>0 & signals<= 0 | prev(signals,1)<0 & signals>= 0,1,0)
	idx_open<-ifelse(prev(signals,1)<= 0 & signals> 0 | prev(signals,1)>=0 & signals< 0,1,0)
	if(signals[1]!=0) idx_open[1] <- 1
	idx_open <- which(idx_open>0)
	idx_close<-which(idx_close>0)-1
	if(last(signals!=0))idx_close<-append(idx_close,length(signals))

    if( commiss) pf<-lapply(seq(1,length(idx_open)),function(i){sum(ret[idx_open[i]:idx_close[i]])-spread})
    if(!commiss) pf<-lapply(seq(1,length(idx_open)),function(i){sum(ret[idx_open[i]:idx_close[i]])})

		return(pf)
}

#http://www.mesasoftware.com/papers/SystemEvaluation.pdf
ProfitFactorExtended <-
function(ret, signals, kelly=FALSE, commiss=TRUE, spread=0.0001)
{

	ret[is.na(ret)]<-0
	signals[is.na(signals)]<-0
	if(sum(length(signals[signals>0])+length(signals[signals<0]))==0){
   					  statistic<-cbind(0,0,0,0,0,0,0,0,0,0)
		              colnames(statistic)<-c('Profit F.','% Wins','% Loss','Av W/L r.','Exp r.','Av Trade','Weighted Ave','P F breakeven','Opt % W','Expected DD')
	                  return(statistic)
					  }
	pf <- unlist (extract_trades ( ret, signals, commiss=commiss, spread=spread ))
	n_trades<-length(pf[pf!=0])
	## profit factor ratio
	prof_f<-sum(pf[pf>0])/abs(sum(pf[pf<0]))
	perc_w<-(length(which(pf>0))*100)/n_trades

	perc_l<-(length(which(pf<0))*100)/n_trades
	if(kelly){
		#Inserite i valori W ed R ottenuti, nella equazione di Kelly K% = W - (1 - W) / R.
		#W = Winning probability ::: R = WinAverage/lossAverage ratio
				K <- perc_w - ( 1 - perc_w ) / ( perc_w / perc_l )
				return(K)
		         }

    ## average winning trade to average losing trade ratioalso known as risk reward ratio
	ave_wl<-abs((sum(pf[pf>0])/length(pf[pf>0]))/(sum(pf[pf<0])/length(pf[pf<0])))
	## expectancy ratio
	exp_ratio<-(ave_wl*(perc_w/100))-(perc_l/100)
	## il breakeven si raggiunge quando l'avetrade, T, è uguale a zero
	aveTrade<-(sum(pf[pf>0])-abs(sum(pf[pf<0])))/n_trades
	# TW weighted average
	tw<-ave_wl*aveTrade
	# calcolo N x l'exp DD
	N<-log(0.0027)/log(1-perc_w/100)
	# l'exp_dd si calcola moltiplicando N x la media dei loss in dollari o pips
	exp_dd<-N * (sum(pf[pf<0])/length(pf[pf<0]))
	# optimum % sarebbe il punto di breakeven relativo alla perc_w !! anche se non sono certo al 100% che la formula sia corretta (cmq l'ho riportata passo passo)
	opt_w<-1/sqrt(1+prof_f)
	pf_be<-abs((1-perc_w)/perc_w)
	# Pessimistic Return Ratio PRR: PRR = (((W - (W^(1/2))) / T) * AW) / (((L + (L^(1/2))) / T) * AL)
	# http://boards.fool.com/performance-measures-pessimistic-return-ratio-10846009.aspx?sort=postdate
	prr<-abs((((length(pf[pf>0])-(length(pf[pf>0])^(1/2)))/n_trades)*sum(pf[pf>0])/length(pf[pf>0]))/(((length(pf[pf<0])+(length(pf[pf<0])^(1/2)))/n_trades)*sum(pf[pf<0])/length(pf[pf<0])))
	statistic<-cbind(prof_f,perc_w,perc_l,ave_wl,exp_ratio,aveTrade,tw,pf_be,opt_w*100,exp_dd,prr)
	colnames(statistic)<-c('Profit F.','% Wins','% Loss','Av W/L r.','Exp r.','Av Trade','Weighted Ave','P F breakeven','Opt % W','Expected DD','PRR')

	return(statistic)
}

##time stamp basato su logica sys.time
TimeStampMinutes <-
function(minutes=5, level='TF1')
{
minutes <- minutes * 60
if(!exists('minute_recorded')){
		minute_recorded <<- list()
		minute_recorded[[level]] <<- as.numeric(attr(as.xts(Sys.time()),'index'))
		return( TRUE )
		}
if(!exists(level, where = minute_recorded)){
		minute_recorded[[level]] <<- as.numeric(attr(as.xts(Sys.time()),'index'))
		return( TRUE )
		}

minutes_now <- as.numeric(attr(as.xts(Sys.time()),'index'))
diff_time <- diff ( c(minute_recorded[[level]],minutes_now) )
if(diff_time >= minutes){
		minute_recorded[[level]] <<- as.numeric(attr(as.xts(Sys.time()),'index'))
		return ( TRUE )
		}
return( FALSE )
}

###funzione per il calcolo di features pin body etc..
pin_calc<-function(ohlc){
	x<-ohlc
			up<-ifelse(Cl(x)>Op(x),Hi(x)-Cl(x),ifelse(Cl(x)<Op(x),Hi(x)-Op(x),ifelse(Cl(x)==Op(x),Hi(x)-Op(x),0)))
			dw<-ifelse(Cl(x)>Op(x),Lo(x)-Op(x),ifelse(Cl(x)<Op(x),Lo(x)-Cl(x),ifelse(Cl(x)==Op(x),Lo(x)-Op(x),0)))
			corpo<-Cl(x)-Op(x)
	return(list(up=up,dw=dw,corpo=corpo))
}

#' The 7 major currencies prices.
#'
#' A dataset containing the 7 major currencies prices in xts format (2,000 rows).
#'
#' @format A matrix xts object with 2000 rows and 7 columns:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   ...
#' }
#' @source \url{www.activtrades.com}
"db"
