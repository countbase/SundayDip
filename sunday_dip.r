
###Set enviroment and load required packages

	options(scipen="100")
	packs = c("dplyr","PoloniexR","lubridate")
	lapply(packs,require,character.only=TRUE)
	Sys.setlocale("LC_ALL","English")

	'%notin%' <- function(x,y) !(x%in%y)

	poloniex.public <- PoloniexPublicAPI()
	currencies <- ReturnCurrencies(poloniex.public) #get currencies on poloniex
	period = "D"

	coin = c("Bitcoin","Litecoin","Ethereum","Ripple","Stellar","Monero") #define your trading pairs
	pair = c("USDT_BTC","USDT_LTC","USDT_ETH","USDT_XRP","USDT_STR","USDT_XMR")
	from = as.POSIXct("2018-01-01 00:00:00 UTC")
	to = as.POSIXct("2018-03-05 00:00:00 UTC")

###Get Data via Poloniex API

	res_list <- list()
	
	for (k in seq_along(pair)) {
		dat = data.frame(ReturnChartData(poloniex.public,pair = pair[k],from = from,to = to,period = period))
		res_list[[k]] = dat %>%	mutate(date = as.Date(rownames(data.frame(dat))), 
										day = strftime(date,"%A"), ym = strftime(date,"%Y-%m"),
										coin = coin[k], change = (close - lag(close)) / close * 100) %>% 
								select(date,day,ym,close,change,coin) %>% arrange(date) %>% filter(!is.na(change))
		}
	
###Function to get performance sell/buy vs holding

	give_plot <- function(x,sell_on,buy_on) {

		k = 10000
		n = nrow(x)
		cash = rep(0,n)
		count = rep(0,n)

		sell_on = sell_on
		buy_on = buy_on

		for (i in 1:n) {
			
			if (i == 1) {
				
				count[i] = k / x[i,"close"]
				
				
			} else if (x[i,"day"] == sell_on) {
			
				cash[i] = count[last(which(count!=0))] * x[i,"close"]
				
			} else if (x[i,"day"] == buy_on) {
			
				count[i] = cash[last(which(cash!=0))] / x[i,"close"]
				
			}
			
		}	
			
		res = cbind(x,cash,count) %>% select(-ym) %>% filter(count!=0)
		lims = c(0,ceiling(max(res$count)*1.5))
		point = 1:nrow(res)
		
		plot(res$count, type = "l",ylim = lims, lwd=2,las=2, ylab="", xaxt="n",xlab="",
			font.main=1,cex.axis=1.5,cex=1.5)
		title(unique(x$coin), line = 1,cex.main=1.5)
		axis(1,point,res$date,las=2,cex.axis=1.1)
		lines(point,rep(res$count[1],length(point)),col="darkred",lwd=2)
	}

###Plots Results

par(mfrow=c(2,3)) #Opens 2x3 device for six plots
sapply(1:length(coin), function(x) give_plot(res_list[[x]],"Friday","Sunday")) #Plots results for coins defined in row 15, set sell/buy days



	