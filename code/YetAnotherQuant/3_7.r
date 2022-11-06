library('quantmod')
getSymbols("MEO.DE")
#stock charts
par(mfrow=c(2,1))
candleChart(MEO.DE, theme='white')
candleChart(MEO.DE, theme='white', subset='last 12 months')
#estimate volatility of daily returns
dailyRets = periodReturn(MEO.DE, period='daily')
n = length(dailyRets)
sigmaALL = round( sd(dailyRets), 4 )
sigmaYear = round( sd(dailyRets[(n-242):n]), 4 )
sigmama6Mon = round( sd(dailyRets[(n-141):n]), 4 )
sigmaQuarter = round( sd(dailyRets[(n-70):n]), 4 )
sigmaMonth = round( sd(dailyRets[(n-23):n]), 4 )
print(paste(sigmaALL, sigmaYear, sigmama6Mon, sep=" "))
print(paste(sigmaQuarter, sigmaMonth, sep=" "))