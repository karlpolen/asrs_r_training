# get stock data for IBM, Microsoft and Apple
start=as.Date("2006-12-20")
end=as.Date("2016-12-31")
tickers=paste(c("IBM","AAPL","MSFT"),"US EQUITY")
field=c("TOT_RETURN_INDEX_GROSS_DVDS","PE_RATIO")
require(Rbbg)
conn=blpConnect()
bbgdat=bdh(conn,tickers,field,start_date=start,end_date=end,
           option_names="periodicitySelection",option_values="MONTHLY")
x=blpDisconnect(conn)
require(tidyr)
bbgdat2=spread(bbgdat[,c(2,3,4)],ticker,TOT_RETURN_INDEX_GROSS_DVDS)
write.csv(bbgdat,file="longstockdat.csv")
write.csv(bbgdat2,file="widestockdat.csv")
