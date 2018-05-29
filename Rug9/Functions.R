#functions
trailing.rows <- function(x){
  data.length <- dim(x)[1]
  t.3rows <- ((data.length - 2):data.length)
  t.12rows <- ((data.length - 11):data.length)
  t.36rows <- ((data.length - 35):data.length)
  t.60rows <- ((data.length - 59):data.length)
  t.120rows <- ((data.length - 119):data.length)
  ans <- list(t.3rows, t.12rows,t.36rows,t.60rows,t.120rows)
  names(ans) <- c('three.mon', 'one.year', 'three.year', 'five.year', 'ten.year')
  return(ans)
}
gd=function(x) {
  exp(cumsum(log(1+x)))
}

gdminus1=function(x,one=1) {
  temp=gd(x)
  temp-matrix(one,nrow=nrow(x),ncol=ncol(x),byrow=TRUE)
}

gdweight=function(x){
  temp=gd(x)
  temp/matrix(rowSums(temp),ncol=ncol(x),nrow=nrow(x))
}
frong=function(atmat,rport,rben) {
  savetime=time(atmat)
  savename=colnames(atmat)
  rpcum=gd(as.numeric(rport))
  rben=as.numeric(rben)
  ans=atmat
  for (i in 2:nrow(atmat)) {
    ans[i,]=(ans[i,]*rpcum[i-1])+(rben[i]*colSums(ans[1:(i-1),]))
  }
  ans=zoo(ans,savetime)
  colnames(ans)=savename
  return(ans)
}
unzoo=function(x) {
  timetemp=as.numeric(time(x))
  nametemp=c("Date",colnames(x))
  ans=cbind(timetemp,as.data.frame(x))
  rownames(ans)=NULL
  colnames(ans)=nametemp
  return(ans)
}
sumdfbycol=function(x,byvec,namevec) {
  cname=colnames(x)
  savetime=time(x)
  x=as.array(x)
  ans=matrix(0,ncol=length(byvec),nrow=nrow(x))
  for(i in 1:length(byvec)) {
    ans[,i]=rowSums(x[,grep(byvec[i],cname)])
  }
  ans=zoo(ans,savetime)
  colnames(ans)=namevec
  return(ans)
}
gg=function(x,variable.name="Variable",value.name="Value") {
  melt(unzoo(x),id.vars="Date",
       variable.name=variable.name,
       value.name=value.name)
}
roll.attr=function (attribmat,rollwidth,grouping,group.name,asset_return,bench_return) {
  xdf=zoo(1:nrow(attribmat),time(attribmat))
  rollfunction=function(x) {
    ans=frong(attribmat[x,],asset_return[x],bench_return[x])
    ans=sumdfbycol(ans,grouping,group.name)
    ans=colSums(ans)
    if(rollwidth>12) ans=-1+exp((12/rollwidth)*log(1+ans))
    return(ans)
  }
  rollapply(xdf,rollwidth,rollfunction,by.column=FALSE,align="right")
}
trailing.returns <- function(x, rows){
  omonth <- fortify(round(x[dim(x)[1],]*100,2))
  one.month <- as.matrix(omonth[,-1])
  tmonth <- cumprod(1+x[rows$three.mon, ])-1
  three <- fortify(tmonth)
  three.month <- as.matrix(round(three[3,-1]*100, 2))
  if(rows$one.year>0){
    one.year <- round(Return.annualized(x[rows$one.year,], scale = 12)*100, 2) }
  else{one.year <- matrix(data=c(0,0), nrow = 1, ncol = 2)
  colnames(one.year) <- colnames(x)}
  if(rows$three.year>0){
    three.year <- round(Return.annualized(x[rows$three.year,], scale = 12)*100, 2)}
  else{three.year <- matrix(data=c(0,0), nrow = 1, ncol = 2)
  colnames(one.year) <- colnames(x)}
  if(rows$five.year>0){
    five.year <- round(Return.annualized(x[rows$five.year,], scale = 12)*100, 2)}
  else{five.year <- matrix(data=c(0,0), nrow = 1, ncol = 2)
  colnames(one.year) <- colnames(x)}
  if(rows$ten.year>0){
    ten.year <- round(Return.annualized(x[rows$ten.year,], scale = 12)*100, 2)}
  else{ten.year <- matrix(data=c(0,0), nrow = 1, ncol = 2)
  colnames(one.year) <- colnames(x)}
  itd <- round(Return.annualized(x, scale=12)*100, 2)
  ans <- rbind(one.month, three.month,one.year, three.year, five.year, ten.year, itd)
  row.names(ans) <- c('1 Month', '3 Month', '1 Year', '3 Year', '5 Year', '10 Year', 'ITD')
  return(ans)
}
coneplot=function(return,annual_expected_return,annual_standard_deviation,
                  periodicity=c('y','q','m'),ylabel=NULL) {
  #
  # return is a time series of monthly, quarterly or annual arithmetic return values
  # annual_expected_return is the annual expected return for the strategy
  # annual_standard_deviation is the expected annual volatility
  # periodicity is 'y', 'q' or 'm' indicating yearly, quarter or monthly data
  # ylabel is optional argument to provide a label for the y axis
  #
  # requires lubridate,zoo packages
  #
  n=length(return)
  months=0:n
  timelabel=months
  if (is.zoo(return)) {
    datehold=time(return)
    if(class(datehold)=="Date") {
      if (periodicity=='y') datehold=year(datehold)
      if (periodicity=='q') datehold=as.yearqtr(datehold)
      if (periodicity=='m') datehold=as.yearmon(datehold)
    }
    timelabel=as.numeric(datehold)
  }
  if(periodicity=='y') {
    expected_return=annual_expected_return
    standard_deviation=annual_standard_deviation
    if(is.zoo(return)) {
      timelabel=c(timelabel[1]-1,timelabel)
    }
    xlabel="Yearly Data"
  }
  if(periodicity=='q') {
    expected_return=-1+exp(log(1+annual_expected_return)/4)
    standard_deviation=annual_standard_deviation/sqrt(4)
    if(is.zoo(return)) {
      timelabel=c(timelabel[1]-(1/12),timelabel)
    }
    xlabel="Quarterly Data"
  }
  if(periodicity=='m') {
    expected_return=-1+exp(log(1+annual_expected_return)/12)
    standard_deviation=annual_standard_deviation/sqrt(12)
    if(is.zoo(return)) {
      timelabel=c(timelabel[1]-.25,timelabel)
    }
    xlabel="Monthly Data"
  }
  expected_gd=c(1,exp(cumsum(rep(log(1+expected_return),n))))
  actual_gd=c(1,exp(cumsum(log(1+coredata(return)))))
  one_sigma_up=expected_gd+(standard_deviation*months/sqrt(n))
  two_sigma_up=expected_gd+(2*standard_deviation*months/sqrt(n))
  one_sigma_down=expected_gd-(standard_deviation*months/sqrt(n))
  two_sigma_down=expected_gd-(2*standard_deviation*months/sqrt(n))
  conedf=data.frame(timelabel,expected_gd,actual_gd,one_sigma_up,two_sigma_up,
                    one_sigma_down,two_sigma_down)
  if (is.null(ylabel)) ylabel=names(x)[1]
  x=ggplot(conedf,(aes(x=timelabel)))+ylab(ylabel)+xlab(xlabel)+
    geom_line(linetype=2,colour='black',aes(y=expected_gd))+
    geom_line(colour='blue',aes(y=actual_gd))+
    geom_line(linetype=2,colour='orange',aes(y=one_sigma_up))+
    geom_line(linetype=2,colour='orange',aes(y=one_sigma_down))+
    geom_line(linetype=2,colour='red',aes(y=two_sigma_up))+
    geom_line(linetype=2,colour='red',aes(y=two_sigma_down))
  return(x)
}

active.mgr <-function(x){  
  #x is an active manager using fund id
  #Active Manager Analysis
  d=subset(mgr.data, mgr.data$FundID == x, select = c('Date','NetReturn'))
  d.xts <- xts(d[,-1], d[,1])
  i.d <- as.character(time(d.xts)[1])
  s.name <- mgrinfo[which(mgrinfo$FundID==x),'ShortName']
  b.code <- mgrinfo[which(mgrinfo$FundID == x),'SSCode']
  b.name <- bm.key[which(bm.key$SSCode==b.code), 'ShortName']
  bench <- bm.data[paste0(i.d, '/') , b.code]
  data <- merge(d.xts, bench)
  d.length <- dim(data)[1]
  colnames(data) <- c(s.name, b.name)
  chart.RelativePerformance(data[,1], data[,2], color = 'blue', legend.loc = 'topleft', ylog = TRUE,
                            main = paste(s.name, 'Cumulative Relative Performance'))
  act.ex <- data[ ,1] - data[ ,2]
  exp.ex <- mgrinfo[which(mgrinfo$FundID == x), 'ExpExcess'] #expected excess return
  exp.tr <- mgrinfo[which(mgrinfo$FundID == x), 'ExpTR']
  if(exp.tr=='Hist'){exp.tr <- TrackingError(data[,1], data[,2], scale=12)} else{
    exp.tr <- as.numeric(exp.tr)  #expected tracking error
  }
  exp.ir <- exp.ex/exp.tr
  excess=unzoo(act.ex)
  timelabel=as.yearmon(time(data))
  actual_return <- zoo(excess[-1],timelabel)
  theplot <- coneplot(actual_return,exp.ex,exp.tr,'m', ylabel='Excess Return')
  plot4 <- theplot+ggtitle(paste(s.name,"Actual Excess vs Expected Excess +/-1 & 2 Std Dev"))
  print(plot4)
  #stats.x <- xtable(stats, caption=paste("ITD Statistical Summary"))
  #digits(stats.x)<-4
  #print(stats.x, caption.placement="top")
  if(d.length < 36){
    charts.RollingPerformance(data, width = 12, main = paste(s.name, 'Rolling 1 year Performance'), 
                              legend.loc = 'bottomleft')
  }else{
    charts.RollingPerformance(data, width = 36, main = paste(s.name, 'Rolling 3 year Performance'), 
                              legend.loc = 'bottomleft') 
  }
  chart.RollingRegression(data[,1], data[,2], width = 12, main = paste(s.name,'Rolling 12 Month Beta'), 
                          attribute = 'Beta')
  #Barra Holdings Based Analysis
  barra(x,'Mgr')
  #next section spits out statistical analysis
  re.12 <- apply.rolling(act.ex, FUN = 'Return.annualized', width = 12)
  tr.12 <- apply.rolling(act.ex, FUN = 'StdDev.annualized', width = 12)
  act.ex.tr <- unzoo(merge(re.12,tr.12))
  act.ex.tr <- cbind(timelabel, act.ex.tr[,-1])
  plot1 <- ggplot(act.ex.tr, aes(x=timelabel, y=act.ex.tr[,2])) + geom_line(colour='blue') + xlab("Year") + ylab('')+
    ggtitle("Rolling 12 Month Excess Return (Blue) & Tracking Error (Orange)") +
    geom_line(colour='darkorange',aes(y=act.ex.tr[,3]))+ scale_x_continuous(trans = yearmon_trans())+
    scale_y_continuous(labels=percent)
  exp.up1 <- (exp.ex+exp.tr)/exp.tr
  exp.up2 <- (exp.ex+exp.tr*2)/exp.tr
  exp.down1 <- (exp.ex - exp.tr)/exp.tr
  exp.down2 <- (exp.ex-exp.tr*2)/exp.tr
  rp.12 <- apply.rolling(data[,1], FUN = 'Return.annualized', width = 12)
  rb.12 <- apply.rolling(data[,2], FUN = 'Return.annualized', width = 12)
  roll.inf.12 <- (rp.12-rb.12)/tr.12
  roll.inf.12$exp <- rep(exp.ir,(dim(roll.inf.12)[1]))
  roll.inf.12$up1 <- rep(exp.up1,(dim(roll.inf.12)[1]))
  roll.inf.12$up2 <- rep(exp.up2,(dim(roll.inf.12)[1]))
  roll.inf.12$down1 <- rep(exp.down1,(dim(roll.inf.12)[1]))
  roll.inf.12$down2 <- rep(exp.down2,(dim(roll.inf.12)[1]))
  ri.df <- unzoo(roll.inf.12)
  ri.df <- cbind(timelabel, ri.df[,-1])
  plot2 <- ggplot(ri.df, aes(x=timelabel, y=ri.df[,2])) + geom_line(colour='blue') + ylab("Information Ratio") +
    xlab("Year") + ggtitle(paste(s.name, "Rolling 12 Month Information Ratio")) +
    geom_line(linetype=2,colour='black',aes(y=exp))+
    geom_line(linetype=2,colour='orange',aes(y=up1))+
    geom_line(linetype=2,colour='red',aes(y=up2))+
    geom_line(linetype=2,colour='orange',aes(y=down1))+
    geom_line(linetype=2,colour='red',aes(y=down2)) +
    scale_x_continuous(trans = yearmon_trans())
  #t.test(roll.inf.12[,1], mu= exp.ir, alternative = 'greater')
  if(d.length >36){
    rp.36 <- apply.rolling(data[,1], FUN = 'Return.annualized', width = 36)
    rb.36 <- apply.rolling(data[,2], FUN = 'Return.annualized', width = 36)
    sd.36 <- apply.rolling(act.ex, FUN = 'StdDev.annualized', width = 36)
    roll.inf.36 <- (rp.36-rb.36)/sd.36
    roll.inf.36$exp <- rep(exp.ir,(dim(roll.inf.36)[1]))
    roll.inf.36$up1 <- rep(exp.up1,(dim(roll.inf.36)[1]))
    roll.inf.36$up2 <- rep(exp.up2,(dim(roll.inf.36)[1]))
    roll.inf.36$down1 <- rep(exp.down1,(dim(roll.inf.36)[1]))
    roll.inf.36$down2 <- rep(exp.down2,(dim(roll.inf.36)[1]))
    ri.df.36 <- unzoo(roll.inf.36)
    ri.df.36 <- cbind(timelabel, ri.df.36[,-1])
    plot3 <- ggplot(ri.df.36, aes(x=timelabel, y=ri.df.36[,2])) + geom_line(colour='blue') + ylab("Information Ratio") +
      xlab("Year") + ggtitle(paste(s.name, "Rolling 36 Month Information Ratio")) +
      geom_line(linetype=2,colour='black',aes(y=exp))+
      geom_line(linetype=2,colour='orange',aes(y=up1))+
      geom_line(linetype=2,colour='red',aes(y=up2))+
      geom_line(linetype=2,colour='orange',aes(y=down1))+
      geom_line(linetype=2,colour='red',aes(y=down2))+
      scale_x_continuous(trans = yearmon_trans())
  }
  print(plot1)
  print(plot2)
  if(d.length > 36){print(plot3)}
}

#Barra function for an individual portfolio or composite
#x is the composite code or fund id
#y is the following:  AC (asset class), 'Comp' (Domestic or International)
#SubAC (sub-asset class), or Mgr (individual manager)


subac.output <-function(x){ #x needs to be the sub asset class code; i.e. US.LC
  #if (onefile) pdf(file='LargeCap.pdf', width=8, height = 11, title = paste (as.of.date))
  #par(mfrow=c(2,2))
  #pull sub asset class data & bench
  d=subset(subac.f, subac.f$SubAC == x, select = c('Date','NetReturn'))
  d.xts <- xts(d[,-1], d[,1])
  i.d <- as.character(time(d.xts)[1])
  subac.bench <- comp.bm[ ,c("Date",paste0(x,".BM"))]
  bm.dat <- subac.bench[paste0(i.d,"/"),]
  sac.data <- merge(bm.dat, d.xts)
  colnames(sac.data)[2] <- x
  name <- sub.ac[which(sub.ac$SubAC == x), 'Description']
  #identify and pull data for individual funds
  funds <- mgrinfo[which(mgrinfo$SubAC == x & mgrinfo$Open =='Y'), 'FundID']
  s.name <- list()
  for(i in funds){
    sn <- mgrinfo[which(mgrinfo$FundID == i), 'ShortName']
    s.name <- c(s.name, sn)
  }
  short.name <- unlist(s.name)
  return.list <- list()
  i.date <- list()
  which.include.3 <- vector()
  which.include.5 <- vector()
  which.include.10 <- vector()
  for (i in funds){
    d <- subset(mgr.data, mgr.data$FundID == i, select = c('Date','NetReturn'))
    d.xts <- xts(d[,-1], d[,1])
    idate <- as.character(time(d.xts)[1])
    colnames(d.xts) = i
    return.list=c(return.list,list(d.xts))
    i.date=c(i.date,idate)
    sn <- mgrinfo[which(mgrinfo$FundID == i), 'ShortName']
    l <- dim(d.xts)[1]
    if(l > 36){which.include.3 <- c(which.include.3, sn)}
    if(l > 60){which.include.5 <- c(which.include.5, sn)}
    if(l > 120){which.include.10 <- c(which.include.10, sn)}
  } 
  names(i.date) <- short.name
  names(return.list) <- short.name
  fund.data <- do.call(merge, return.list)
  colnames(fund.data) <- short.name
  data <- merge(sac.data, fund.data)
  data <- data[paste0(i.d,'/'), ]
  d.length <- dim(data)[1]
  charts.PerformanceSummary(data, ylog = TRUE, wealth.index = TRUE, 
                            main=paste(name, 'Performance Summary'), colorset=dark6equal)
  chart.RelativePerformance(data[ , -1], data[ , 1], colorset = dark6equal, ylab='Excess Return',
                            legend.loc = 'topleft', ylog = TRUE, main = paste(name, 'Cumulative Relative Performance'))
  chart.Boxplot(data, Xlab='Monthly Return with Circle = Mean & centerline = Median')
  chart.RiskReturnScatter(data[((dim(data)[1]-(12-1)):dim(data)[1]), ], colorset = dark6equal,
                          main=paste(name, "Trailing 1 Year Performance vs Sharpe Ratios (1,2,3)"))
  chart.RiskReturnScatter(data[((dim(data)[1]-(36-1)):dim(data)[1]),c((colnames(data)[1:2]),which.include.3) ], main="Trailing 3 Year Performance vs Sharpe Ratios (1,2,3)", 
                          colorset = dark6equal, sharpe.ratio=NULL)
  chart.RiskReturnScatter(data[((dim(data)[1]-(60-1)):dim(data)[1]), c((colnames(data)[1:2]),which.include.5)],
                          main="Trailing 5 Year Performance vs Sharpe Ratios (1,2,3)", colorset = dark6equal,sharpe.ratio=NULL)
  if(d.length >= 120){
    chart.RiskReturnScatter(data[((dim(data)[1]-(120-1)):dim(data)[1]), c((colnames(data)[1:2]),which.include.10)],
                            main="Trailing 10 Year Performance vs Sharpe Ratios (1,2,3)", colorset = dark6equal)
  }
  charts.RollingPerformance(data[ , c((colnames(data)[1:2]),which.include.3)], width = 36, 
                            main=paste(name, 'Rolling 3 Year Performance'), 
                            colorset=dark6equal, legend.loc = 'topleft')
  chart.RollingRegression(data[ ,-1], data[,1], main=paste(name, 'Rolling 12 Month Beta'),
                          attribute='Beta',width = 12, colorset = dark6equal, legend.loc='bottomleft')
  #if(!onefile) dev.off()
  barra(x,'SubAC')
  internal <- mgrinfo[which(mgrinfo$SubAC == x & mgrinfo$Open =='Y' & mgrinfo$Internal == 'Y'), 'FundID']
  for(i in internal){
    #import navd data
    data <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/NAVD/", i,".csv"), stringsAsFactors = F)
    data$Date <- as.Date(data$Date, format='%m/%d/%Y')
    name <- mgrinfo[which(mgrinfo$FundID == i),'ShortName']
    d.xts <- xts(data[,-1], data[,1])
    d.xts <- d.xts/100
    colnames(d.xts)[4] <- "Cash and Futures Impact"
    total <- d.xts["2016-06-30/",-1]
    timeline <- as.yearmon(time(total))
    excess <- zoo(coredata(d.xts["2016-06-30/",1]), timeline)
    excess.gd <- gd(excess) -1
    total <- zoo(coredata(total), timeline)
    navd=gg(total,"Decomposition","Contribution")
    allpos=subset(navd,Contribution>0)
    allneg=subset(navd,Contribution<0)
    plot=ggplot()+
      geom_bar(data=allpos,aes(x=Date,y=Contribution,fill=Decomposition),stat="identity")+
      geom_bar(data=allneg,aes(x=Date,y=Contribution,fill=Decomposition),stat="identity")+
      geom_line(data = excess, aes(x=Index, y=excess[,1]), colour='navy', size=.5)+
      scale_y_continuous(labels = percent)+ ggtitle(paste(name,"NAV Decomposition Of Excess Return"))+
      scale_x_yearmon('Date') + xlab("") + ylab("Decomposition/ Excess Return")
    print(plot)
    #next section spits out statistical analysis
    d=subset(mgr.data, mgr.data$FundID == i, select = c('Date','NetReturn'))
    d.xts <- xts(d[,-1], d[,1])
    i.d <- as.character(time(d.xts)[1])
    s.name <- mgrinfo[which(mgrinfo$FundID==i),'ShortName']
    b.code <- mgrinfo[which(mgrinfo$FundID == i),'SSCode']
    b.name <- bm.key[which(bm.key$SSCode==b.code), 'ShortName']
    bench <- bm.data[paste0(i.d, '/') , b.code]
    data <- merge(d.xts, bench)
    d.length <- dim(data)[1]
    colnames(data) <- c(s.name, b.name)
    act.ex <- data[ ,1] - data[ ,2]
    exp.ex <- mgrinfo[which(mgrinfo$FundID == i), 'ExpExcess'] #expected excess return
    exp.tr <- mgrinfo[which(mgrinfo$FundID == i), 'ExpTR']
    if(exp.tr=='Hist'){exp.tr <- TrackingError(data[,1], data[,2], scale=12)} else{
      exp.tr <- as.numeric(exp.tr)  #expected tracking error
    }
    exp.ir <- exp.ex/exp.tr
    excess=unzoo(act.ex)
    timelabel=as.yearmon(time(data))
    actual_return <- zoo(excess[-1],timelabel)
    theplot <- coneplot(actual_return,exp.ex,exp.tr,'m', ylabel='Excess Return')
    plot4 <- theplot+ggtitle(paste(s.name,"Actual Excess vs Expected Excess +/-1 & 2 Std Dev"))
    print(plot4)
    re.12 <- apply.rolling(act.ex, FUN = 'Return.annualized', width = 12)
    tr.12 <- apply.rolling(act.ex, FUN = 'StdDev.annualized', width = 12)
    act.ex.tr <- unzoo(merge(re.12,tr.12))
    act.ex.tr <- cbind(timelabel, act.ex.tr[,-1])
    plot1 <- ggplot(act.ex.tr, aes(x=timelabel, y=act.ex.tr[,2])) + geom_line(colour='blue') + xlab("Year") + ylab('')+
      ggtitle("Rolling 12 Month Excess Return (Blue) & Tracking Error (Orange)") +
      geom_line(colour='darkorange',aes(y=act.ex.tr[,3]))+ scale_x_continuous(trans = yearmon_trans())+
      scale_y_continuous(labels=percent)
    exp.up1 <- (exp.ex+exp.tr)/exp.tr
    exp.up2 <- (exp.ex+exp.tr*2)/exp.tr
    exp.down1 <- (exp.ex - exp.tr)/exp.tr
    exp.down2 <- (exp.ex-exp.tr*2)/exp.tr
    rp.12 <- apply.rolling(data[,1], FUN = 'Return.annualized', width = 12)
    rb.12 <- apply.rolling(data[,2], FUN = 'Return.annualized', width = 12)
    roll.inf.12 <- (rp.12-rb.12)/tr.12
    roll.inf.12$exp <- rep(exp.ir,(dim(roll.inf.12)[1]))
    roll.inf.12$up1 <- rep(exp.up1,(dim(roll.inf.12)[1]))
    roll.inf.12$up2 <- rep(exp.up2,(dim(roll.inf.12)[1]))
    roll.inf.12$down1 <- rep(exp.down1,(dim(roll.inf.12)[1]))
    roll.inf.12$down2 <- rep(exp.down2,(dim(roll.inf.12)[1]))
    ri.df <- unzoo(roll.inf.12)
    ri.df <- cbind(timelabel, ri.df[,-1])
    plot2 <- ggplot(ri.df, aes(x=timelabel, y=ri.df[,2])) + geom_line(colour='blue') + ylab("Information Ratio") +
      xlab("Year") + ggtitle(paste(s.name, "Rolling 12 Month Information Ratio")) +
      geom_line(linetype=2,colour='black',aes(y=exp))+
      geom_line(linetype=2,colour='orange',aes(y=up1))+
      geom_line(linetype=2,colour='red',aes(y=up2))+
      geom_line(linetype=2,colour='orange',aes(y=down1))+
      geom_line(linetype=2,colour='red',aes(y=down2)) +
      scale_x_continuous(trans = yearmon_trans())
    #t.test(roll.inf.12[,1], mu= exp.ir, alternative = 'greater')
    if(d.length >36){
      rp.36 <- apply.rolling(data[,1], FUN = 'Return.annualized', width = 36)
      rb.36 <- apply.rolling(data[,2], FUN = 'Return.annualized', width = 36)
      sd.36 <- apply.rolling(act.ex, FUN = 'StdDev.annualized', width = 36)
      roll.inf.36 <- (rp.36-rb.36)/sd.36
      roll.inf.36$exp <- rep(exp.ir,(dim(roll.inf.36)[1]))
      roll.inf.36$up1 <- rep(exp.up1,(dim(roll.inf.36)[1]))
      roll.inf.36$up2 <- rep(exp.up2,(dim(roll.inf.36)[1]))
      roll.inf.36$down1 <- rep(exp.down1,(dim(roll.inf.36)[1]))
      roll.inf.36$down2 <- rep(exp.down2,(dim(roll.inf.36)[1]))
      ri.df.36 <- unzoo(roll.inf.36)
      ri.df.36 <- cbind(timelabel, ri.df.36[,-1])
      plot3 <- ggplot(ri.df.36, aes(x=timelabel, y=ri.df.36[,2])) + geom_line(colour='blue') + ylab("Information Ratio") +
        xlab("Year") + ggtitle(paste(s.name, "Rolling 36 Month Information Ratio")) +
        geom_line(linetype=2,colour='black',aes(y=exp))+
        geom_line(linetype=2,colour='orange',aes(y=up1))+
        geom_line(linetype=2,colour='red',aes(y=up2))+
        geom_line(linetype=2,colour='orange',aes(y=down1))+
        geom_line(linetype=2,colour='red',aes(y=down2))+
        scale_x_continuous(trans = yearmon_trans())
    }
    print(plot1)
    print(plot2)
    if(d.length > 36){print(plot3)}
  }
  active <- mgrinfo[which(mgrinfo$SubAC == x & mgrinfo$Open =='Y' & mgrinfo$Active == 'Y'), 'FundID']
  for(a in active){
    active.mgr(a)}
}

barra <-function(x, y){
  if (y == 'AC'){name <- ac.key[which(ac.key$Asset.Class == x),'ACName']}
  if (y == 'Comp'){name <- comp[which(comp$Composite == x),'Comp.Desc']}
  if (y == 'SubAC') {name <- sub.ac[which(sub.ac$SubAC == x),'Description']}
  if (y == 'Mgr') {name <- mgrinfo[which(mgrinfo$FundID == x),'ShortName']}
  t <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",x, '.barra.total.csv'), skip=1)
  t$Date <- as.Date(t$Date,format='%m/%d/%Y')
  colnames(t)[6:9] <- c("Sector", "Cash Drag", "Style Factors", "Stock Specific")
  b.excess <- zoo(t[,4], t[,1])
  timeline <- as.yearmon(time(b.excess))
  b <- coredata(b.excess)
  b.excess <- zoo(b, timeline)
  gd.be <- gd(b.excess)-1
  total <- zoo(t[ ,5:11], timeline)
  gdminus1.tot <- gdminus1(total)
  barra.df=gg(gdminus1.tot,"Total","Contribution")
  allpos=subset(barra.df,Contribution>0)
  allneg=subset(barra.df,Contribution<0)
  #col <- c('blue','orangered','forestgreen','cyan','red4','coral','aquamarine4')
  #pos.length <- length(unique(allpos[,'Factor']))
  #neg.length <- length(unique(allneg[,'Factor']))
  #pdf(file = 'Public Mkts Allocation.pdf', height = 6, width = 8)
  plot1 <- ggplot()+
    geom_bar(data=allpos,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
    geom_bar(data=allneg,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
    geom_line(data = gd.be, aes(x=Index, y=gd.be[,2]), colour='navy', size=.5)+
    ylab("Cumulative Excess Return (Line)")+scale_y_continuous(labels=percent)+
    scale_fill_manual(values = colorpalette, name = "Attribution") +
    ggtitle(paste(name, "Barra Factor Attribution"))
  
  #Barra Style Factor Breakdown
  s=read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",x, '.style.csv'), skip=1,
             stringsAsFactors = F)
  
  s$Date <- as.Date(s$Date, format='%m/%d/%Y')
  style <- zoo(s[,-1], timeline)
  style.gd <- gd(total[,4])-1
  gdminus1.style <- gdminus1(style)
  style.df=gg(gdminus1.style,"Factor","Contribution")
  s.allpos=subset(style.df,Contribution>0)
  s.allneg=subset(style.df,Contribution<0)
  #pdf(file = 'Public Mkts Allocation.pdf', height = 6, width = 8)
  plot2 <- ggplot()+
    geom_bar(data=s.allpos,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
    geom_bar(data=s.allneg,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
    geom_line(data = style.gd, aes(x=Index, y=style.gd[,2]), colour='blue', size=.5)+
    ylab("Cumulative Style Factors / Excess Return")+scale_y_continuous(labels=percent)+
    scale_fill_manual(values = colorpalette, name = "Factor") +
    ggtitle(paste(name,"Style Factor Breakout"))
  
  #Barra Style Factor by Month
  style.df=gg(style,"Factor","Contribution")
  s.allpos=subset(style.df,Contribution>0)
  s.allneg=subset(style.df,Contribution<0)
  plot6 <- ggplot()+
    geom_bar(data=s.allpos,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
    geom_bar(data=s.allneg,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
    ylab("Cumulative Style Factors")+scale_y_continuous(labels=percent)+
    scale_fill_manual(values = colorpalette, name = "Factor") +
    ggtitle("Monthly Style Factor Excess Return")
  
  #style factor exposure
  s.e=read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",x, '.style.exposure.csv'), skip=1,
               stringsAsFactors = F)
  s.e$Date <- as.Date(s.e$Date, format='%m/%d/%Y')
  s.e$Leverage <- s.e$Leverage * -1
  s.e$Earnings.Variability <- s.e$Earnings.Variability * -1
  s.e.zoo <- zoo(s.e[,-1], timeline)
  #combine exposures to be useful
  Value <- c('Book.to.Price', 'Earnings.Yield')
  Size <- c('Size','Mid.Capitalization')
  Volatility <- c('Beta','Residual.Volatility')
  Momentum <- c('Momentum','Long.Term.Reversal')
  Quality <- c('Leverage','Profitability','Earnings.Variability','Earnings.Quality','Investment.Quality')
  Yield <- 'Dividend.Yield'
  Growth <- 'Growth'
  Liquidity <- 'Liquidity'
  factors <- list(Value, Size, Volatility, Momentum, Quality, Yield, 
                  Growth,Liquidity)
  f.list <- list()
  for(f in factors){
    if(length(f) == 1){s <- s.e.zoo[ ,f]} else {
      sum <- rowSums(s.e.zoo[ ,f])
      s <- zoo(sum, timeline) 
    }
    f.list <- c(f.list, list(s))
  }
  combine <- do.call(merge, f.list)
  colnames(combine) <- c('Value','Size','Volatility','Momentum','Quality',
                         'Yield','Growth','Liquidity')
  fs.df <- unzoo(combine)[-1]
  fs.df <- cbind(fs.df, -rowSums(fs.df))
  fspos <- fs.df * subset(fs.df>0)
  ypos <- max(rowSums(fspos))
  fsneg <- fs.df * subset(fs.df<0)
  yneg <- max(rowSums(-fsneg))
  style.exp=gg(combine,"Style","Exposure")
  style.df <- unzoo(total[,'Style Factors'])
  yexpos <- max(style.df[ ,2])
  yexneg <- max(-style.df[ ,2])
  se.allpos=subset(style.exp,Exposure>0)
  se.allneg=subset(style.exp,Exposure<0)
  y1 <- max(yneg, ypos)
  y2 <- max(yexpos, yexneg)
  scalefactor <- floor((y1/y2)/2)
  excess.scaled <- style.df
  excess.scaled$excess <- style.df[,2] * scalefactor
  #customize the ticks
  niceticks <- c(.001, .01, .05, .2, .8, 1.5, 2.5, 5, 10)
  interval <- niceticks[which.min(abs(4-y1/niceticks))]
  ticksup <- floor(ypos/interval)
  ticksdown <- floor(yneg/interval)
  ticks <- interval * (-ticksdown:ticksup)
  ticklabel <- paste(ticks,'/', round(ticks/scalefactor*10000, 0),'bps')
  #pdf(file = 'Public Mkts Allocation.pdf', height = 6, width = 8)
  plot3 <- ggplot()+
    geom_bar(data=se.allpos,aes(x=Date,y=Exposure,fill=Style),stat="identity")+
    geom_bar(data=se.allneg,aes(x=Date,y=Exposure,fill=Style),stat="identity")+
    geom_line(data = excess.scaled, aes(x=Date, y=excess), colour='blue', size=.5)+
    scale_y_continuous("Exposure Variance / Excess Return", breaks = ticks, labels = ticklabel)+
    scale_fill_manual(values = colorpalette, name = "Factor") +
    ggtitle(paste(name,"Monthly Style Factors Active Exposure"))
  #Barra Sector Breakdown
  i <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",x, '.sector.csv'), skip=1,
                stringsAsFactors = F)
  i$Date <- as.Date(i$Date, format='%m/%d/%Y')
  industry <- zoo(i[,-1], timeline)
  industry.gd <- zoo((rowSums(gd(industry)-1)),timeline)
  industry.plot <- as.data.frame(coredata(industry.gd))
  industry.plot <- as.data.frame(cbind(timeline, industry.plot[,1]))
  gdminus1.industry <- gdminus1(industry)
  industry.df=gg(gdminus1.industry,"Sector","Contribution")
  i.allpos=subset(industry.df,Contribution>0)
  i.allneg=subset(industry.df,Contribution<0)
  #plot
  #pdf(file = 'Public Mkts Allocation.pdf', height = 6, width = 8)
  plot4 <- ggplot()+
    geom_bar(data=i.allpos,aes(x=Date,y=Contribution,fill=Sector),stat="identity")+
    geom_bar(data=i.allneg,aes(x=Date,y=Contribution,fill=Sector),stat="identity")+
    geom_line(data = industry.plot, aes(x=timeline, y=industry.plot[ ,2]), colour='blue', size=.5)+
    ylab("Cumulative Sector Excess Return")+scale_y_continuous(labels=percent)+
    scale_fill_manual(values = colorpalette10, name = "Sector") +
    ggtitle(paste(name,"Monthly GICS Sector Return"))
  i.e <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",x, '.sector.exposure.csv'), skip=1,
                  stringsAsFactors = F)
  i.e$Date <- as.Date(i.e$Date, format='%m/%d/%Y')
  industry.exposure <- zoo(round(i.e[,-1]*100,2), timeline)
  ind.df <- unzoo(industry.exposure)[-1]
  ind.sum <- cbind(ind.df, -rowSums(ind.df))
  indpos <- ind.sum*subset(ind.sum>0)
  ypos <- max(rowSums(indpos))
  indneg <- ind.sum*subset(ind.sum<0)
  yneg <- max(rowSums(-indneg))
  industry.ret <- unzoo(round(total[,'Sector']*100, 2))
  yexpos <- max(industry.ret[,2])
  yexneg <- max(-industry.ret[,2])
  industry.exp=gg(industry.exposure,"Sector","Contribution")
  i.allpos=subset(industry.exp,Contribution>0)
  i.allneg=subset(industry.exp,Contribution<0)
  y1 <- max(yneg, ypos)
  y2 <- max(yexpos, yexneg)
  scalefactor <- floor((y1/y2)/2)
  sector.scaled <- industry.ret[,2] * scalefactor
  excess.scaled <- as.data.frame(cbind(timeline, sector.scaled))
  #customize the ticks
  niceticks <- c(.001, .01, 1, 1.5, 4, 8, 15, 25, 50)
  interval <- niceticks[which.min(abs(4-y1/niceticks))]
  ticksup <- floor(ypos/interval)
  ticksdown <- floor(yneg/interval)
  ticks <- interval * (-ticksdown:ticksup)
  ticklabel <- paste(ticks,"%",'/', round(ticks/scalefactor, 1),'%')
  #pdf(file = 'Public Mkts Allocation.pdf', height = 6, width = 8)
  plot5 <- ggplot()+
    geom_bar(data=i.allpos,aes(x=Date,y=Contribution,fill=Sector),stat="identity")+
    geom_bar(data=i.allneg,aes(x=Date,y=Contribution,fill=Sector),stat="identity")+
    geom_line(data = excess.scaled, aes(x=timeline, y=excess.scaled[,2]), colour='blue', size=.5)+
    scale_y_continuous("Exposure Variance / Excess Return", breaks = ticks, labels = ticklabel)+
    scale_fill_manual(values = colorpalette10, name = "Sector") +
    ggtitle(paste(name,"GICS Sector Active Exposure"))
  print(plot1)
  print(plot3)
  print(plot6)
  print(plot2)
  print(plot5)
  print(plot4)
}

fi_active.mgr <-function(x)
{  
  #x is an active manager using fund id
  #Active Manager Analysis for Fixed Income
  d=subset(mgr.data, mgr.data$FundID == x, select = c('Date','NetReturn'))
  d.xts <- xts(d[,-1], d[,1])
  i.d <- as.character(time(d.xts)[1])
  s.name <- mgrinfo[which(mgrinfo$FundID==x),'ShortName']
  b.code <- mgrinfo[which(mgrinfo$FundID == x),'SSCode']
  b.name <- bm.key[which(bm.key$SSCode==b.code), 'ShortName']
  bench <- bm.data[paste0(i.d, '/') , b.code]
  data <- merge(d.xts, bench)
  d.length <- dim(data)[1]
  colnames(data) <- c(s.name, b.name)
  chart.RelativePerformance(data[,1], data[,2], color = 'blue', legend.loc = 'topleft', ylog = TRUE,
                            main = paste(s.name, 'Cumulative Relative Performance'))
  act.ex <- data[ ,1] - data[ ,2]
  exp.ex <- mgrinfo[which(mgrinfo$FundID == x), 'ExpExcess'] #expected excess return
  exp.tr <- mgrinfo[which(mgrinfo$FundID == x), 'ExpTR']
  if(exp.tr=='Hist'){exp.tr <- TrackingError(data[,1], data[,2], scale=12)} else{
    exp.tr <- as.numeric(exp.tr)  #expected tracking error
  }
  exp.ir <- exp.ex/exp.tr
  excess=unzoo(act.ex)
  timelabel=as.yearmon(time(data))
  actual_return <- zoo(excess[-1],timelabel)
  theplot <- coneplot(actual_return,exp.ex,exp.tr,'m', ylabel='Excess Return')
  plot4 <- theplot+ggtitle(paste(s.name,"Actual Excess vs Expected Excess +/-1 & 2 Std Dev"))
  print(plot4)
  
  if(d.length < 60){
    charts.RollingPerformance(data, width = 12, main = paste(s.name, 'Rolling 1 year Performance'), 
                              legend.loc = 'bottomleft')
  }else{
    charts.RollingPerformance(data, width = 36, main = paste(s.name, 'Rolling 3 year Performance'), 
                              legend.loc = 'bottomleft') 
  }
  #chart.RollingRegression(data[,1], data[,2], width = 12, main = paste(s.name,'Rolling 12 Month Beta'), 
  #                        attribute = 'Beta')
  
  #next section spits out statistical analysis
  re.12 <- apply.rolling(act.ex, FUN = 'Return.annualized', width = 12)
  tr.12 <- apply.rolling(act.ex, FUN = 'StdDev.annualized', width = 12)
  act.ex.tr <- unzoo(merge(re.12,tr.12))
  act.ex.tr <- cbind(timelabel, act.ex.tr[,-1])
  plot1 <- ggplot(act.ex.tr, aes(x=timelabel, y=act.ex.tr[,2])) + geom_line(colour='blue') + xlab("Year") + ylab('')+
    ggtitle("Rolling 12 Month Excess Return (Blue) & Tracking Error (Orange)") +
    geom_line(colour='darkorange',aes(y=act.ex.tr[,3]))+ scale_x_continuous(trans = yearmon_trans())+
    scale_y_continuous(labels=percent)
  exp.up1 <- (exp.ex+exp.tr)/exp.tr
  exp.up2 <- (exp.ex+exp.tr*2)/exp.tr
  exp.down1 <- (exp.ex - exp.tr)/exp.tr
  exp.down2 <- (exp.ex-exp.tr*2)/exp.tr
  rp.12 <- apply.rolling(data[,1], FUN = 'Return.annualized', width = 12)
  rb.12 <- apply.rolling(data[,2], FUN = 'Return.annualized', width = 12)
  roll.inf.12 <- (rp.12-rb.12)/tr.12
  roll.inf.12$exp <- rep(exp.ir,(dim(roll.inf.12)[1]))
  roll.inf.12$up1 <- rep(exp.up1,(dim(roll.inf.12)[1]))
  roll.inf.12$up2 <- rep(exp.up2,(dim(roll.inf.12)[1]))
  roll.inf.12$down1 <- rep(exp.down1,(dim(roll.inf.12)[1]))
  roll.inf.12$down2 <- rep(exp.down2,(dim(roll.inf.12)[1]))
  ri.df <- unzoo(roll.inf.12)
  ri.df <- cbind(timelabel, ri.df[,-1])
  plot2 <- ggplot(ri.df, aes(x=timelabel, y=ri.df[,2])) + geom_line(colour='blue') + ylab("Information Ratio") +
    xlab("Year") + ggtitle(paste(s.name, "Rolling 12 Month Information Ratio")) +
    geom_line(linetype=2,colour='black',aes(y=exp))+
    geom_line(linetype=2,colour='orange',aes(y=up1))+
    geom_line(linetype=2,colour='red',aes(y=up2))+
    geom_line(linetype=2,colour='orange',aes(y=down1))+
    geom_line(linetype=2,colour='red',aes(y=down2)) +
    scale_x_continuous(trans = yearmon_trans())
  #t.test(roll.inf.12[,1], mu= exp.ir, alternative = 'greater')
  if(d.length >36){
    rp.36 <- apply.rolling(data[,1], FUN = 'Return.annualized', width = 36)
    rb.36 <- apply.rolling(data[,2], FUN = 'Return.annualized', width = 36)
    sd.36 <- apply.rolling(act.ex, FUN = 'StdDev.annualized', width = 36)
    roll.inf.36 <- (rp.36-rb.36)/sd.36
    roll.inf.36$exp <- rep(exp.ir,(dim(roll.inf.36)[1]))
    roll.inf.36$up1 <- rep(exp.up1,(dim(roll.inf.36)[1]))
    roll.inf.36$up2 <- rep(exp.up2,(dim(roll.inf.36)[1]))
    roll.inf.36$down1 <- rep(exp.down1,(dim(roll.inf.36)[1]))
    roll.inf.36$down2 <- rep(exp.down2,(dim(roll.inf.36)[1]))
    ri.df.36 <- unzoo(roll.inf.36)
    ri.df.36 <- cbind(timelabel, ri.df.36[,-1])
    plot3 <- ggplot(ri.df.36, aes(x=timelabel, y=ri.df.36[,2])) + geom_line(colour='blue') + ylab("Information Ratio") +
      xlab("Year") + ggtitle(paste(s.name, "Rolling 36 Month Information Ratio")) +
      geom_line(linetype=2,colour='black',aes(y=exp))+
      geom_line(linetype=2,colour='orange',aes(y=up1))+
      geom_line(linetype=2,colour='red',aes(y=up2))+
      geom_line(linetype=2,colour='orange',aes(y=down1))+
      geom_line(linetype=2,colour='red',aes(y=down2))+
      scale_x_continuous(trans = yearmon_trans())
  }
  print(plot1)
  print(plot2)
  if(d.length > 60){print(plot3)}
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
