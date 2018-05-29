
require(xts)
require(PerformanceAnalytics)
require(lubridate)
require(scales)
require(RColorBrewer)
colorpalette=brewer.pal(8, 'Set1')
colorpalettem=muted(colorpalette, l=65, c=75)
colorpalette10 <- brewer.pal(11, 'Set3')
colorpalette12 <- brewer.pal(12, 'Paired')
require(zoo)
require(ggplot2)
require(reshape2)
require(knitr)
require(xtable)
source(file = "P:/IMD/Karl/R projects/Public Performance/Scripts/Functions.R")

#Bring in mapping data
mgrinfo <- read.csv('P:/IMD/Karl/R projects/Public Performance/Mgrinfo.csv',stringsAsFactors = F)

#bring in all manager benchmarks and transform data
bm <- unique(mgrinfo[which(mgrinfo$Open=='Y'), 'SSCode'])
bms<-read.csv('P:/IMD/Karl/R projects/Public Performance/BenchmarkData/Index_Returns.csv', stringsAsFactors = F)
bms$Date <- as.Date(bms$Date,format='%m/%d/%Y')
bm.data <- zoo(bms[,-1], bms[,1])
bm.data=bm.data/100

#bring in all composite benchmarks and transform data
c.bm <- read.csv("P:/IMD/Karl/R projects/Public Performance/BenchmarkData/Comp_BM.csv", stringsAsFactors = F)
c.bm$Date <- as.Date(c.bm$Date, format='%m/%d/%Y')
comp.bm <- zoo(c.bm[,-1], c.bm[,1])
comp.bm <- comp.bm/100

#bring in rest of mapping and all data
ac.key <- read.csv('P:/IMD/Karl/R projects/Public Performance/AssetClassKey.csv', stringsAsFactors = F)
comp <- read.csv('P:/IMD/Karl/R projects/Public Performance/Composite.csv', stringsAsFactors = F)
ac.dat <- read.csv("P:/IMD/Karl/R projects/Public Performance/Composites/Asset.Class.csv", stringsAsFactors = F)
ac.dat$Date <- as.Date(ac.dat$Date, format='%m/%d/%Y')
ac.dat$NetReturn <- ac.dat$NetReturn/100
comp.dat <- read.csv("P:/IMD/Karl/R projects/Public Performance/Composites/Composites.csv", stringsAsFactors = F)
comp.dat$Date <- as.Date(comp.dat$Date, format='%m/%d/%Y')
comp.dat$NetReturn <- comp.dat$NetReturn/100
pw.file <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/PolicyWeights",'FI','.csv'),stringsAsFactors = F)
pw.file$Date <- as.Date(pw.file$Date,format='%m/%d/%Y')
#set the look back number
n <- 120
timeline <- as.yearmon(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]),'Date'])
assets <- colnames(pw.file)[-1]
weight.policy <- zoo(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]), assets], timeline)

#get market values, returns, and calculate weights
mvlist <- list()
ret.list <- list()
for(i in assets){
  d <- subset(comp.dat, comp.dat$Composite==i,select = c('Date','MktVal'))
  d.xts <- xts(d[,-1], d[,1])
  colnames(d.xts) <- i
  mvlist = c(mvlist, list(d.xts))
  dr <- subset(comp.dat, comp.dat$Composite==i, select = c("Date", "NetReturn"))
  dr$NetReturn <- dr$NetReturn
  dr.xts <- xts(dr[,-1], dr[,1])
  colnames(dr.xts) <- i
  ret.list = c(ret.list, list(dr.xts))
}
mvs <- do.call(merge, mvlist)
mvs <- na.fill(mvs, fill = 0)
mvs.data <- mvs[((dim(mvs)[1]-(n-1)):dim(mvs)[1]),]
total <- zoo(rowSums(mvs.data), timeline)
aw.list <- list()
for(i in assets){
  m <- as.data.frame(coredata(mvs.data))
  mvs.zoo <- zoo(m, timeline)
  pct <- mvs.zoo[,i]/total
  aw.list <- c(aw.list, list(pct))
}
weight.actual <- do.call(merge, aw.list)
colnames(weight.actual) <- assets
over.under <- weight.actual - weight.policy
ra <- do.call(merge, ret.list)
colnames(ra) <- assets
ra <- as.data.frame(coredata(ra))
return.actual <- zoo(ra[((dim(mvs)[1]-(n-1)):dim(mvs)[1]),], timeline)
return.actual <- na.fill(return.actual, 0)
m <- ncol(return.actual)
bm.name <- paste0(assets, '.BM')
return.benchmark=zoo(comp.bm[((dim(comp.bm)[1]-(n-1)):dim(comp.bm)[1]), bm.name], timeline)
return.benchmark <- na.fill(return.benchmark, fill = 0)

#calculate returns (asset class benchmark simply need to be pulled from file above)
ca <- subset(ac.dat, ac.dat$Asset.Class == 'FI', select = c("Date", "NetReturn"))
ca.xts <- xts(ca[,-1], ca[,1])
ca.xts <- as.data.frame(coredata(ca.xts[((dim(ca.xts)[1]-(n-1)):dim(ca.xts)[1]),]))
composite.actual <- zoo(ca.xts,timeline)
ac.bm <- comp.bm[ ,paste0('FI','.BM')]
ac.bm <- as.data.frame(coredata(ac.bm))
composite.benchmark <- zoo(ac.bm[((dim(comp.bm)[1]-(n-1)):dim(comp.bm)[1]),], timeline)
excess=composite.actual-composite.benchmark

#write to csv inputs to frongello method
write.csv(data.frame('Date'=time(return.actual),coredata(return.actual)),"comp.ret.csv")
write.csv(data.frame('Date'=time(return.benchmark),coredata(return.benchmark)),"bench.ret.csv")
write.csv(data.frame('Date'=time(weight.actual),coredata(weight.actual)),"weights.csv")

#Selection effect
selection <- weight.policy*(return.actual-return.benchmark)
#selection <- na.fill(selection, fill = 0)
colnames(selection)=paste0(assets,".sel")

#allocation effect
allocation <- return.benchmark*(weight.actual-weight.policy)
#allocation <- na.fill(allocation, fill = 0)
colnames(allocation)=paste0(assets,".allo")

#interaction effect
interaction <- (return.actual-return.benchmark)*(weight.actual-weight.policy)
#interaction <- na.fill(interaction, fill = 0)
colnames(interaction)=paste0(assets,".inter")

attrib.df=merge(selection,allocation,interaction)
attrib.fr=frong(attrib.df,composite.actual,composite.benchmark)
#check result  -- sum of attrib.df should equal the difference between bench and actual returns
#check at tolerance of one tenth basis point
#.00001>(sum(attrib.fr)-(gd(composite.actual)[n]-gd(composite.benchmark)[n]))

#Plots of Attribution Analysis
#plot just the cumulative attribution effects
attrib.sum <- sumdfbycol(attrib.fr,c(".sel",".allo",".inter"),
                         c("Selection","Allocation","Interaction"))
attrib.sum <- cumsum(attrib.sum)
attrib.sum <- gg(attrib.sum,"Attribution","Growth_of_Dollar")
attrpos <- subset(attrib.sum,Growth_of_Dollar>=0)
attrneg <- subset(attrib.sum,Growth_of_Dollar<0)
excess.ret <- apply.rolling(excess, FUN = 'Return.annualized', width = 12, scale=12)
excess.ret <- coredata(excess.ret)
ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  scale_fill_manual(values = colorpalette) +
  scale_y_continuous(labels=percent) + ggtitle("Brinson Return Attribution")

#plot contributions to allocation effect by asset
allo.gd=subset(attrib.sum, Attribution=='Allocation')
alloc.de=gg(over.under,"Allocation","Difference")
allpos=subset(alloc.de,Difference>0)
allneg=subset(alloc.de,Difference<0)
ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Difference,fill=Allocation),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Difference,fill=Allocation),stat="identity")+
  geom_line(data = allo.gd, aes(x=Date, y=Growth_of_Dollar), colour='blue') +
  scale_y_continuous(labels=percent) + labs(fill='Sub-Asset Class') + 
  scale_fill_manual(values = colorpalette) +
  ggtitle("Over/Under Weights & Cumulative Allocation Effect (Line)")+ylab('Active Exposure / Allocation Excess Return')
#dev.off()

#plot contributions to selection effect by asset
attrib.sel=attrib.fr[,grep(".sel",colnames(attrib.fr))]
colnames(attrib.sel)=assets
attrib.sel=cumsum(attrib.sel)
attrib.sel=gg(attrib.sel,"Selection","Growth_of_Dollar")
attrpos=subset(attrib.sel,Growth_of_Dollar>=0)
attrneg=subset(attrib.sel,Growth_of_Dollar<0)
ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Selection),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Selection),stat="identity")+
  scale_y_continuous("Selection Effect Excess Return",labels=percent) + 
  scale_fill_manual(values = colorpalette) + ggtitle('Cumulative Selection Effect')

#plot contributions to interaction effect by asset
attrib.sel=attrib.fr[,grep(".inter",colnames(attrib.fr))]
colnames(attrib.sel)=assets
attrib.sel=cumsum(attrib.sel)
attrib.sel=gg(attrib.sel,"Interaction","Growth_of_Dollar")
attrpos=subset(attrib.sel,Growth_of_Dollar>=0)
attrneg=subset(attrib.sel,Growth_of_Dollar<0)
ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Interaction),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Interaction),stat="identity")+
  scale_y_continuous("Interaction Effect Excess Return",labels=percent) + 
  scale_fill_manual(values = colorpalette) + ggtitle('Cumulative Interaction Effect')

#a rolling attribution example--36 month roll grouped by attribution effect
attribroll36=roll.attr(attrib.df,36,c(".sel",".allo",".inter"),
                       c("Selection","Allocation","Interaction"),
                       composite.actual,
                       composite.benchmark)
attribroll36gg=gg(attribroll36,"Attribution","Return")
attrpos=subset(attribroll36gg,Return>=0)
attrneg=subset(attribroll36gg,Return<0)
ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Return,fill=Attribution),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Return,fill=Attribution),stat="identity")+
  ggtitle("Rolling 36 Month Attribution Analysis")+
  scale_fill_manual(values = colorpalette) + scale_y_continuous('36 Month Annualized Excess Return',labels=percent)