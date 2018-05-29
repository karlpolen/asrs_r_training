## @knitr setup
require(xts)
require(PerformanceAnalytics)
require(lubridate)
require(scales)
require(RColorBrewer)
colorpalette=brewer.pal(8, 'Set1')
require(zoo)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(xtable)
source(file = "P:/IMD/Karl/R projects/Public Performance/Scripts/Functions.R")
c.bm <- read.csv("P:/IMD/Karl/R projects/Public Performance/BenchmarkData/Comp_BM.csv", stringsAsFactors = F)
c.bm$Date <- as.Date(c.bm$Date, format='%m/%d/%Y')
comp.bm <- zoo(c.bm[,-1], c.bm[,1])
comp.bm <- comp.bm/100
ac.dat <- read.csv("P:/IMD/Karl/R projects/Public Performance/Composites/Asset.Class.csv", stringsAsFactors = F)
ac.dat$Date <- as.Date(ac.dat$Date, format='%m/%d/%Y')
ac.dat$NetReturn <- ac.dat$NetReturn/100
pw.file = read.csv("PolicyWeightsFI.csv",stringsAsFactors = F)
pw.file$Date = as.Date(pw.file$Date,format='%m/%d/%Y')
comp.ret = read.csv("comp.ret.csv",stringsAsFactors = F)
bench.ret = read.csv("bench.ret.csv",stringsAsFactors = F)
weights = read.csv("weights.csv",stringsAsFactors = F)
#set the look back number and create the time series
n <- 120
timeline = as.yearmon(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]),'Date'])
assets = colnames(pw.file)[-1]
weight.policy = zoo(pw.file[((dim(pw.file)[1]-(n-1)):dim(pw.file)[1]), assets], timeline)
return.actual = zoo(comp.ret[,-1], timeline)
return.benchmark = zoo(bench.ret[,-1], timeline)
weight.actual = zoo(weights[,-1], timeline)

#calculate returns (asset class benchmark simply need to be pulled from file above)
ca <- subset(ac.dat, ac.dat$Asset.Class == 'FI', select = c("Date", "NetReturn"))
ca.xts <- xts(ca[,-1], ca[,1])
ca.xts <- as.data.frame(coredata(ca.xts[((dim(ca.xts)[1]-(n-1)):dim(ca.xts)[1]),]))
composite.actual <- zoo(ca.xts,timeline)
ac.bm <- comp.bm[ ,paste0('FI','.BM')]
ac.bm <- as.data.frame(coredata(ac.bm))
composite.benchmark <- zoo(ac.bm[((dim(comp.bm)[1]-(n-1)):dim(comp.bm)[1]),], timeline)
over.under = weight.actual - weight.policy

## @knitr brinson

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
extract=tail(data.frame(attrib.df[,1:7]*100))
print(xtable(extract))

## @knitr frongello
attrib.fr=frong(attrib.df,composite.actual,composite.benchmark)
attrib.sum <- sumdfbycol(attrib.fr,c(".sel",".allo",".inter"),
                         c("Selection","Allocation","Interaction"))
attrib.sum <- cumsum(attrib.sum)
frong=tail(data.frame(attrib.sum*100))
print(xtable(frong))



## @knitr summary
#Plots of Attribution Analysis
#plot just the cumulative attribution effects
attrib.sum <- gg(attrib.sum,"Attribution","Growth_of_Dollar")
attrpos <- subset(attrib.sum,Growth_of_Dollar>=0)
attrneg <- subset(attrib.sum,Growth_of_Dollar<0)
summary = ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Attribution),stat="identity")+
  scale_fill_manual(values = colorpalette) +  ylab("Cumulative Excess Return")+
  scale_y_continuous(labels=percent) + ggtitle("Brinson Return Attribution")+
  theme(plot.title = element_text(hjust = 0.5))
print(summary)

## @knitr all.sel
#plot contributions to allocation effect by asset
allo.gd=subset(attrib.sum, Attribution=='Allocation')
alloc.de=gg(over.under,"Allocation","Difference")
allpos=subset(alloc.de,Difference>0)
allneg=subset(alloc.de,Difference<0)
allocation=ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Difference,fill=Allocation),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Difference,fill=Allocation),stat="identity")+
  geom_line(data = allo.gd, aes(x=Date, y=Growth_of_Dollar), colour='blue') +
  scale_y_continuous(labels=percent) + labs(fill='Sub-Asset Class') + 
  scale_fill_manual(values = colorpalette) + theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Over/Under Weights & Cumulative Allocation Effect (Line)")+ylab('Active Exposure / Allocation Excess Return')


#plot contributions to selection effect by asset
attrib.sel=attrib.fr[,grep(".sel",colnames(attrib.fr))]
colnames(attrib.sel)=assets
attrib.sel=cumsum(attrib.sel)
attrib.sel=gg(attrib.sel,"Selection","Growth_of_Dollar")
attrpos=subset(attrib.sel,Growth_of_Dollar>=0)
attrneg=subset(attrib.sel,Growth_of_Dollar<0)
selection=ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Selection),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Selection),stat="identity")+
  scale_y_continuous("Selection Effect Excess Return",labels=percent) + 
  scale_fill_manual(values = colorpalette) + ggtitle('Cumulative Selection Effect')+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(allocation,selection,ncol=2)

## @knitr interaction
#plot contributions to interaction effect by asset
attrib.sel=attrib.fr[,grep(".inter",colnames(attrib.fr))]
colnames(attrib.sel)=assets
attrib.sel=cumsum(attrib.sel)
attrib.sel=gg(attrib.sel,"Interaction","Growth_of_Dollar")
attrpos=subset(attrib.sel,Growth_of_Dollar>=0)
attrneg=subset(attrib.sel,Growth_of_Dollar<0)
interaction=ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Growth_of_Dollar,fill=Interaction),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Growth_of_Dollar,fill=Interaction),stat="identity")+
  scale_y_continuous("Interaction Effect Excess Return",labels=percent) + 
  scale_fill_manual(values = colorpalette) + ggtitle('Cumulative Interaction Effect')+
  theme(plot.title = element_text(hjust = 0.5))
print(interaction)

## @knitr rolling
#a rolling attribution example--36 month roll grouped by attribution effect
attribroll36=roll.attr(attrib.df,36,c(".sel",".allo",".inter"),
                       c("Selection","Allocation","Interaction"),
                       composite.actual,
                       composite.benchmark)
attribroll36gg=gg(attribroll36,"Attribution","Return")
attrpos=subset(attribroll36gg,Return>=0)
attrneg=subset(attribroll36gg,Return<0)
roll.36=ggplot()+
  geom_bar(data=attrpos,aes(x=Date,y=Return,fill=Attribution),stat="identity")+
  geom_bar(data=attrneg,aes(x=Date,y=Return,fill=Attribution),stat="identity")+
  ggtitle("Rolling 36 Month Attribution Analysis")+
  scale_fill_manual(values = colorpalette) + scale_y_continuous('36 Month Annualized Excess Return',labels=percent)+
  theme(plot.title = element_text(hjust = 0.5))
print(roll.36)

## @knitr homework
set.seed(1234)
stocks = rnorm(n=120,mean=.09, sd=.15)
bonds = rnorm(n=120, mean=.04, sd=.06)
timeline=as.yearmon("Feb 2017")-0:119/12
data = zoo(cbind(stocks,bonds),timeline)
weight.s = rnorm(n=120, mean=.6, sd=.05)
weight.b = 1 - weight.s
weights = zoo(cbind(weight.s,weight.b),timeline)
target.weights = zoo(data.frame("Stock Target"=rep(.6,120),
                                "Bond Target"= rep(.4,120)),timeline)
sp500 = rnorm(n=120, mean=.085, sd=.16)
lba = rnorm(n=120, mean=.039, sd=.057)
bms = zoo(cbind(sp500,lba), timeline)
composite.actual = zoo(rowSums(data * weights), timeline)
composite.benchmark=zoo(rowSums(bms * target.weights),timeline)

