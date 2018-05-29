#Rug 6

## @knitr setup
require(ggplot2)
require(lubridate)
require(zoo)
require(gridExtra)
allcf=read.csv("allcashflowirr.csv")
allmv=read.csv("allhistoricalmv.csv")
fundinfo=read.csv("fundinfo.csv")
allcf=subset(allcf,allcf$Cash.Flow.Type=="EV")
colnames(allcf)=c("Date","ShortName","Type","Amount")
allcf$Type="V"
allcf$Date=as.Date(allcf$Date,format="%m/%d/%Y")
rownames(allcf)=NULL
allcfdate=unique(allcf$Date)
if (length(allcfdate)!=1) stop("more than one value date in all cf -- not sure how to handle")
allmv$Date=as.Date(allmv$Date,format="%m/%d/%Y")
allmv=subset(allmv,allmv$Date<allcfdate)
mv=rbind(allcf,allmv)
mvm=merge(mv,fundinfo,by.x="ShortName",by.y="Short")
mvm$Amount=mvm$Amount/1000000

# plot all private markets NAV by portfolio
## @knitr plot_priv_market
mvm_byport=aggregate(mvm$Amount,by=list(mvm$Date,mvm$Portfolio),sum)
colnames(mvm_byport)=c("Date","Portfolio","NAV")
ggplot(mvm_byport,aes(x=Date,y=NAV)) + geom_bar(stat="identity",aes(fill=Portfolio))+
  ggtitle("ASRS Private Markets Investments") + ylab("NAV ($Millions)")

# now plot percentages
mvm_bydate=aggregate(mvm$Amount,by=list(mvm$Date),sum)
colnames(mvm_bydate)=c("Date","Total")
mvm_byport=merge(mvm_byport,mvm_bydate)
mvm_byport$Percent=100*mvm_byport$NAV/mvm_byport$Total
ggplot(mvm_byport,aes(x=Date,y=Percent)) + geom_bar(stat="identity",aes(fill=Portfolio))+
  ggtitle("ASRS Private Markets Investments") + ylab("Percent")

  
## @knitr plot_by_strategy
mvmpe=subset(mvm,mvm$Portfolio=="PE")
mvmpe_bystrat=aggregate(mvmpe$Amount,by=list(mvmpe$Date,mvmpe$catshort),sum)
colnames(mvmpe_bystrat)=c("Date","Category","NAV")
mvmpe_bystrat$Category=as.character(mvmpe_bystrat$Category)
mvmpe_bystrat$Category=factor(mvmpe_bystrat$Category,
                            levels=c("Mezz","Scndry","VC","Tech","Dstr","Enrgy","MegBo","LrgBo","MedBo","SmlBo"))
plot1=ggplot(mvmpe_bystrat,aes(x=Date,y=NAV)) + geom_bar(stat="identity",aes(fill=Category))+
  ylab("NAV ($Millions)")+
  theme(legend.position="none")
# now plot percentages
mvmpe_bydate=aggregate(mvmpe$Amount,by=list(mvmpe$Date),sum)
colnames(mvmpe_bydate)=c("Date","Total")
mvmpe_bystrat=merge(mvmpe_bystrat,mvmpe_bydate)
mvmpe_bystrat$Percent=100*mvmpe_bystrat$NAV/mvmpe_bystrat$Total
plot2=ggplot(mvmpe_bystrat,aes(x=Date,y=Percent)) + geom_bar(stat="identity",aes(fill=Category))+
  ylab("Percent")
#plot them both side-by-side
grid.arrange(plot1,plot2,ncol=2,widths=c(4.5,5.5))


## @knitr plot_by_vintage
mvmpe_byvint=aggregate(mvmpe$Amount,by=list(mvmpe$Date,mvmpe$Vintage),sum)
colnames(mvmpe_byvint)=c("Date","Vintage","NAV")
ggplot(mvmpe_byvint,aes(x=Date,y=NAV)) + geom_bar(stat="identity",aes(fill=Vintage))+
  ggtitle("ASRS Private Equity") + ylab("NAV ($Millions)")

## @knitr plot_by_age
Age=as.numeric(as.yearqtr(mvmpe_byvint$Date)-mvmpe_byvint$Vintage)
Age[Age<0]=0
Age=round(Age)
Age=factor(Age,levels=max(Age):min(Age))
mvmpe_byvint$Age=Age
ggplot(mvmpe_byvint,aes(x=Date,y=NAV)) + geom_bar(stat="identity",aes(fill=Age))+
  ggtitle("ASRS Private Equity") + ylab("NAV ($Millions)")


