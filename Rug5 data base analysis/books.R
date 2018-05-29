# this is the books.r file

## @knitr setup
books=read.csv("books.csv")
attach(books)
read[is.na(read)]=0

## @knitr book_table
require(xtable)
Author_freq=table(Author)
Author_freq=data.frame(Author_freq[order(Author_freq,decreasing=TRUE)])
print(xtable(Author_freq[1:6,]))

## @knitr decades

decade=10*floor(Year/10)
decade[(decade<1900)&(decade>=1800)]=1850
decade[decade<1800]=1750
require(ggplot2)
dec_read=aggregate(read,by=list(decade),sum)
dec_read.df=data.frame(vintage=dec_read[,1],N_read=dec_read[,2])
ggplot(dec_read.df,aes(x=vintage,y=N_read))+geom_point()+geom_smooth(method=lm)

## @knitr regress

decade_lm=lm(dec_read.df$N_read~dec_read.df$vintage)
print(xtable(decade_lm))
