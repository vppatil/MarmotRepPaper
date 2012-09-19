#breeding number predictions figure

pred<-c(6,8,3)
obs<-c(7,10,4)
setwd('C:/Users/Vijay/Documents/My Dropbox/thesis stuff/thesis docs/chapter 2')
tiff(file='breed pred.tiff',height=6,width=6,units='in',res=300)
dat<-matrix(c(pred,obs),2,3,byrow=T)
a<-barplot(dat,beside=T,col=c('black','grey'),ylim=c(0,10.2),tcl=.5,legend.text=c('Predicted','Observed'),args.legend=list(bty='n'),ylab='Number of Breeding Females',xlab='Year')
axis(1,at=c(2,5,8),labels=2007:2009,tcl=0,lwd=0)
abline(h=0)
box("plot")

dev.off()

