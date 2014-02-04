#mother age effects plot

#load fertility models workspace first
setwd('~/Dropbox/thesis stuff/R workspaces')
load(file='fertility models.RData')

setwd('~/Dropbox/thesis stuff/thesis docs/chapter 2')
tiff(file='litter_motherage.tiff',height=8,width=6,units='in',res=300)
litter2<-subset(litter,litter$mother.age<7)
litter3<-subset(litter,litter$mother.age>=7)
litter2<-subset(litter,litter$Year<2007)

oldpred<-lm(N.Litter~mother.age,data=litter2)

plot(litter$mother.age,litter$N.Litter,xlim=c(1.8,11.2),ylim=c(1,6),ylab='Litter Size',xlab='Age of Mother',bty='l',pch=16)

abline(v=6.5,lty=2)


abline(oldpred)
dev.off()

library(gam)
#a<-gam(N.Litter~s(mother.age),data=litter)
#plot(a,se=T,res=T,xlab='Age of Mother',ylab='Litter Size',bty='l')