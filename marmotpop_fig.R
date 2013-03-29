#load multi_covariate values first

 setwd("C:/Users/Vijay/Documents/My Dropbox/thesis stuff/R workspaces")
 load(file='multi_covariate_values.RData')
 setwd("C:/Users/Vijay/Documents/My Dropbox/thesis stuff/marmot data files")
 all<-read.csv(file='all.csv',header=TRUE,sep=',')

 setwd("C:/Users/Vijay/Documents/My Dropbox/thesis stuff/thesis docs/chapter 2")

tiff(filename='marmotpopplot.tiff',width=6,height=6,units='in',bg='white',res=300)

 #a<-table(all$Dye.Pattern,all$Year)
 #a<-ifelse(a>0,1,a)
 #a<-apply(a,2,sum)
 #a<-a[4:15]

#par(cex=1.5)
par(tcl=.5)
par(xaxp=c(20,160,7))

total.marmots=apply(groupsize,2,sum)
years=1998:2009
years2=1999:2004
popplot=plot(years2,total.marmots,ylim=c(20,160),xlim=c(1998.8,2009.2),type='b',pch=19,lwd=1,xlab='Year',ylab='Number of Marked Marmots in Study Area')
#lines(years2,total.marmots[1:6],ylim=c(25,160),xlim=c(1999,2009),type='b',bty='l',pch=19,col='red',lwd=1)
#legend(2003,170,legend=c('Pikas','All Marmot Social Groups'),pch=19,col=c('red','blue'),,bty='n')
#lines(1999:2004,sub[1:6],type='b',pch=19,ylim=c(25,75),xlim=c(1999,2009),col='red')
#lines(2007:2009,sub[7:9],type='b',pch=19,col='red')

b<-table(all$ID,all$Social.Group,all$Year)
b<-ifelse(b>0,1,b)
b<-apply(b,c(2,3),sum)

subset<-c('Camp Creek','Narnia','K-Talus','West')
tot<-apply(b,2,sum)
sub<-b[row.names(b)%in%subset,]
sub<-apply(sub,2,sum)

lines(1999:2004,sub[1:6],type='b',pch=2)
lines(2007:2009,sub[7:9],type='b',pch=2)

legend(2005, 140,legend=c('All Groups','2007-2009 subset'),pch=c(19,2),bty='n')


dev.off()
