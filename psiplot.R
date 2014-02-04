setwd('C:/Users/Brad Griffith/Dropbox/thesis stuff/R workspaces')
#setwd('C:/Documents and Settings/Vijay Patil/Desktop/desktop/My Dropbox/thesis stuff/R workspaces/')

load(file='ch3oikos.RData')
superpose.eb <-
function (x, y, ebl, ebu = ebl, length = 0.08, ...)
    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
    length = length, ...)
years<-2000:2004
years2<-years-.05
years3<-years+.05

setwd('C:/Users/Brad Griffith/Dropbox/thesis stuff/thesis docs/chapter 2/marmotRepPaper')
#setwd('C:/Documents and Settings/Vijay Patil/Desktop/desktop/My Dropbox/thesis stuff/thesis docs/chapter 2/')

tiff(file='Psiplot.tiff',height=6,width=6,units='in',res=300)
plot(years,1-Psi.estimates[,2,2],xlab='Year',ylim=c(0,.9),xaxt='n',xlim=c(1999.8,2004.2),tcl=.5,ylab=expression(paste('Probability of breeding (',Psi,')',sep='')),type='o',lwd=1,pch = 16)
lines(years2,1-Psi.estimates[,1,2],lwd=1,lty=2)
lines(years3,1-Psi.estimates[,1,1],lwd=1,lty=3)
points(years2,1-Psi.estimates[,1,2],pch=2,lwd=1)
points(years3,1-Psi.estimates[,1,1],pch = 15,lwd=1)
axis(1,at=years,labels=years,tcl=.5)

superpose.eb(years,1-Psi.estimates[,2,2],Psi.se[,2,2],lwd=1)

superpose.eb(years2,1-Psi.estimates[,1,2],Psi.se[,1,2],lwd=1)
superpose.eb(years3,1-Psi.estimates[,1,1],Psi.se[,1,1],lwd=1)

legend(2000.1,.90,legend=c('Mature females (Age 4+): NB prev. year','Mature females (Age 4+): Bred prev. year','Newly mature females (Age 3)'),lwd=1,bty='n',pch = c(16,2,15),lty=c(1,2,3))

dev.off()

tiff(file='multisurv.tiff',height=6,width=6,units='in',res=300)
oldpar<-par(no.readonly=TRUE)
par(mar=c(5,6,4,2))
years=1:5
years2<-years-.05
years3<-years+.05
intervals<-paste(1999:2003,2000:2004,sep=':')
plot(years2,S.estimates[,1,1],lty=1,xlab='Time Interval',tcl=.5,ylab='Apparent Survival Probability (S)',type='o',lwd=1,xaxt='n',ylim=c(.35,1),pch = 16)
lines(years3,S.estimates[,2,1],lty=2,lwd=1)
points(years3,S.estimates[,2,1],pch=6,lwd=1)

axis(1,at=years,labels=intervals,font.axis=1,lwd=1,tcl=.5)
superpose.eb(years2,S.estimates[,1,1],S.se[,1,1],lwd=1)
superpose.eb(years3,S.estimates[,2,1],S.se[,2,1],lty=1,lwd=1)

legend(2,.52,legend=c('Breeder','Nonbreeder'),lwd=1,lty=c(1,2),pch=c(16,6),bty='n')
dev.off()
