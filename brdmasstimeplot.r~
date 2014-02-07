a<-adults.proc$data$ch
b<-adults.proc$data$Dye.Pattern
a<-strsplit(a,split='')
c<-matrix(0,76,6)
for (i in 1:76){
	c[i,]=a[[i]]}
	
library(lme4)
b99<-vector()
b00 <-vector()
b01 <-vector()
b02 <-vector()
b03 <-vector()
b04 <-vector()

b99<-ifelse(c[,1]=='B',1,0)
b00<-ifelse(c[,2]=='B',1,0)
b01 <-ifelse(c[,3]=='B',1,0)
b02 <-ifelse(c[,4]=='B',1,0)
b03 <-ifelse(c[,5]=='B',1,0)
b04<-ifelse(c[,6]=='B',1,0)
brd<-data.frame(Dye.Pattern=b,b99,b00,b01,b02,b03,b04)

#now merge with all
cap<-read.csv(file='captures_all.csv',header=TRUE,sep=',')
cap<-subset(cap,cap$Year<2005)
cap<-merge(cap,brd,by='Dye.Pattern')
cap<-subset(cap,cap$Sex=='Female')
cap<-subset(cap,cap$Numerical.Age>2)
cap$jdate<-julian.df(cap)
cap$cond<-log(cap$Mass/cap$Zygomatic.Arch)

cap$brd<-0
cap$brd[cap$Year==1999&cap$b99==1]=1
cap$brd[cap$Year==2000&cap$b00==1]=1
cap$brd[cap$Year==2001&cap$b01==1]=1
cap$brd[cap$Year==2002&cap$b02==1]=1
cap$brd[cap$Year==2003&cap$b03==1]=1
cap$brd[cap$Year==2004&cap$b04==1]=1
cap$brd<-as.factor(cap$brd)

source(file="C:/Users/Vijay/Documents/My Dropbox/thesis stuff/r scripts/julian.r")
c99<-subset(cap,cap$Year==1999)
c00<-subset(cap,cap$Year==2000)
c01<-subset(cap,cap$Year==2001)
c02<-subset(cap,cap$Year==2002)
c03<-subset(cap,cap$Year==2003)
c04<-subset(cap,cap$Year==2004)

mglm1<-lmer(Mass~jdate+brd+(brd-1|Year)+(1|Year),data=cap)
mglm2<-glm((Mass/Zygomatic.Arch)~jdate*brd,data=cap)
mglm2<-glm(Mass/Zygomatic.Arch~jdate+brd+as.factor(Year)+jdate:brd+as.factor(Year):jdate,data=cap)

cap$Year<-as.factor(cap$Year)
mglm2<-lmer(Mass/Zygomatic.Arch~jdate*brd+(brd|Year),data=cap)


mglm1<-glm(log(Mass)~jdate+b99,data=c99)
mglm2<-glm(log(Mass)~jdate+b00,data=c00)
mglm3<-glm(log(Mass)~jdate+b01,data=c01)
mglm4<-glm(log(Mass)~jdate+b02,data=c02)
mglm5<-glm(log(Mass)~jdate+b03,data=c03)
mglm6<-glm(log(Mass)~jdate+b04,data=c04)

cbind(coef(cglm1),coef(cglm2),coef(cglm3),coef(cglm4),coef(cglm5),coef(cglm6))

cglm1<-glm(cond~jdate+b99,data=c99)
cglm2<-glm(cond~jdate+b00,data=c00)
cglm3<-glm(cond~jdate+b01,data=c01)
cglm4<-glm(cond~jdate+b02,data=c02)
cglm5<-glm(cond~jdate+b03,data=c03)
cglm6<-glm(cond~jdate+b04,data=c04)


cbind(coef(cglm1),coef(cglm2),coef(cglm3),coef(cglm4),coef(cglm5),coef(cglm6))
setwd('C:/Users/Vijay/Documents/My Dropbox/thesis stuff/thesis docs/chapter 2')
tiff(file='brdvdate.tiff',height=6,width=6,units='in',res=300)
plot(cap$jdate[cap$brd==0],cap$cond[cap$brd==0],tcl=.5,xlab='Julian Day',ylab='log(Mass/Zygomatic Arch)',pch=15)
points(cap$jdate[cap$brd==1],cap$cond[cap$brd==1],pch=6)
legend(145,-2.3,legend=c('Non-Breeders','Breeders'),pch=c(15,6),lty=c(1,2),bty='n')

 b<-lm(cap$cond[cap$brd==0]~cap$jdate[cap$brd==0])
 a<-lm(cap$cond[cap$brd==1]~cap$jdate[cap$brd==1])
 
 abline(a,lty=2)
 abline(b)
 dev.off()
 
 summary(a)
 summary(b)
 c<-glm(log(cond)~jdate*brd,data=cap)
 