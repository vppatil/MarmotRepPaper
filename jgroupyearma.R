setwd('~/Desktop')
jgy<-read.csv(file='jgroupyear_forfertilitymodels.csv',header=TRUE,sep=',')

attach(jgy)

glm1<-glm(juvs~offset(ad.fems)+as.factor(Year),family=poisson,na.action=na.omit,data=jgy)

capall<-read.csv(file='captures_all.csv',header=TRUE,sep=',')
juvs<-subset(capall,capall$Numerical.Age==0)

jt<-table(juvs$Dye.Pattern,juvs$Sex,juvs$Year)
jt<-ifelse(jt>0,1,jt)
jsum<-apply(jt,c(2,3),sum)

fjuvs<-jsum[1,]+jsum[3,]/2