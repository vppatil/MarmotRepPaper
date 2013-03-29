setwd('~/Desktop')
cap<-read.csv(file='captures_all.csv',header=TRUE,sep=',')
cap$Numerical.Age<-ifelse(cap$Numerical.Age>3,3,cap$Numerical.Age)
juv<-subset(cap,cap$Numerical.Age==0)
cap<-subset(cap,cap$Numerical.Age==3)
t<-table(cap$Dye.Pattern,cap$Social.Group,cap$Year,cap$Sex)
t<-ifelse(t>0,1,t)
tsum<-apply(t,c(2,3,4),sum)

j<-table(juv$Dye.Pattern,juv$Social.Group,juv$Year,juv$Sex)
j<-ifelse(j>0,1,j)
jsum<-apply(j,c(2,3,4),sum)
fjuvs<-jsum[,,1]+jsum[,,3]/2

fadult<-tsum[,,1]
Ma<-fjuvs/fadult

jgy<-read.csv(file='jgroupyear_forfertilitymodels.csv',header=TRUE,sep=',')

groups<-rep(row.names(Ma),8)
Years<-rep(names)