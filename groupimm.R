setwd('~/Desktop')
library(rpart)
cap<-read.csv(file='capturesmod.csv',header=T,sep=',')
sight<-read.csv(file='sightings.csv',header=T,sep=',')
group<-read.csv(file='socialgroup.csv',header=T,sep=',')
sight<-subset(sight,sight$Dye.Pattern!='')


all.dyes<-unique(cap$Dye.Pattern)
group.dyes<-unique(group$Dye.Pattern)

#cap<-merge(cap,group,by='Dye.Pattern')
cap<-merge(sight,group,by='Dye.Pattern')

#exclude=c(1192,777,730,432)
#cap<-cap[-exclude,]
#cap$East.West.Valley[row.names(cap) %in% c(345,711,767,1036,1137)]='E'

#cap$Dye.Pattern[row.names(cap) %in% c(345,711,767,1036,1137)]
#cap$let.num<-as.numeric(cap$Letter)


alphabet<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

ns.a<-c(sort(alphabet,decreasing=T),'.',paste(alphabet,alphabet,sep=''))
ns.1<--26:26

coords<-cbind(ns.a,ns.1)

#ex1<-grep('Boulder',cap$Letter,ignore.case=T)
#ex2<-grep('loo',cap$Letter,ignore.case=T)
#ex<-c(ex1,ex2)
#ex<-sort(ex,decreasing=T)

#cap<-cap[-ex,]

cap<-subset(cap,cap$Letter!='')
cap$Letter[grep('BB',cap$Letter)]='BB'

let<-cap$Letter
for(i in 1:26){
	let<-gsub(alphabet[i],alphabet[i],let,ignore.case=T)}


let.num<-matrix(0,length(cap$Letter),1)
for (i in 1:53){

let.num<-ifelse(let==ns.a[i],ns.1[i],let.num)}

cap$let.num<-let.num
cap$ew.num<-as.numeric(cap$East.West)-1

east<-cap[cap$East.West=='E',]
west<-cap[cap$East.West=='W',]

#East.West.Vally,Letter, and Number are key here.
east.tree<-rpart(Social.Group~let.num+Number,data=east,na.action=na.omit)
west.tree<-rpart(Social.Group~let.num+Number,data=west)

forpred.east<-subset(east,select=c('let.num','Number'))
forpred.west<-subset(west,select=c('let.num','Number'))

east$pred.group<-predict(east.tree,forpred.east,type='class')
west$pred.group<-predict(west.tree,forpred.west,type='class')

pred<-rbind(east,west)

a<-pred$pred.group==pred$Social.Group
sum(a)/length(pred$pred.group)

pred[a==FALSE,c(1,39,42,23,24,25)]
