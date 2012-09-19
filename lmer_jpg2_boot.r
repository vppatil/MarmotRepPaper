#set the dir as marmot data files
setwd('C:/Users/Vijay/Documents/My Dropbox/thesis stuff/marmot data files')
#first test for poisson overdispersion
library(qcc)
library(lme4)

nov<-read.csv(file='nov09ma4.csv',header=TRUE,sep=',')


nov<-subset(nov,nov$ad.fems>0)
qcc.overdispersion.test(nov$juvs)
qcc.overdispersion.test(litter$N.Litter)


AIC.weight<-function(models,results){
results.weight<-matrix(0,models,1)
for (i in 1:models) {results.weight[i] = ((exp((-1/2)*(results$AIC[i]-min(results$AIC))))/sum(exp((-1/2)*(results$AIC-min(results$AIC)))))}
delta.AIC<-matrix(0,models,1)
for (i in 1:models) {delta.AIC[i] = results$AIC[i]-min(results$AIC)}

results<-cbind(results,delta.AIC,results.weight)
results
}


library(MASS)

lmer.avg<-function(nov){
samp<-1:length(nov[,1])
last.warning<<-NULL
indices<-sample(samp,length(samp),replace=TRUE)
dat<-nov[indices,]
#dat=nov
lmer1<-lmer(juvs~N.group+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer2<-lmer(juvs~PDO+(PDO-1|Social.Group)+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer3<-lmer(juvs~PDOlag+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer4<-lmer(juvs~N.group+PDO+(PDO-1|Social.Group)+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer5<-lmer(juvs~N.group+PDOlag+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer6<-lmer(juvs~PDO+PDOlag+(PDO-1|Social.Group)+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer7<-lmer(juvs~N.group+PDO+PDOlag+(PDO-1|Social.Group)+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer8<-lmer(juvs~N.group*PDO+(PDO-1|Social.Group)+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer9<-lmer(juvs~N.group*PDOlag+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer10<-lmer(juvs~N.group*(PDO+PDOlag)+(PDO-1|Social.Group)+(1|Social.Group),family=poisson,data=dat,na.action=na.omit)
lmer11<-lmer(juvs~(1|Social.Group),family=poisson,data=dat,na.action=na.omit)



ma.list<-list(lmer1,lmer2,lmer3,lmer4,lmer5,lmer6,lmer7,lmer8,lmer9,lmer10,lmer11)
aic<-vector()
k<-vector()
aic.c<-vector()
errors<-vector()
for (i in 1:11){
	false<-grep('false',names(warnings(ma.list[[i]])))
	sing<-grep('singular',names(warnings(ma.list[[i]])))
	errors[i]=length(c(false,sing))
	aic[i]=AIC(logLik(ma.list[[i]]))
	k[i]=(aic[i]+2*logLik(ma.list[[i]]))/2
	aic.c[i]=aic[i]+(2*k[i]*(k[i]+1))/(length(nov$juvs)-k[i]-1)}
	
	empty<-vector()
	if(sum(errors>0)>0){
	avg<-vector()
	return (avg)}else{
	

jpg.list<-ma.list

models<-paste('ma.lmer',1:11,sep='')
ma<-data.frame(models,aic.c)
names(ma)[2]<-'AIC'

ma<-AIC.weight(11,ma)
jpg<-ma


N.group<-vector()
pdo<-vector()
pdolag<-vector()
N.group.pdo<-vector()
N.group.pdolag<-vector()

for (i in 1:11){
	fix<-cbind(fixef(jpg.list[[i]]))
	var<-row.names(fix)
	N.group[i]<-ifelse(length(var[var=='N.group'])>0,fix[var=='N.group'],0)
	pdo[i]<-ifelse(length(var[var=='PDO'])>0,fix[var=='PDO'],0)
	pdolag[i]<-ifelse(length(var[var=='PDOlag'])>0,fix[var=='PDOlag'],0)
	N.group.pdo[i]<-ifelse(length(var[var=='N.group:PDO'])>0,fix[var=='N.group:PDO'],0)
	N.group.pdolag[i]<-ifelse(length(var[var=='N.group:PDOlag'])>0,fix[var=='N.group:PDOlag'],0)}
	weights<-jpg$results.weight
	
	N.group.w<-sum(N.group*weights)
	pdo.w<-sum(pdo*weights)
	pdolag.w<-sum(pdolag*weights)
	N.group.pdo.w<-sum(N.group.pdo*weights)
	N.group.pdolag.w<-sum(N.group.pdolag*weights)}
	
	avg<-c(N.group.w,pdo.w,pdolag.w,N.group.pdo.w,N.group.pdolag.w)
	return(avg)
	}
B=1000
#avg.boot<-vector()
for (i in 1:B){
	avg.boot<-cbind(avg.boot,c(lmer.avg(nov),i))
	}
	