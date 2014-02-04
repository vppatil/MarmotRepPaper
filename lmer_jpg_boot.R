#set the dir as marmot data files
setwd('C:/Users/Vijay/Documents/My Dropbox/thesis stuff/marmot data files')
#setwd('C:/Documents and Settings/Vijay Patil/Desktop/desktop/My Dropbox/thesis stuff/marmot data files/')

#first test for poisson overdispersion
library(qcc)
nov<-read.csv(file='nov09ma4.csv',header=TRUE,sep=',')


nov<-subset(nov,nov$ad.fems>0)
qcc.overdispersion.test(nov$juvs)
qcc.overdispersion.test(nov$Ma)
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

lmer.avg<-function(d,indices){
last.warning<<-NULL
samp<-1:length(nov[,1])
indices<-sample(samp,length(samp),replace=TRUE)
dat<-nov[indices,]
#dat<-nov
# candidate model set for juvs per group, juvs/female models has to be pasted in here manually

glm.nb1<-glm.nb(juvs~N.group, data=dat,na.action=na.omit)
glm.nb2<-glm.nb(juvs~PDO, data=dat,na.action=na.omit)
glm.nb3<-glm.nb(juvs~PDOlag, data=dat,na.action=na.omit)
glm.nb4<-glm.nb(juvs~N.group+PDO, data=dat,na.action=na.omit)
glm.nb5<-glm.nb(juvs~N.group+PDOlag, data=dat,na.action=na.omit)
glm.nb6<-glm.nb(juvs~PDO+PDOlag, data=dat,na.action=na.omit)
glm.nb7<-glm.nb(juvs~N.group+PDO+PDOlag,data=dat,na.action=na.omit)
glm.nb8<-glm.nb(juvs~N.group*PDO, data=dat,na.action=na.omit)
glm.nb9<-glm.nb(juvs~N.group*PDOlag,data=dat,na.action=na.omit)
glm.nb10<-glm.nb(juvs~N.group*(PDO+PDOlag), data=dat,na.action=na.omit)
glm.nb11<-glm.nb(juvs~1, data=dat,na.action=na.omit)



jpg.list<-list(glm.nb1,glm.nb2,glm.nb3,glm.nb4,glm.nb5,glm.nb6,glm.nb7,glm.nb8,glm.nb9,glm.nb10,glm.nb11)

aic<-AIC(glm.nb1,glm.nb2,glm.nb3,glm.nb4,glm.nb5,glm.nb6,glm.nb7,glm.nb8,glm.nb9,glm.nb10,glm.nb11)[,2]
k<-AIC(glm.nb1,glm.nb2,glm.nb3,glm.nb4,glm.nb5,glm.nb6,glm.nb7,glm.nb8,glm.nb9,glm.nb10,glm.nb11)[,1]

aic.c<-vector()
for (i in 1:11){
	aic.c[i]=aic[i]+(2*k[i]*(k[i]+1))/(length(nov$juvs)-k[i]-1)}

models<-paste('jpg2.lmer',1:11,sep='')
jpg<-data.frame(models,aic.c)
names(jpg)[2]<-'AIC'

jpg<-AIC.weight(11,jpg)

	
	
models<-paste('jpg.lmer',1:11,sep='')
jpg<-data.frame(models,aic.c)
names(jpg)[2]<-'AIC'

jpg<-AIC.weight(11,jpg)


N.group<-vector()
pdo<-vector()
pdolag<-vector()
N.group.pdo<-vector()
N.group.pdolag<-vector()

for (i in 1:11){
	fix<-cbind(coef(jpg.list[[i]]))
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
	N.group.pdolag.w<-sum(N.group.pdolag*weights)
	
	avg<-c(N.group.w,pdo.w,pdolag.w,N.group.pdo.w,N.group.pdolag.w)
	return(avg)
	}

B=10000
jpg.nb.boot<-vector()
for (i in 1:B){
	jpg.nb.boot<-cbind(jpg.nb.boot,c(lmer.avg(nov),i))
	}
	
names<-c('N.group','PDO','PDOlag','N.group*PDO','N.group*PDOlag','counter')

summary.func<-function(x){
	mean.x<-mean(x)
	se.x<-sd(x)
	ci.x<-quantile(x,c(.025,.975))
	uncertainty.x<-c(mean.x,se.x,ci.x)
	return(uncertainty.x)}
	
res.2000<-t(apply(jpg.nb.boot,1,summary.func))
row.names(res.2000)<-names

	
