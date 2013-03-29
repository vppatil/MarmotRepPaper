pglmm2<-list()
pglmm2[[1]]<-lmer(juvs~PDO+(1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[2]]<-lmer(juvs~PDOlag+(PDOlag|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[3]]<-lmer(juvs~PDO+PDOlag+(PDOlag|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[4]]<-lmer(juvs~N.group+(N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[5]]<-lmer(juvs~num.breed+(num.breed|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[6]]<-lmer(juvs~PDO+N.group+(N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[7]]<-lmer(juvs~PDO+num.breed+(num.breed|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[8]]<-lmer(juvs~PDOlag+N.group+(PDOlag+N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[9]]<-lmer(juvs~PDOlag+num.breed+(PDOlag+num.breed|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[10]]<-lmer(juvs~PDO+PDOlag+N.group+(PDOlag+N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[11]]<-lmer(juvs~PDO+PDOlag+num.breed+(PDOlag+num.breed|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[12]]<-lmer(juvs~1+(1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[13]]<-lmer(juvs~PDOlag*num.breed+(num.breed|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[14]]<-lmer(juvs~PDO*num.breed+(num.breed|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[15]]<-lmer(juvs~PDOlag*N.group+(PDOlag+N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[16]]<-lmer(juvs~PDO*N.group+(N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2[[17]]<-lmer(juvs~(PDOlag+PDO)*num.breed+(PDOlag+num.breed|Social.Group),data=nov,family=poisson,na.action=na.omit)

AIC<-vector()
for (i in 1:17){
	AIC[i]=AIC(logLik(pglmm2[[i]]))}
	
models<-paste('pglmm',1:17,sep='.')
res<-data.frame(models,AIC)
res<-AIC.weight(17,res)
b<-sort(res$AIC)
res2<-res[match(b,res$AIC),]