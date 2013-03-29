#set wd
setwd('~/Desktop')
library(lme4)
library(gam)
source(file='~/Desktop/research/marmot work/Marmotdata&scripts/aicweight.r')

#first load multi_covariates.RData
load("/Users/HikLab/Desktop/desktop/multi_covariate_values.RData")

#then read in nov2 and drop unnecessary columns
nov<-read.csv(file='nov2.csv',header=TRUE,sep=',')
nov<-nov[,-c(3:7)]
#exclude pika creek and unknown
nov<-subset(nov,nov$Social.Group!='pika creek'&nov$Social.Group!='unknown')

#make colony and year vectors for forloops
colonies=c('cck','h-talus','k-talus','narnia','ob1','ob2','ob3','pika creek','sched','unknown','west','west snow')

years=1999:2004

#attach jsum, femsum, b.tot,groupsize

#j is colony (row), k is year (column)

#jsum first
for (i in 1:length(nov$Year)) {
	for (j in 1:12){
		for (k in 1:6){
			if (nov$Social.Group[i]==colonies[j] & nov$Year[i]==years[k]){
				nov$juvs[i]=jsum[j,k]
				nov$ad.fems[i]=femsum[j,k]
				nov$num.breed[i]=b.tot[j,k]
				nov$N.group[i]=groupsize[j,k]
				}}}}

#now make the other covariate, response variable values
#check for obvious inconsistencies, do the quick fix
nov$N.group<-nov$N.group-nov$juvs
nov$Ma<-nov$juvs/nov$ad.fems
nov$p.breed<-nov$num.breed/nov$ad.fems
nov$Ma.brd<-nov$juvs/nov$num.breed

#only one juv seen in narnia in 2000, so one of the females must have bred
nov[11,11]=1/3

#dataset should be complete now
#response variables are N.group, Ma, and Ma.brd

#the knownlitters dataset is separate- you will probably have to exclude the Ma stuff from this analysis

#figure out the appropriate distribution, the specification of random effects, and the results

#fit poisson glms first
#check overdispersion with quasi glm
#plot groupedData (nlme library), to look at the numbers

#model average if possible
#for Ma models, include an offset- as log(average(Ma))

####model set######
#poisson glms first
#run as reg, then quasi, then check for non-linearity with gams
setwd('~/Desktop')

pglm1<-glm(juvs~PDO,data=nov,family=poisson,na.action=na.omit)
pglm2<-glm(juvs~PDOlag,data=nov,family=poisson,na.action=na.omit)
pglm3<-glm(juvs~PDO+PDOlag,data=nov,family=poisson,na.action=na.omit)
pglm4<-glm(juvs~N.group,data=nov,family=poisson,na.action=na.omit)
pglm5<-glm(juvs~ad.fems,data=nov,family=poisson,na.action=na.omit)
pglm6<-glm(juvs~PDO+N.group,data=nov,family=poisson,na.action=na.omit)
pglm7<-glm(juvs~PDO+ad.fems,data=nov,family=poisson,na.action=na.omit)
pglm8<-glm(juvs~PDOlag+N.group,data=nov,family=poisson,na.action=na.omit)
pglm9<-glm(juvs~PDOlag+ad.fems,data=nov,family=poisson,na.action=na.omit)
pglm10<-glm(juvs~PDO+PDOlag+N.group,data=nov,family=poisson,na.action=na.omit)
pglm11<-glm(juvs~PDO+PDOlag+num.breed,data=nov,family=poisson,na.action=na.omit)
pglm12<-glm(juvs~1,data=nov,family=poisson,na.action=na.omit)

pglmm<-glm(juvs~PDO+PDOlag+N.group,data=nov,family=poisson,na.action=na.omit)

pglm.AIC<-AIC(pglm1,pglm2,pglm3,pglm4,pglm5,pglm6,pglm7,pglm8,pglm9,pglm10,pglm11,pglm12)

b<-sort(pglm.AIC$AIC)
pglm.AIC<-pglm.AIC[match(b,pglm.AIC$AIC),]
pglm.AIC<-AIC.weight(12,pglm.AIC)

#quasi poisson models
qglm1<-glm(juvs~PDO,data=nov,family=quasipoisson,na.action=na.omit)
qglm2<-glm(juvs~PDOlag,data=nov,family=quasipoisson,na.action=na.omit)
qglm3<-glm(juvs~PDO+PDOlag,data=nov,family=quasipoisson,na.action=na.omit)
qglm4<-glm(juvs~N.group,data=nov,family=quasipoisson,na.action=na.omit)
qglm5<-glm(juvs~ad.fems,data=nov,family=quasipoisson,na.action=na.omit)
qglm6<-glm(juvs~PDO+N.group,data=nov,family=quasipoisson,na.action=na.omit)
qglm7<-glm(juvs~PDO+ad.fems,data=nov,family=quasipoisson,na.action=na.omit)
qglm8<-glm(juvs~PDOlag+N.group,data=nov,family=quasipoisson,na.action=na.omit)
qglm9<-glm(juvs~PDOlag+ad.fems,data=nov,family=quasipoisson,na.action=na.omit)
qglm10<-glm(juvs~PDO+PDOlag+N.group,data=nov,family=quasipoisson,na.action=na.omit)
qglm11<-glm(juvs~PDO+PDOlag+num.breed,data=nov,family=quasipoisson,na.action=na.omit)
qglm12<-glm(juvs~1,data=nov,family=quasipoisson,na.action=na.omit)

#gam for best model
pgam8<-gam(juvs~s(PDOlag)+s(ad.fems),data=nov,family=poisson,na.action=na.omit)

#poisson glmm's using lmer, intercept only
pglmm1<-lmer(juvs~PDO+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2<-lmer(juvs~PDOlag+(PDOlag|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm3<-lmer(juvs~PDO+PDOlag+(PDO+PDOlag|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm4<-lmer(juvs~N.group+(N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm5<-lmer(juvs~ad.fems+(ad.fems|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm6<-lmer(juvs~PDO+N.group+(PDO+N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm7<-lmer(juvs~PDO+ad.fems+(PDO+ad.fems|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm8<-lmer(juvs~PDOlag+N.group+(PDOlag+N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm9<-lmer(juvs~PDOlag+ad.fems+(PDOlag+ad.fems|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm10<-lmer(juvs~PDO+PDOlag+N.group+(PDO+PDOlag+N.group|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm11<-lmer(juvs~PDO+PDOlag+ad.fems+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm12<-lmer(juvs~1+(1|Social.Group),data=nov,family=poisson,na.action=na.omit)


pglmm.AIC<-c(151.6,176.9,150.9,184.8,159.2,143.6,132.3,182.7,156.4,144.8,136.9,186.6)
pglmms<-as.factor(paste('pglmm',1:12,sep=''))
pglmm.AIC<-data.frame(pglmms,pglmm.AIC)
names(pglmm.AIC)<-c('model','AIC')

b<-sort(pglmm.AIC$AIC)
pglmm.AIC<-pglmm.AIC[match(b,pglmm.AIC$AIC),]
pglmm.AIC<-AIC.weight(12,pglmm.AIC)

#7 and 11 are best, so use LR tests to evaluate random effects
pglmm7.25<-lmer(juvs~PDO+ad.fems+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm7.5<-lmer(juvs~PDO+ad.fems+(ad.fems|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm7.75<-lmer(juvs~PDO+ad.fems+(1|Social.Group),data=nov,family=poisson,na.action=na.omit)


pglmm11.1<-lmer(juvs~PDO+PDOlag+ad.fems+(PDOlag+ad.fems|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm11.2<-lmer(juvs~PDO+PDOlag+ad.fems+(PDO+ad.fems|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm11.3<-lmer(juvs~PDO+PDOlag+ad.fems+(PDO+PDOlag|Social.Group),data=nov,family=poisson,na.action=na.omit)

pglmm11.4<-lmer(juvs~PDO+PDOlag+ad.fems+(PDOlag|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm11.5<-lmer(juvs~PDO+PDOlag+ad.fems+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm11.6<-lmer(juvs~PDO+PDOlag+ad.fems+(ad.fems|Social.Group),data=nov,family=poisson,na.action=na.omit)

pglmm11.7<-lmer(juvs~PDO+PDOlag+ad.fems+(1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm11.8<-lmer(juvs~PDO+PDOlag+ad.fems+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)

#use AIC values for 11.5, 7.5
#now a couple more models in which the fixed effects are dropped, all models now only have random effects for PDO and intercept
pglmm1<-lmer(juvs~PDO+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm2<-lmer(juvs~PDOlag+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm3<-lmer(juvs~PDO+PDOlag+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm4<-lmer(juvs~N.group+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm5<-lmer(juvs~num.breed+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm6<-lmer(juvs~PDO+N.group+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm7<-lmer(juvs~PDO+num.breed+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm8<-lmer(juvs~PDOlag+N.group+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm9<-lmer(juvs~PDOlag+num.breed+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm10<-lmer(juvs~PDO+PDOlag+N.group+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm11<-lmer(juvs~PDO+PDOlag+num.breed+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm12<-lmer(juvs~1+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm13<-lmer(juvs~PDOlag*num.breed+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm14<-lmer(juvs~PDO*num.breed+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm15<-lmer(juvs~PDOlag*N.group+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm16<-lmer(juvs~PDO*N.group+(PDO|Social.Group),data=nov,family=poisson,na.action=na.omit)
pglmm17<-lmer(juvs~(PDOlag+PDO)*num.breed+(PDO-1|Social.Group),data=nov,family=poisson,na.action=na.omit)


#check AIC numbers
pglmm.AIC<-c(151.6,147.2,146.9,145.4,101.5,146.6,100.3,133.4,101.8,135.1,100.4,151.8,101.1,99.35,132.7,148.1,97.98)

noPDOlag.AIC<-c(151.6,145.4,101.5,146.6,100.3,135.1,151.8,99.35,148.1)

pglmms<-as.factor(paste('pglmm',1:17,sep=''))
pglmm.AIC<-data.frame(pglmms,pglmm.AIC)
names(pglmm.AIC)<-c('model','AIC')

b<-sort(pglmm.AIC$AIC)
pglmm.AIC<-pglmm.AIC[match(b,pglmm.AIC$AIC),]
pglmm.AIC<-AIC.weight(17,pglmm.AIC)





#stick with poisson errors for now- the quasi models don't report a scale parameter anyway- presumably smaller than the value of 2 you got for the regular poisson glm.

#no evidence for slope terms using LR tests, so the best model out of the whole mess is the mixed model with lagged PDO and adult females thrown in.  Why lagged PDO would have a negative effect is hard to say.


#ma glms are acting weird, so go with littersize
knownlitters<-read.csv(file='knownlitters.csv',header=TRUE,sep=',')
names(knownlitters)[1]='Social.Group'
for (i in 1:length(knownlitters$Year)) {
	for (j in 1:12){
		for (k in 1:6){
			if (knownlitters$Social.Group[i]==colonies[j] & knownlitters$Year[i]==years[k]){
				knownlitters$juvs[i]=jsum[j,k]
				knownlitters$ad.fems[i]=femsum[j,k]
				knownlitters$num.breed[i]=b.tot[j,k]
				knownlitters$N.group[i]=groupsize[j,k]
				knownlitters$PDO[i]=df[k,2]
				knownlitters$PDOlag[i]=df[k,3]
				}}}}
				
knownlitters$p.breed<-knownlitters$num.breed/knownlitters$ad.fems
knownlitters$p.breed[c(8,9)]=1				
knownlitters<-subset(knownlitters,knownlitters$mother.guess!='unknown'&knownlitters$mother!='unresolved')
k.glm1<-glm(N.Litter~PDO,data=knownlitters,family=poisson,na.action=na.omit)
k.glm2<-glm(N.Litter~PDOlag,data=knownlitters,family=poisson,na.action=na.omit)
k.glm3<-glm(N.Litter~PDO+PDOlag,data=knownlitters,family=poisson,na.action=na.omit)
k.glm4<-glm(N.Litter~N.group,data=knownlitters,family=poisson,na.action=na.omit)
k.glm5<-glm(N.Litter~mother.age,data=knownlitters,family=poisson,na.action=na.omit)
k.glm6<-glm(N.Litter~PDO+N.group,data=knownlitters,family=poisson,na.action=na.omit)
k.glm7<-glm(N.Litter~PDO+mother.age,data=knownlitters,family=poisson,na.action=na.omit)
k.glm8<-glm(N.Litter~PDOlag+N.group,data=knownlitters,family=poisson,na.action=na.omit)
k.glm9<-glm(N.Litter~PDOlag+mother.age,data=knownlitters,family=poisson,na.action=na.omit)
k.glm10<-glm(N.Litter~PDO+PDOlag+N.group,data=knownlitters,family=poisson,na.action=na.omit)
k.glm11<-glm(N.Litter~PDO+PDOlag+mother.age,data=knownlitters,family=poisson,na.action=na.omit)
k.glm12<-glm(N.Litter~1,data=knownlitters,family=poisson,na.action=na.omit)
k.glm13<-glm(N.Litter~PDO*mother.age,data=knownlitters,family=poisson,na.action=na.omit)
k.glm14<-glm(N.Litter~PDOlag*mother.age,data=knownlitters,family=poisson,na.action=na.omit)
k.glm15<-glm(N.Litter~(PDO+PDOlag)*mother.age,data=knownlitters,family=poisson,na.action=na.omit)

#need to check litter size numbers.


k.glm.AIC<-AIC(k.glm1,k.glm2,k.glm3,k.glm4,k.glm5,k.glm6,k.glm7,k.glm8,k.glm9,k.glm10,k.glm11,k.glm12,k.glm13,k.glm14,k.glm15)
b<-sort(k.glm.AIC$AIC)
k.glm.AIC<-k.glm.AIC[match(b,k.glm.AIC$AIC),]
k.glm.AIC<-AIC.weight(15,k.glm.AIC)

