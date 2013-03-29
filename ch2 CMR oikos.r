#oikos resubmission analyses
#checks to make sure that results in thesis are correct

setwd('C:/Users/Vijay/My Documents/My Dropbox/thesis stuff/R workspaces/')
#setwd('C:/Documents and Settings/Vijay Patil/Desktop/desktop/My Dropbox/thesis stuff/R workspaces/')

load(file='ch2multistate.RData')
#try to run mark wrapper with adults2.proc, adults2.ddl,final.list
#for comparsion of model sets:


#clear out old names, rebuild candidate model set
#still using adults2.proc, adults2.ddl

rm(list=ls()[grep('S.',ls())])
rm(list=c('p.estimates','p.se','p.average'))
rm(list=ls()[grep('Psi.',ls())])


S.pdo.pdolag<-list(formula=~pdo+pdolag)
S.pdo.pdolag.stratum<-list(formula=~pdo+pdolag+stratum)
S.pdo.pdolag.bystratum<-list(formula=~(pdo+pdolag)*stratum)


p.dot<-list(formula=~1)

#this was the list that went into the thesis
#I have to think about why covariates were excluded from 2yrolds in psi.ac,psi.ag,psi.ab
#remember, BRD is the number of potential breeders present.

Psi.ab3<-list(formula=~-1+Mat:BRD+Imm:BRD)
Psi.ac3<-list(formula=~-1+Mat:(pdo+pdolag)+Imm:(pdo+pdolag))
Psi.ag3<-list(formula=~-1+Mat:NJ+Imm:NJ)
Psi.age=list(formula=~Mat)
Psi.as<-list(formula=~Mat:stratum)
Psi.asc<-list(formula=~-1+Mat:(stratum+pdo+pdolag)+Imm:(pdo+pdolag))
Psi.ascb=list(formula=~-1+Mat:(stratum+pdo+pdolag+BRD)+Imm:(pdo+pdolag+BRD))
Psi.ascg=list(formula=~-1+Mat:(stratum+pdo+pdolag+NJ)+Imm:(pdo+pdolag+NJ))
Psi.asg=list(formula=~-1+Mat:(stratum+NJ)+Imm:NJ)
Psi.brd.s=list(formula=~BRD+stratum)
Psi.brd=list(formula=~BRD)
Psi.grp.p.s=list(formula=~NJ+pdo+stratum)
Psi.grp.s=list(formula=~NJ+stratum)
Psi.grp=list(formula=~NJ)
Psi.pdo.pdolag.s=list(formula=~stratum+pdo+pdolag)
Psi.pdo.pdolag=list(formula=~pdo+pdolag)
Psi.pdo.s=list(formula=~pdo+stratum)
Psi.pdo=list(formula=~pdo)
Psi.pdolag.s=list(formula=~pdolag+stratum)

#temporary section to determine whether the interaction between age and other covariates in first model set was appropriate
#Psi.ab2<-list(formula=~Mat*BRD)
#Psi.ac2<-list(formula=~Mat*(pdo+pdolag))
#Psi.ag2<-list(formula=~Mat*NJ)

#Psi.ab=list(formula=~Mat:BRD)
#Psi.ac=list(formula=~Mat:(pdo+pdolag))
#Psi.ag=list(formula=~Mat:NJ)
#previous models should not be used in final set

setwd('C:/Users/Vijay/Documents/')

#mark.wrapper requires you to write out model.list=, data=,ddl =
compare.list<-create.model.list('Multistrata')

full.compare<-mark.wrapper(model.list=compare.list,data=adults2.proc,ddl=adults2.ddl,output=FALSE,adjust=FALSE)
#compare.allmodels<-compare
compare.table<-full.compare$model.table[full.compare$model.table$DeltaAICc<7,4:9]
bad.indices<-as.numeric(row.names(full.compare$model.table)[full.compare$model.table$DeltaAICc>=7])

#full model list in full.compare, only models w/ deltaAICc<7 in compare
#full.compare<-compare
goodmodels.compare<-remove.mark(full.compare,model.numbers=bad.indices)
#compare<-full.compare
compare<-goodmodels.compare

S.average<-model.average(compare,parameter='S')
Psi.average<-model.average(compare,parameter='Psi')
p.average<-model.average(compare,parameter='p')

Psi.estimates<-tapply(Psi.average$estimate,list(Psi.average$time,Psi.average$stratum,Psi.average$age),mean)
S.estimates<-tapply(S.average$estimate,list(S.average$time,S.average$stratum,Psi.average$age),mean)
p.estimates<-tapply(p.average$estimate,list(p.average$time,p.average$stratum,Psi.average$age),mean)
Psi.estimates<-Psi.estimates[,,1:2]
S.estimates<-S.estimates[,,1:2]
p.estimates<-p.estimates[,,1:2]

Psi.se<-tapply(Psi.average$se,list(Psi.average$time,Psi.average$stratum,Psi.average$age),mean)
S.se<-tapply(S.average$se,list(S.average$time,S.average$stratum,Psi.average$age),mean)
p.se<-tapply(p.average$se,list(p.average$time,p.average$stratum,Psi.average$age),mean)
Psi.se<-Psi.se[,,1:2]
S.se<-S.se[,,1:2]
p.se<-p.se[,,1:2]

mat.indices<-unique(c(grep('Mat',top.res$model.table$Psi,fixed=TRUE),grep('Imm',top.res$model.table$Psi,fixed=T)))
stratum.indices<-grep('stratum',top.res$model.table$Psi)
pdo.indices<-grep('pdo',top.res$model.table$Psi)
pdolag.indices<-grep('pdolag',top.res$model.table$Psi)
NJ.indices<-grep('NJ',top.res$model.table$Psi)
BRD.indices<-grep('BRD',top.res$model.table$Psi)
mat.stratum<-intersect(mat.indices,stratum.indices)
mat.pdo<-intersect(mat.indices,pdo.indices)
mat.pdolag<-intersect(mat.indices,pdolag.indices)
mat.NJ<-intersect(mat.indices,NJ.indices)
mat.BRD<-intersect(mat.indices,BRD.indices)

stratum.Psi<-sum(top.res$model.table$weight[grep('stratum',top.res$model.table$Psi)])
pdo.Psi<-sum(top.res$model.table$weight[grep('pdo',top.res$model.table$Psi)])
pdolag.Psi<-sum(top.res$model.table$weight[grep('pdolag',top.res$model.table$Psi)])
NJ.Psi<-sum(top.res$model.table$weight[grep('NJ',top.res$model.table$Psi)])
BRD.Psi<-sum(top.res$model.table$weight[grep('BRD',top.res$model.table$Psi)])
Imm.Psi<-sum(top.res$model.table$weight[grep('Mat',top.res$model.table$Psi,fixed=TRUE)])
mat.stratum.Psi<-sum(top.res$model.table$weight[mat.stratum])
mat.pdo.Psi<-sum(top.res$model.table$weight[mat.pdo])
mat.pdolag.Psi<-sum(top.res$model.table$weight[mat.pdolag])
mat.NJ.Psi<-sum(top.res$model.table$weight[mat.NJ])
mat.BRD.Psi<-sum(top.res$model.table$weight[mat.BRD])

stratum.S<-sum(top.res$model.table$weight[grep('stratum',top.res$model.table$S)])
pdo.S<-sum(top.res$model.table$weight[grep('pdo',top.res$model.table$S)])
pdolag.S<-sum(top.res$model.table$weight[grep('pdolag',top.res$model.table$S)])
NJ.S<-sum(top.res$model.table$weight[grep('NJ',top.res$model.table$S)])
BRD.S<-sum(top.res$model.table$weight[grep('BRD',top.res$model.table$S)])
stratum.int.S<-sum(top.res$model.table$weight[grep(' *',top.res$model.table$S,fixed=TRUE)])

Psi.weights<-rbind(stratum.Psi,pdo.Psi,pdolag.Psi,NJ.Psi,BRD.Psi,Imm.Psi,mat.stratum.Psi,mat.pdo.Psi,mat.pdolag.Psi,mat.NJ.Psi,mat.BRD.Psi)
S.weights<-rbind(stratum.S,stratum.int.S,pdo.S,pdolag.S,NJ.S,BRD.S,Imm.S)

