#litter size by group bar chart
litter

setwd('~/Dropbox/thesis stuff/thesis docs/chapter 2')
tiff(file='litterbygroup.tiff',height=6,width=8,units='in',res=300)
se_func<-function(x){sqrt(sum((x-mean(x))^2)/length(x))}

l<-tapply(litter$N.Litter,litter$Social.Group,mean)
se.l<-tapply(litter$N.Litter,litter$Social.Group,se_func)
groups<-row.names(cbind(se.l))
l.plot<-barplot(l,ylim=c(0,6),ylab='Mean Litter Size',xlab='Social Group',xaxt='n')
par(cex=.75)
axis(1,at=l.plot,labels=groups,cex=.75,tcl=0)
superpose.eb(l.plot,l,se.l)
dev.off()

#anova test for litter size variation between social groups

a<-glm(N.Litter~Social.Group,data=litter,family=gaussian)
anova(a,test='F')
