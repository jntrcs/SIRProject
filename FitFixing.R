

constMat<-matrix(c(-1, 0, #Constrain the two parameters to be between 0 and 1
                   1, 0,
                   0, -1,
                   0,1), byrow=T, nrow=4)
ci<- c(-1, 0, -1,0)

ramped.data<-data[-(1:4)]
res<-constrOptim(inits, f, data=ramped.data, grad=NULL, n=680, ui=constMat,ci=ci)
rampeda12<-res$par[1]
rampeda23=res$par[2]


normal<-matrix(0, nrow=200, ncol=10)
for (i in 1:200){
  normal[i, ]<-rand.obs(rampeda12, rampeda23, 680, 10)
}

require(reshape2)
require(ggplot2)
normal<-as.data.frame( t(normal))
school<-data.frame(V1=read.table("sir1.txt")$V1[-(1:4)])
norm <- melt(normal)

pdf(file="BetterFit.pdf")
ggplot(norm, aes(x=rep(1:10, 200)))+geom_line(aes(y=value, group=variable), alpha=.3, colour="darkgray", size=.7)+
  geom_line(data=school, aes(x=1:10, y=V1), color="red")+xlab("Observation Time Period")+
  ylab("# Observed Infected")+ggtitle("Boarding School vs. Scenarios Simulated with MLEs")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, angle=0, vjust=0.3), axis.title.x = element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.title=element_text(size=12)) 
dev.off()
