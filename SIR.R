setwd("~/Stat624/SIRProject")
source("sourceFunctions.R")

data<-read.table("sir1.txt")$V1
n<-743

sum(data)/n #If everyone in the whole school was sick, the average person was sick for > 2 time periods



a12<-0.08477529 #Actual parameter estimates for my data
a32<-0.34203531 #actual parameter estimates for my data

#An example of how to generate data and then estimate the parameters
a12=.02
a32=.1
n<-150
days<-90
constMat<-matrix(c(-1, 0, #Constrain the two parameters to be between 0 and 1
                   1, 0,
                   0, -1,
                   0,1), byrow=T, nrow=4)
ci<- c(-1, 0, -1,0)

data<-rand.obs(a12, a32, n, days)
inits<-c(.05, .4)
constrOptim(inits,f,data=data,grad=NULL, n=n,ui=constMat,ci=ci)



#Simulation for answering questions about peak illness
steps<-(1:100)/100
combos<-expand.grid(steps,steps)
grid<-matrix(0, nrow=length(steps), ncol = length(steps))
max.grid<-matrix(0, nrow=length(steps), ncol = length(steps))

for (i in 1:nrow(combos)){

  grid[round(combos[i, 1]*100), round(combos[i,2]*100)]<-max_expected(combos[i,1], combos[i,2])
  max.grid[round(combos[i, 1]*100), round(combos[i,2]*100)]<-average_max(combos[i,1], combos[i,2], 
                      grid[round(combos[i, 1]*100), round(combos[i,2]*100)]+3 ) #only check three days past the theoretical maximum
}
save(grid, max.grid, file="GridResults.RData")
#grid

load("GridResults.RData")
require(ggplot2)
library(reshape2)


longData<-melt(grid)
longData$a12<-longData$Var1/100
longData$a32<-longData$Var2/100
max.grid<-max.grid[seq(5, 95, by=10), seq(5,95,by=10)]

breaks<-c(1,2,4,6,8,12,25,50,100)
pdf(file="DayGrid.pdf")
ggplot(longData, aes(x = a12, y = a32)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradientn(name = "Days", trans = "log", colors=topo.colors(9),
                      breaks = breaks, labels = breaks, guide="legend")+
  labs(x="Infection Rate", y="Recovery Rate", title="Number of Days Until Peak Sickness") +
  theme_bw() +
    geom_point(aes(x=.084775, y=.34203))+geom_text(aes(label="MLE for Boarding \n School", x= .084775, y=.34203),,hjust=-.010, vjust=1.3)
 dev.off()
 
 longMax<-melt(max.grid)
 longMax$a32<-rep(seq(5, 95, by=10), each=10)/100
 longMax$a12<-rep(seq(5, 95, by=10), times=10)/100
 


 pdf(file="MaxGrid.pdf")
 ggplot(longMax, aes(x = a12, y = a32)) + 
   geom_raster(aes(fill=value)) + 
   scale_fill_gradient(name = "People", high = "firebrick4", low="white", guide="legend",breaks=seq(0,100,20))+
   labs(x="Infection Rate", y="Recovery Rate", title="Max People Sick out of 100") +
   theme_bw() + theme(axis.text.x=element_text(size=12, angle=0, vjust=0.3), axis.title.x = element_text(size=12),
                      axis.title.y=element_text(size=12),
                      axis.text.y=element_text(size=12),
                      plot.title=element_text(size=12)) +
   geom_point(aes(x=.084775, y=.34203))+geom_text(aes(label="MLE for Boarding \n School", x= .084775, y=.34203),hjust=-.15, vjust=1)
 dev.off()
 
 
 #Create a graphic showing the boarding school's data lack of fit
 normal<-matrix(0, nrow=200, ncol=14)
 for (i in 1:200){
   normal[i, ]<-rand.obs(a12, a32, 743, 14)
 }
 
 require(reshape2)
 normal<-as.data.frame( t(normal))
 school<-read.table("sir1.txt")
 norm <- melt(normal)
 
pdf(file="LackOfFit.pdf")
ggplot(norm, aes(x=rep(1:14, 200)))+geom_line(aes(y=value, group=variable), alpha=.3, colour="darkgray", size=.7)+
  geom_line(data=school, aes(x=1:14, y=V1), color="red")+xlab("Observation Time Period")+
  ylab("# Observed Infected")+ggtitle("Boarding School vs. MLE Simulated Scenarios")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, angle=0, vjust=0.3), axis.title.x = element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.text.y=element_text(size=16),
        plot.title=element_text(size=20)) 
dev.off()

#Simulation to find confidence intervals on the maximum # people infected
boarding=quantile(sapply(1:1000, FUN=function(x)max(rand.obs( a12, a32, 100, 50))), c(.025, .975))
taf=quantile(sapply(1:1000, FUN=function(x)max(rand.obs( .2, .5, 100, 50))), c(.025, .975))
fat<-quantile(sapply(1:1000, FUN=function(x)max(rand.obs( .4, .2, 100, 50))), c(.025, .975))
oao<-quantile(sapply(1:1000, FUN=function(x)max(rand.obs( .1, .1, 100, 50))), c(.025, .975))
save(boarding, taf, fat, oao, file="ReasonableRange.RData")
