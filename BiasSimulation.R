setwd("~/Stat624/project2")
source("sourceFunctions.R")

scenarios<-matrix(c(.084775, .34203, 473, 14, #Did I  really type 473 here??? IDIOT
                    .2, .7, 100, 14,
                    .7, .2, 100, 14,
                    .5, .5, 100, 14,
                    .1, .3, 200, 6,
                    .1, .3, 50, 20,
                    .02, .3, 500, 14,
                    .4, .3, 20, 8,
                    .084775, .34203, 743, 14
                  ), byrow=T, ncol=4)#  .084775, .34203, 743, 14
reps<-500
a12.results<-matrix(0, nrow=nrow(scenarios), ncol=reps)
a32.results<-matrix(0, nrow=nrow(scenarios), ncol=reps)
constMat<-matrix(c(-1, 0, #Constrain the two parameters to be between 0 and 1
                   1, 0,
                   0, -1,
                   0,1), byrow=T, nrow=4)
ci<- c(-1, 0, -1,0)
inits<-c(.05, .4)
for ( i in 9){#1:nrow(scenarios)){
  print(i)
  params<-scenarios[i,]
  for (j in 405:reps){
    rand.dat<-rand.obs(params[1], params[2], params[3], params[4])
    mles<-constrOptim(inits,f,data=rand.dat,grad=NULL, n=params[3],ui=constMat,ci=ci)$par
    a12.results[i, j]<-mles[1]
    a32.results[i,j]<-mles[2]
  }
  save(scenarios, a12.results, a32.results, file="BiasStudy.RData")
}

reps=500
load("BiasStudy.RData")
a12.ints<-apply(a12.results, 1, FUN=function(x) mean(x)+c(-1,1)*qt(.025, reps-1)*sd(x)/sqrt(length(x)))
a32.ints<-apply(a32.results, 1, FUN=function(x) mean(x)+c(-1,1)*qt(.025, reps-1)*sd(x)/sqrt(length(x)))

apply(a12.results, 1, sd)[order(-scenarios[,3])]
apply(a32.results, 1, sd)

scenarios[,1]<a12.ints[1,] & scenarios[,1]>a12.ints[2,]
scenarios[,2]<a32.ints[1,] & scenarios[,2]>a32.ints[2,]

plot(scenarios[,3]~apply(a12.results, 1, sd))
