#SourceFunctions


require(Rcpp)
sourceCpp("cppLikelihood.cpp",rebuild=TRUE)

#Random data generator
rand.obs<-function(a12, a32, n, days){
  trans.mat<-matrix(c(1-a12, 0 ,0 ,
                      a12, 1-a32, 0,
                      0, a32, 1), byrow=T, nrow=3)
  t0<-c(n-1, 1, 0)/n
  obs<-c(n-1,1,0)
  t.new<-t0
  sick<-rep(0, days)
  for (i in 1:days){
    t.new<-trans.mat%*%t.new
    got.sick<-rbinom(1, obs[1], a12)
    got.better<-rbinom(1, obs[2], a32)
    obs<-obs+c(-got.sick, got.sick-got.better, got.better)
    sick[i]<-obs[2]
    #print(t.new)
  }
  sick
}

#The log likelihood function in R (use the c++ version for speed)
logLikeR<-function(data, a12, a32, n){
  like<-0
  pr_old<-c(1, rep(0, n))
  for (i in 1:length(data)){
    x0<-ifelse(i==1, 1, data[i-1])
    x1<-data[i]
    pr_new<-rep(0, n+1)
    px<- 0
    for (j in which(pr_old!=0)){
      probr<-pr_old[j]
      r<-j-1 #r is for recovered people
      pxgr<-0 #prob x given r
      for (h in 0:x0){ #h stand for healed
        if (x0 - h <= x1 & x1<=n-r-h){
          probh<-dbinom(h, x0, a32)
          pxgr<-pxgr + probh*dbinom(x1-(x0-h), n-x0-r, a12)

          pr_new[j+h]<-pr_new[j+h]+probr*probh
        
        }
      }
      px<-px +pxgr*probr
    }
    like<-like+log(px)
    pr_old<-pr_new/sum(pr_new)

  }
  like
}

f<-function(inits, data, n){
  a=-logLike(data, inits[1],inits[2], n)
  return(a)
}

#A function that takes your parameters, pop. size, and returns the expected number of people infected at a given time period
expected.infected<-function(day, n, a12, a32){
  p0<-c(n-1, 1,0)
  t0<-p0/n
  trans<-matrix(c(1-a12,.0,0,
                  a12,1-a32,.0,
                  0,a32,1), byrow = T, nrow=3)
  for (i in 1:day){
    t0<-trans%*%t0
  }
  t0[2]*n
  
}

max_expected<-function(a12, a32){
  day<-1
  last_expected<-expected.infected(day, 100, a12, a32)
  next_expected<-Inf
  while (last_expected<=next_expected){
    day<-day+1
    next_expected<-expected.infected(day, 100, a12, a32)
    if (next_expected<last_expected){return(day-1)}
    last_expected<-next_expected
  }
}

average_max<-function(a12, a32, daysToCheck){
  mean(sapply(1:1000, FUN=function(x)max(rand.obs( a12, a32, 100, daysToCheck))))
}

