Nest<-function(est.type=c("hypothetical", "pilot"), w, ICC=NULL, k=NULL, x=NULL, y=NULL, data=NULL, alpha=0.05){
  z<-qnorm(1-alpha/2, mean=0, sd=1) #point on standard normal distribution exceeded with probability alpha/2

  type<-match.arg(est.type)
  if(type=="hypothetical") {
    n.est<-matrix(nrow=length(ICC), ncol=length(k))   
    for (i in 1:length(ICC)){
      for(j in 1:length(k)){
        n.est1<-8*(z^2)*(((1-ICC[i])^2)*((1+(k[j]-1)*ICC[i])^2))/(k[j]*(k[j]-1)*(w^2))+1 #estimated sample size for a given CI width
        n.est2<-ceiling(n.est1) #rounded UP to nearest integer
        n.est[i,j]<-n.est2
      }
    }
n.est.table<-data.frame(n.est, row.names=ICC)
names(n.est.table)<-k
n.est.table
    }
  
  else {
    square<-function(z){z^2}
    n.estcall <- Call <-match.call()
    xc<-as.character(n.estcall$x)
    yc<-as.character(n.estcall$y)
  inds<-unique(data[xc])[[1]]
  a<-length(inds)
  tdata<-data.frame(data[yc], data[xc])
  tmpbb<-anova.lm(aov(tdata[,1]~tdata[,2], data=tdata))
  num.df<-tmpbb[1][1,1]
  denom.df<-tmpbb[1][2,1];
  MSa<-tmpbb[3][1,1]
  MSw<-tmpbb[3][2,1]
  tmp.outj<-data.frame(lapply(unstack(na.omit(tdata)), FUN=length))
  k<-(1/(a-1))*(sum(tmp.outj)-(sum(square(tmp.outj))/(sum(tmp.outj))))
  var.w<-MSw
  var.a<-(MSa-MSw)/(k)
  r<-var.a/(var.w + var.a)
#below computes the Confidence interval
  low.F<-qf(alpha/2, num.df, denom.df, lower.tail=FALSE)
  up.F<-1/(qf(1-alpha/2, denom.df, num.df, lower.tail=TRUE))
  low.CI<-1-((k*MSw*low.F)/(MSa+MSw*(k-1)*low.F))
  up.CI<-1-((k*MSw*up.F)/(MSa+MSw*(k-1)*up.F))
  ICC.results<-list(ICC=r, LowerCI=low.CI, UpperCI=up.CI, N=a, k=k, var.within=var.w, var.among=var.a)
#estimated sample size for a given CI width using provided data
    n.est1b<-8*(z^2)*(((1-ICC.results$ICC)^2)*((1+(ICC.results$k-1)*ICC.results$ICC)^2))/(ICC.results$k*(ICC.results$k-1)*(w^2))+1
    n.est2b<-ceiling(n.est1b)
    n.est2b
  }
}

