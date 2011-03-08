ICCest<-function(x, y, data=data, alpha=0.05){
  if(is.data.frame(data)==FALSE)
       stop(paste("object dataframe is of the type", class(data), "and must be of type data.frame", sep=" "))
  square<-function(z){z^2}
  ICCcall <- Call <-match.call()
  xc<-as.character(ICCcall[[2L]])
  yc<-as.character(ICCcall[[3L]])
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
  
  low.F<-qf(alpha/2, num.df, denom.df, lower.tail=FALSE)
  up.F<-1/(qf(1-alpha/2, denom.df, num.df, lower.tail=TRUE))
  low.CI<-1-((k*MSw*low.F)/(MSa+MSw*(k-1)*low.F))
  up.CI<-1-((k*MSw*up.F)/(MSa+MSw*(k-1)*up.F))

list(ICC=r, LowerCI=low.CI, UpperCI=up.CI, N=a, k=k, varw=var.w, vara=var.a)
}
