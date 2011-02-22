#Below takes a dataframe of a trait and individuals with multiple measurements on each individual and calculates the repeatability.
# Alpha is the size of the confidence interval (95% default).  x is the groups or individuals and y is the trait (measurements).

ICCest<-function(x, y, data=data, alpha=0.05){
  if(is.data.frame(data)==FALSE)
       stop(paste("object dataframe is of the type", class(data), "and must be of type data.frame", sep=" "))
  square<-function(z){z^2}
  ICCcall <- Call <-match.call()
  xc<-as.character(ICCcall[[2L]])
  yc<-as.character(ICCcall[[3L]])
  inds<-unique(data[xc])[[1]]
  a<-length(inds)#"a" from Lessells&Boag(# groups/individuals)
  tdata<-data.frame(data[yc], data[xc])

  tmpbb<-anova.lm(aov(tdata[,1]~tdata[,2], data=tdata))
  num.df<-tmpbb[1][1,1]
  denom.df<-tmpbb[1][2,1];
  MSa<-tmpbb[3][1,1]
  MSw<-tmpbb[3][2,1]
  tmp.outj<-data.frame(lapply(unstack(na.omit(tdata)), FUN=length))
  k<-(1/(a-1))*(sum(tmp.outj)-(sum(square(tmp.outj))/(sum(tmp.outj))))
      #tmp.outj is N subscript i from Lessells & Boag
  
  var.w<-MSw
  var.a<-(MSa-MSw)/(k)
  r<-var.a/(var.w + var.a) #r="r" from Lessells & Boag

#Below computes the 95% confidence interval of the repeatability, from Becker 1984 which references Bogyo and Becker 1963.  The normality of the repeatability doesn't have to be assumed, as with computing the standard error of the repeatability.
  low.F<-qf(alpha/2, num.df, denom.df, lower.tail=FALSE)
  up.F<-1/(qf(1-alpha/2, denom.df, num.df, lower.tail=TRUE))
  low.CI<-1-((k*MSw*low.F)/(MSa+MSw*(k-1)*low.F))
  up.CI<-1-((k*MSw*up.F)/(MSa+MSw*(k-1)*up.F))
list(ICC=r, LowerCI=low.CI, UpperCI=up.CI, N=a, k=k, varw=var.w, vara=var.a)
}
