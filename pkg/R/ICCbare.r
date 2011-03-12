 ICCbare<-function(x, y, data){
  ICCcall <- Call <-match.call()
  xc<-as.character(ICCcall[[2L]])
  yc<-as.character(ICCcall[[3L]])
  tmp1<-aggregate(data[yc][[1]], list(data[xc][[1]]),FUN=mean)
  tmp2<-aggregate(data[yc][[1]], list(data[xc][[1]]), FUN=length)
  ord.data<-data[order(data[xc]),]
  Treat.m<-rep(tmp1$x,tmp2$x)
  Among<-(Treat.m-rep(mean(data[yc][[1]]),nrow(data[yc][1])))^2
  Within<-(ord.data[yc][[1]]-Treat.m)^2
  MS<-(c(sum(Among),sum(Within)))/(c(length(tmp2$x)-1, length(tmp2$x)*(tmp2$x[1]-1)))
  var.a<-(MS[1]-MS[2])/tmp2$x[1]
list(ICC=var.a/(var.a+MS[2]))
 }
