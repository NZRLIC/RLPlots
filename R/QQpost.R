#' QQpost
#' 
#' function to make QQ plot from posterior
#' 
#' @export
#' 
QQpost<-function(data, ylab = "Sample quantiles", xlab = "Theoretical quantiles", useminresid = FALSE, minresid = 0)
{
  temp0<-matrix(NA,nrow(data),ncol(data))
  temp1<-as.matrix(data)
  for(i in 1:nrow(data))
  {
    temp0[i,]<-temp1[i,order(temp1[i,])]
  }
  temp2<-matrix(NA,3,ncol(temp0))
  for (i in 1:ncol(temp0))
  {
    temp2[,i]<-quantile(temp0[,i],prob=c(0.05,0.5,0.95))
  }
  temp3<-rep(TRUE,ncol(temp0))
  if(useminresid)    temp3<-abs(temp2[2,])>minresid
  x<-qnorm(ppoints(ncol(temp2[,temp3])))
  plot(x,temp2[2,temp3],type="l",ylab=ylab,xlab=xlab)
  lines(x,temp2[1,temp3],lty=2)
  lines(x,temp2[3,temp3],lty=2)
  abline(a=0,b=1)
}
