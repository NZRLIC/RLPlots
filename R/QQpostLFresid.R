#' MQQ post LF resid
#' 
#' @export
#' 
QQpostLFresid<-function(stock,source.dir,target.dir = source.dir)
{
  LF1resid<-read.table(paste(source.dir,"/",stock,"LF1respost.out",sep=""),header=T)## AS edit ##[c(firstrow:lastrow),]
  LF2resid<-read.table(paste(source.dir,"/",stock,"LF2respost.out",sep=""),header=T)## AS edit ##[c(firstrow:lastrow),]
  LF3resid<-read.table(paste(source.dir,"/",stock,"LF3respost.out",sep=""),header=T)## AS edit ##[c(firstrow:lastrow),]
  #Make new window
  windows()
  #Set graphics parameters
  par(las=1,mfrow=c(3,1),oma=c(0,0,1,0),mar=c(5,5,1,1),lab=c(3,5,5),cex=0.8)
 
  QQpost(LF1resid,ylab="Standard residuals",useminresid=T,minresid=0.05)
  mtext("Male",side=3,adj=0,cex=0.8)
  QQpost(LF2resid,ylab="Standard residuals",useminresid=T,minresid=0)
  mtext("Immature female",side=3,adj=0,cex=0.8)
  QQpost(LF3resid,ylab="Standard residuals",useminresid=T,minresid=0.05)
  mtext("Mature female",side=3,adj=0,cex=0.8)

  savePlot(filename=paste(target.dir,"/",stock,"QQpostLFresid.wmf",sep=""),type="wmf",device=dev.cur())  
}
