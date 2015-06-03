#' POO plots
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @param CROptions
#' @param PlotOptions
#' @export
#' 
POO<-function(stock,source.dir,target.dir = source.dir,CROptions,PlotOptions){
  #Read file
  po<-read.table(paste(source.dir,"/",stock,"PooResids.out",sep=""),header=T,as.is=T)
  if(CROptions$IsAnnual)
  {
    po$x<-po$Year
    xlab<-"Fishing year"
  } else
  {
    po$x<-po$Period
    xlab<-"Period"
  } 
  prange1<-unique(po$x)

  attach(po)
  logObs<-log(Obs)
  logPred<-log(Pred)
  po$LB<-exp(logObs-Sigma)
  po$UB<-exp(logPred+Sigma)

  #Plot obs v pred
  if(CROptions$ObsPred)
  { 
    #Set graphics parameters
    PlotType(paste(target.dir, "/", stock, "PooObsPred", sep = ""), PlotOptions, width = 270, height = 200)
    par(las=1,oma=c(1,1,1,1),mar=c(5,4,1,1))
#    plot(Obs~x,data=po,pch=16,ylab="Puerulus Index",xlim=range(po$x),ylim=c(0,max(po$UB)),xlab=xlab)
    plot(Obs~x,data=po,pch=16,ylab="Puerulus Index",xlim=range(po$x),ylim=c(0,6),xlab=xlab)
    lines(Pred~x,data=po,lwd=PlotOptions$thin)
       if(Captions) 
    {
      mtext(paste(source.dir," ",stock,": Observed and predicted for POO fits."),side=1,line=-0.7,outer=TRUE,cex=0.7)
      mtext(paste("Points: Observed; Lines: Predicted"),side=1,line=0,outer=TRUE,cex=0.7)
    }
    dev.off()
  }

  #Residuals
  if(CROptions$Resid)
  {
    #Set graphics parameters
    PlotType(paste(target.dir, "/", stock, "POResid", sep = ""), PlotOptions, width = 270, height = 200)
    par(las=1,oma=c(1,1,1,1),mar=c(4,3.5,1,1),mgp=c(2.5,1,0))
    #Resids v period
    plot(StdRes~x,data=po,pch=16,lwd=PlotOptions$thin,ylab="Standardised residual",xlab=xlab)
    abline(h=0,lty=2)
    if(Captions) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residuals for POO fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
    }
    dev.off()
    
    #Resids v predicted
    PlotType(paste(target.dir, "/", stock, "POOResidPred", sep = ""), PlotOptions, width = 270, height = 200)
    plot(StdRes~Pred,data=po,pch=16,lwd=PlotOptions$thin,ylab="Standardised residual",xlab="Predicted")
    abline(h=0,lty=2)
    if(Captions) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residual for POO fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
    }
    dev.off()
  }
  detach(po)
}
