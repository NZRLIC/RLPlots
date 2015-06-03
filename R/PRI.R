#' PRIResids.out
#' 
#' @export
#' 
PRI<-function(stock,source.dir,target.dir = source.dir, PlotOption = .PlotOptions){
  #Read file
  pri<-read.table(paste(source.dir,"/",stock,"PRIesids.out",sep=""),header=T,as.is=T)

  if(UsePeriod==TRUE){
    pri$x<-pri$Period
    xlab<-"Period"     
    prange1<-unique(pri$x[pri$Season==1])
    prange2<-unique(pri$x[pri$Season==2])
  }
  else {
    pri$x<-pri$Year
    xlab<-"Fishing year"
    prange1<-unique(pri$x[pri$Season==1])
    prange2<-unique(pri$x[pri$Season==2])
  }

  attach(pri)
  logObs<-log(Obs)
  pri$LB<-exp(logObs-Sigma)
  pri$UB<-exp(logObs+Sigma)
  
  #Set graphics parameters
  windows(12,4)

  #Plot obs v pred
  if(PRIOptions$ObsPred)
  { 
    #Set graphics parameters
     par(las=1,oma=c(1,1,0,1),mar=c(5,4,1,1),mfrow=c(1,2)) 
    #AW
    plot(Obs~x,data=pri[pri$Season==1,],pch=1,ylab="PRI index",
          xlim=range(pri$x),ylim=c(0,max(pri$UB)),xlab=xlab)
    lines(Pred~x,data=pri[pri$Season==1,],lwd=thin)
    for(p in prange1) lines(c(LB,UB)~c(p,p),data=pri[pri$x==p & pri$Season==1,],pch="-",type="o",lwd=thin)
    legend("topleft",legend=c("AW"),bty="n")

    #SS
    plot(Obs~x,data=pri[pri$Season==2,],pch=1, ylab="PRI index",
        xlim=range(pri$x),ylim=c(0,max(pri$UB)),xlab=xlab)
    lines(Pred~x,data=pri[pri$Season==2,],lwd=thin)
    for(p in prange2) lines(c(LB,UB)~c(p,p),data=pri[pri$x==p & pri$Season==2,],pch="-",type="o",lwd=thin)
    legend("topleft",legend=c("SS"),bty="n")

    if(Captions) 
    {
      mtext(paste(source.dir," ",stock,": Observed and predicted for PRI fits."),side=1,line=-0.7,outer=TRUE,cex=0.7)
      mtext(paste(" Points: Observed; Lines: Predicted  "),side=1,line=0,outer=TRUE,cex=0.7)
    }
    savePlot(filename=paste(target.dir,"/",stock,"PRIObsPred.wmf",sep=""),type=plottype,device=dev.cur())
  }

  #Residuals
  if(PRIOptions$Resid)
  {
    #Set graphics parameters
     windows(7,6)
    par(las=1,oma=c(1,1,1,1),mar=c(4,3.7,1,1),mgp=c(2.7,1,0))
    #Resids v period
    plot(StdRes~x,data=pri,pch=c(1,16)[Season],lwd=thin,ylab="Standardised residual",
         xlab=xlab)
    abline(h=0,lty=2)
    if(Captions) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residual for PRI fits"),side=1,line=-0.5,outer=TRUE,cex=0.7)
      mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
    }
    savePlot(filename=paste(target.dir,"/",stock,"PRIResid.wmf",sep=""),type=plottype,device=dev.cur())

    #Resids v predicted
    plot(StdRes~Pred,data=pri,pch=c(1,16)[Season],lwd=thin,ylab="Standardised residual",xlab="Predicted")
    abline(h=0,lty=2)
    if(Captions) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residual for PRI fits"),side=1,line=-0.5,outer=TRUE,cex=0.7)
      mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
    }
    savePlot(filename=paste(target.dir,"/",stock,"PRIResidPred.wmf",sep=""),type=plottype,device=dev.cur())

    #qq plot
    qqnorm(pri$StdRes,pch=c(1,16)[Season],xlab="Theoretical quantiles",ylab="Standardised residual",main="",las=1)
    abline(a=0,b=1)
    abline(h=quantile(pri$StdRes,p=c(5,25,50,75,95)/100),lty=2)#Plot quntiles of the stdevs
    if(Captions) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residual for PRI fits"),side=1,line=-0.5,outer=TRUE,cex=0.7)
      mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
    }
    savePlot(filename=paste(target.dir,"/",stock,"PRIQQ.wmf",sep=""),type=plottype,device=dev.cur())
  }  
  detach(pri)
}
