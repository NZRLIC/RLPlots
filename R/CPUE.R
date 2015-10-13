#' CPUE fit plots
#' 
#' Plots the MPD fit to CPUE data and residual diagnostics
#'
#' @author Darcy Webber and Charles Edwards
#' @param stock character string: label for the stock (e.g. CRA1)
#' @param source.dir character string: the directory containing the ADMB output files
#' @param target.dir character string: the directory to save the plots to
#' @export
#' 
CPUE <- function(stock, source.dir, target.dir = source.dir)
{
    
  #Read file
  cpue <- read.table(paste(source.dir,"/",stock,"CPUEResids.out",sep=""),header=T,as.is=T)

  if ( PlotOptions$UsePeriod )
  {
    cpue$x<-cpue$Period
    xlab<-"Period"     
  } else {
    cpue$x<-PeriodToFishingYear(cpue$Period)
    xlab<-"Fishing year"
  }

  prange1<-unique(cpue$Year[cpue$Season==1])
  prange2<-unique(cpue$Year[cpue$Season==2])

  attach(cpue)
  logObs<-log(Obs)
  cpue$LB<-exp(logObs-Sigma)
  cpue$UB<-exp(logObs+Sigma)
  
  # ==============================================================================
  # Plot obs vs pred
  # ==============================================================================
  if ( CPUEOptions$ObsPred )
  { 
    PlotType(paste(target.dir, "/", stock, "CPUEObsPred", sep = ""), width = 300, height = 10+150)
    par(las=1,oma = c(4,1,1,1), mar = c(3,4,1,1),mfrow=c(1,2)) 
    #AW
    plot(Obs~Year,data=cpue[cpue$Season==1,],pch=1,ylab="CPUE (kg/potlift)",
          xlim=range(Year),ylim=c(0,max(cpue$UB)),xlab="")
    lines(Pred~Year,data=cpue[cpue$Season==1,],lwd=PlotOptions$thin)
    for(p in prange1) lines(c(LB,UB)~c(p,p),data=cpue[cpue$Year==p & cpue$Season==1,],pch="-",type="o",lwd=PlotOptions$thin)
    legend("topleft",legend=c("AW"),lwd=c(PlotOptions$thin,PlotOptions$thick),pch=c(1),bty="n")

    #SS
    plot(Obs~Year,data=cpue[cpue$Season==2,],pch=16,ylab="",
          xlim=range(Year),ylim=c(0,max(cpue$UB)),xlab="")
    lines(Pred~Year,data=cpue[cpue$Season==2,],lwd=PlotOptions$thick)
    for(p in prange2) lines(c(LB,UB)~c(p,p),data=cpue[cpue$Year==p & cpue$Season==2,],pch="-",type="o",lwd=PlotOptions$thick)
    legend("topleft",legend=c("SS"),lwd=c(PlotOptions$thin,PlotOptions$thick),pch=c(16),bty="n")
    
    if ( PlotOptions$Captions ) 
    {
      mtext(paste(source.dir," ",stock,": Observed and predicted for CPUE fits."),side=1,line=2,outer=TRUE,cex=0.7)
      mtext(paste(" Points: Observed; Lines: Predicted  "),side=1,line=3,outer=TRUE,cex=0.7)
    }
    mtext(xlab, side = 1, line = -0.5, outer = TRUE)
    dev.off()
  }
  
  # ==============================================================================
  # Residuals
  # ==============================================================================
  if ( CPUEOptions$Resid )
  {
    PlotType(paste(target.dir, "/", stock, "CPUEResid", sep = ""), width = 150, height = 10+150)
    par(las=1,oma=c(2,1,1,1),mar=c(5,4,1,1),mgp=c(3,1,0)) 
    #Resids v period
    plot(StdRes~Year,data=cpue,pch=c(1,16)[Season],lwd=PlotOptions$thin,ylab="Standardised residual",
         xlab=xlab)
    abline(h=0,lty=2)
    if ( PlotOptions$Captions )
    {
      mtext(paste(source.dir," ",stock,": Standardised residual for CPUE fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
      mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
    }
    dev.off()
    
    #Resids v predicted
    PlotType(paste(target.dir, "/", stock, "CPUEResidPred", sep = ""), width = 150, height = 10+150)
    par(las=1,oma=c(2,1,1,1),mar=c(5,4,1,1),mgp=c(3,1,0)) 
    plot(StdRes~Pred,data=cpue,pch=c(1,16)[Season],lwd=PlotOptions$thin,ylab="Standardised residual",xlab="Predicted")
    abline(h=0,lty=2)
    if ( PlotOptions$Captions ) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residual for CPUE fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
      mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
    }
    dev.off()
    
    #qq plot
    PlotType(paste(target.dir, "/", stock, "CPUEQQ", sep = ""), width = 150, height = 10+150)
    par(las=1,oma=c(2,1,1,1),mar=c(5,4,1,1),mgp=c(3,1,0)) 
    qqnorm(cpue$StdRes,pch=c(1,16)[Season],xlab="Theoretical quantiles",ylab="Standardised residual",main="",las=1)
    abline(a=0,b=1)
    abline(h=quantile(cpue$StdRes,p=c(5,25,50,75,95)/100),lty=2)#Plot quantiles of the stdevs
    if ( PlotOptions$Captions ) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residual for CPUE fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
      mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
    }
    dev.off()
  }  
  detach(cpue)
  
}
