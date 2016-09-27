#' CR plots
#' 
#' Plots the MPD fit to historic catch rate data and residual diagnostics
#'
#' @author Darcy Webber and Charles Edwards
#' @param stock character string: label for the stock (e.g. CRA1)
#' @param source.dir character string: the directory containing the ADMB output files
#' @param target.dir character string: the directory to save the plots to
#' @export
#' 
CR <- function(stock, source.dir, target.dir = source.dir)
{
    #Read file
    cr<-read.table(paste(source.dir,"/",stock,"CRResids.out",sep=""),header=T,as.is=T)
    if(CROptions$IsAnnual)
    {
        cr$x<-cr$Year
        xlab<-"Fishing year"
    } else {
        cr$x<-cr$Period
        xlab<-"Period"
    } 
    prange1<-unique(cr$x)

    attach(cr)
    logObs<-log(Obs)
    cr$LB<-exp(logObs-Sigma)
    cr$UB<-exp(logObs+Sigma)
  
    #Set graphics parameters
    #Plot obs v pred
    if(CROptions$ObsPred)
    { 
        PlotType(paste(target.dir, "/", stock, "CRObsPred", sep = ""), width = 170, height = 200)
        par(las=1,oma=c(1,1,1,1),mar=c(5,4,1,1), cex=3)
        plot(Obs~x,data=cr,pch=16,ylab="Catch rates (kg/day)",xlim=range(cr$x),ylim=c(0,max(cr$UB)),xlab=xlab)
        lines(Pred~x,data=cr,lwd = PlotOptions$thin)
        # vh : comment out followng line cause it doens't work with 2 periods/yr
        #for(p in prange1) lines(c(LB,UB)~c(p,p),data=cr[cr$x==p,],pch="-",type="o",lwd=thin)
        if ( PlotOptions$Captions ) 
        {
            mtext(paste(source.dir," ",stock,": Observed and predicted for CR fits."),side=1,line=-0.7,outer=TRUE,cex=2)
            mtext(paste("Points: Observed; Lines: Predicted"),side=1,line=0,outer=TRUE,cex=2)
        }
        dev.off()
    }
    
    #Residuals
    if( CROptions$Resid )
    {
        PlotType(paste(target.dir, "/", stock, "CRResid", sep = ""), width = 170, height = 200)
        par(las=1,oma=c(1,1,1,1),mar=c(4,3.5,1,1),mgp=c(2.5,1,0),cex=3)
        #Resids v period
        plot(StdRes~x,data=cr,pch=16,lwd = PlotOptions$thin,ylab="Standardised residual",xlab=xlab)
        abline(h=0,lty=2)
        if ( PlotOptions$Captions ) 
        {
            mtext(paste(source.dir," ",stock,": Standardised residual for CR fits"),side=1,line=-0.7,outer=TRUE,cex=2)
        }
        dev.off()
        
        #Resids v predicted
        PlotType(paste(target.dir, "/", stock, "CRResidPred", sep = ""), width = 170, height = 200)
        plot(StdRes~Pred,data=cr,pch=16,lwd=PlotOptions$thin,ylab="Standardised residual",xlab="Predicted", cex = 3)
        abline(h=0,lty=2)
        if ( PlotOptions$Captions ) 
        {
            mtext(paste(source.dir," ",stock,": Standardised residual for CR fits"),side=1,line=-0.7,outer=TRUE,cex=2)
        }
        dev.off()

        #qq plot
        PlotType(paste(target.dir, "/", stock, "CRQQ", sep = ""), width = 170, height = 200)
        qqnorm(cr$StdRes,pch=16,xlab="Theoretical quantiles",ylab="Standardised residual",main="",las=1, cex = 3)
        abline(a=0,b=1)
        abline(h=quantile(cr$StdRes,p=c(5,25,50,75,95)/100),lty=2)#Plot quntiles of the stdevs
        if ( PlotOptions$Captions )
        {
            mtext(paste(source.dir," ",stock,": Standardised residual for CR fits"),side=1,line=-0.7,outer=TRUE,cex=2)
        }
        dev.off()
    }
    detach(cr)
}
