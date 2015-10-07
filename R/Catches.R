#' Catch plots
#' 
#' Plots the fit of estimated and observed catches for size-limited (legal) and non-size-limited (illegal) catches
#'
#' @author Darcy Webber and Charles Edwards
#' @param stock character string: label for the stock (e.g. CRA1)
#' @param source.dir character string: the directory containing the ADMB output files
#' @param target.dir character string: the directory to save the plots to
#' @param PlotOptions list: plot options
#' @export
#' 
Catches <- function(stock,source.dir,target.dir = source.dir, PlotOptions = .PlotOptions)
{
    #Read file
    catches <- read.table(paste(source.dir,"/",stock,"Catches.out",sep = ""), header = TRUE, as.is = TRUE)
    
    if ( PlotOptions$UsePeriod==TRUE )
    {
        catches$x<-catches$Period
        xlab<-"Period"
    } else {
        catches$x<-catches$Year
        xlab<-"Fishing year"
    }
  
    # ==============================================================================
    # Plot SLC and NSLC
    # ==============================================================================
    PlotType(paste(target.dir, "/", stock, "Catch", sep = ""), PlotOptions, width = 300, height = 10+150)
    par(las=1,oma = c(4,1,1,1), mar = c(3,4,1,1),mfrow=c(1,2))
    plot(P_SLC~x,data=catches[catches$Season==2,],type="l",
         lwd = PlotOptions$thick,ylim=c(0,max(catches$SLC)),ylab="SL Catch (t)", xlab = "")
    points(SLC~x,data=catches[catches$Season==2,],pch=19)
    lines(P_SLC~x,data=catches[catches$Season==1,],lwd = PlotOptions$thin)
    points(SLC~x,data=catches[catches$Season==1,],pch=1)
    legend("top", legend = c("AW","SS"), lwd = c(PlotOptions$thin, PlotOptions$thick), bty = "n")
    
    plot(P_NSLC~x,data=catches[catches$Season==2,],type="l",
         lwd = PlotOptions$thick,ylim=c(0,max(catches$NSLC)),ylab="NSL Catch (t)",xlab= "")
    points(NSLC~x,data=catches[catches$Season==2,],pch=19)
    lines(P_NSLC~x,data=catches[catches$Season==1,], lwd = PlotOptions$thin)
    points(NSLC~x,data=catches[catches$Season==1,],pch=1)
    if ( PlotOptions$Captions ) {
        mtext(paste(source.dir," ",stock,": Observed and predicted for Catch fits."),side=1,line=2,outer=TRUE,cex=0.7)
        mtext(paste(" Points: Observed; Lines: Predicted  "),side=1,line=3,outer=TRUE,cex=0.7)
    }
    mtext(xlab, side = 1, line = -0.5, outer = TRUE)
    dev.off()
}
