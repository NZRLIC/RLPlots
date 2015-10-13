#' POO plots
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @export
#' 
POO <- function(stock, source.dir, target.dir = source.dir)
{
    po <- read.table(paste(source.dir,"/",stock,"PooResids.out",sep=""),header=T,as.is=T)
    if ( CROptions$IsAnnual )
    {
        po$x <- po$Year
        xlab <- "Fishing year"
    } else {
        po$x <- po$Period
        xlab <- "Period"
    } 
    po$LB <- exp(log(po$Obs) - po$Sigma)
    po$UB <- exp(log(po$Obs) + po$Sigma)

    # Plot obs v pred
    if ( CROptions$ObsPred )
    {
        p <- ggplot(data = po) +
            geom_pointrange(aes(x = Year, y = Obs, ymin = LB, ymax = UB)) +
            geom_line(aes(x = Year, y = Pred)) +
            theme_lobview(PlotOptions) +
            labs(x = "\nFishing year", y = "Puerulus index\n")
        if ( PlotOptions$Captions )
        {
            p <- p + ggtitle(paste(source.dir, " ", stock, ": Observed and predicted for POO fits. Points: Observed; Lines: Predicted")) +
                theme(plot.title = element_text(size = 9, vjust = 2.7))
        }
        PlotType(paste(target.dir, "/", stock, "PooObsPred", sep = ""), width = 270, height = 200)
        print(p)
        dev.off()
    }

    # Residuals
    if ( CROptions$Resid )
    {
        #Set graphics parameters
        PlotType(paste(target.dir, "/", stock, "POResid", sep = ""), width = 270, height = 200)
        par(las=1,oma=c(1,1,1,1),mar=c(4,3.5,1,1),mgp=c(2.5,1,0))
        #Resids v period
        plot(StdRes~x,data=po,pch=16,lwd=PlotOptions$thin,ylab="Standardised residual",xlab=xlab)
        abline(h = 0, lty = 2)
        if ( PlotOptions$Captions ) 
        {
            mtext(paste(source.dir," ",stock,": Standardised residuals for POO fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
        }
        dev.off()
        
        #Resids v predicted
        PlotType(paste(target.dir, "/", stock, "POOResidPred", sep = ""), width = 270, height = 200)
        plot(StdRes~Pred,data=po,pch=16,lwd=PlotOptions$thin,ylab="Standardised residual",xlab="Predicted")
        abline(h=0,lty=2)
        if ( PlotOptions$Captions )
        {
            mtext(paste(source.dir," ",stock,": Standardised residual for POO fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
        }
        dev.off()
    }
}
