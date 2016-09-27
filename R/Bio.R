#' Biomass plots
#'
#' @param stock a label for the stock (e.g. CRA1)
#' @param source.dir the directory containing the ADMB output files
#' @param target.dir the directory to save the plots to
#' @export
#' 
Bio <- function(stock, source.dir, target.dir = source.dir)
{
  
    filename <- paste(source.dir, "/", stock, "Biomass.out", sep = "")
    
    if (file.exists(filename)) {
        biom <- read.table(filename, header = TRUE, as.is = TRUE)
    } else {
        stop(paste("can't find:", filename, "\n"))
    }
    
  bm <- biom[biom$Period <= PlotOptions$ModelEndPeriod,]
  
  if( PlotOptions$UsePeriod == TRUE )
  {
    bm$x<-bm$Period
    xlab<-"Period"
  } else {
    bm$x<-PeriodToFishingYear(bm$Period)
    xlab<-"Fishing year"
  }


  # ==============================================================================
  # Plot Blegal 
  # ==============================================================================
  if( BioOptions$VulnB )
  {
    PlotType(paste(target.dir, "/", stock, "Bvulnref", sep = ""),
             width = PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
    par(las = 1, oma = c(1,0,0,0), mar = c(5,4,1,1), cex = 3)
    plot(Bvulnref/1000 ~ Year, data = bm[bm$Season==1,], type = "l", lwd = PlotOptions$thin,
         ylab = "Bvuln (thousands t)", xaxt = "s", xlim = range(Year), ylim = c(0,max(Bvulnref/1000)), xlab = xlab)
    lines(Bvulnref/1000~Year,data=bm[bm$Season==2,],type="l",lwd = PlotOptions$thick)
    legend("top",legend=c("AW","SS"),lwd=c(PlotOptions$thin, PlotOptions$thick), bty = "n", cex = 1) ## AS edit
    #mtext(xlab,1,2)
    if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Vulnerable Biomass"),side=1,line=0,outer=TRUE,cex=2)
    dev.off()
  }

  # ==============================================================================
  # Plot Brect
  # ==============================================================================
  if ( BioOptions$RecB )
  {
    PlotType(paste(target.dir, "/", stock, "Brect", sep = ""),
             width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
    par(mfrow = c(1,2), oma = c(2,1,1,1), mar = c(4,3,1,1), mgp = c(4,1,0), cex = 3)
    #Plot Brect1, Brect2, Brect3, Brectd
    for ( Season in 1:2 )
    {
      plot(Brect1/1000 ~ Year, data = bm[bm$Season==Season,], type = "l",
           lwd = PlotOptions$thin, ylab = "", xlim = range(Year),
           ylim = c(0,max(bm$Brectd/1000)), xlab = "", las = 1)
      lines(Brect2/1000~Year,data=bm[bm$Season==Season,],lty=2,lwd = PlotOptions$thin)
      lines(Brect3/1000~Year,data=bm[bm$Season==Season,],lty=2,lwd = PlotOptions$thick)
      lines(Brectd/1000~Year,data=bm[bm$Season==Season,],lty=1,lwd = PlotOptions$thick)
      if( Season == 1 )
      {
          mtext("Recruited biomass (thousands t)", side = 2, line = 3, cex = 3)
          legend("topright",
                 legend = c("Males","Immature females","Mature females","Total"),
                 lwd = c(PlotOptions$thin, PlotOptions$thin, PlotOptions$thick, PlotOptions$thick),
                 lty = c(1,2,2,1), bty = "n", cex = 0.7)
      }
      mtext(c("AW","SS")[Season], side = 3, line = 0.1, adj = 0, cex = 2.5)
    }
    mtext(xlab, side = 1, line = -1, outer = TRUE, cex = 3)
    if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Recruited biomass"),
                                      side = 1, line = 1, outer = TRUE, cex = 2)
    dev.off()
  }

  # ==============================================================================
  # Plot Btotal
  # ==============================================================================  
  if ( BioOptions$TotalRecB )
  {
    PlotType(paste(target.dir, "/", stock, "Btotal", sep = ""),
             width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
    par(mfrow = c(1,2), oma = c(2,1,1,1), mar = c(4,3,1,1), mgp = c(4,1,0), cex = 3)
    for ( Season in 1:2 )
    {
      #Plot Ballmale, Ballfemale, Ballfemmat, Balltotal 
      plot(Ballmale/1000 ~ Year, data = bm[bm$Season==Season,], type = "l",
           lwd = PlotOptions$thin, ylab = "",
           xlim = range(Year), ylim = c(0,max(bm$Balltotal/1000)), xlab = "", las = 1)
      lines(Ballfemale/1000~Year,data=bm[bm$Season==Season,],lty=2,lwd = PlotOptions$thin)
      lines(Ballfemmat/1000~Year,data=bm[bm$Season==Season,],lty=2,lwd = PlotOptions$thick)
      lines(Balltotal/1000~Year,data=bm[bm$Season==Season,],lty=1,lwd = PlotOptions$thick)
      if ( Season == 1 )
      {
        mtext("Total biomass (thousands t)", side = 2, line = 3, cex = 3)
        legend("topright", lty = c(1,2,2,1), bty = "n", cex = 0.7,
               legend = c("Males","Immature females","Mature females","Total"),
               lwd = c(PlotOptions$thin, PlotOptions$thin, PlotOptions$thick, PlotOptions$thick))
      }
      mtext(c("AW","SS")[Season], side = 3, line = 0.1, adj = 0, cex = 2.5)
    }
    mtext(xlab, side = 1, line = -1, outer = TRUE, cex = 3)
    if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Total biomass"),
                                      side = 1, line = 1, outer = TRUE, cex = 2)
    dev.off()
  }

}
