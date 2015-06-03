#' Recruitment plots
#'
#' @export
#' 
Rect <- function(stock,source.dir,target.dir = source.dir, PlotOptions = .PlotOptions)
{
  recall <- read.table(paste(source.dir,"/",stock,"Recruitment.out",sep=""),header=T,as.is=T)
  rec <- recall[recall$Period <= PlotOptions$ModelEndPeriod,]
  if ( PlotOptions$UsePeriod == TRUE )
  {
    rec$x<-rec$Period
    xlab<-"Period"
  } else {
   rec$x<-PeriodToFishingYear(rec$Period)
   xlab<-"Fishing year"
  }
  attach(rec)
  Recr<-Recruitment/1000000;
  
  PlotType(paste(target.dir, "/", stock, "Rect", sep = ""), PlotOptions, width = 170, height = 200)
  par(las=1,oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(3,1,0))
  
  #Plot Erate vs Period
  plot(Recruitment/100000~Year,data=rec[rec$Season==1,],pch=16,type="o", lwd = PlotOptions$thin,xlab=xlab,ylab="Recruitment(million animals) ",cex=0.6,xaxt="s",
       xlim=range(Year))
  if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Recruitment plot"),side=1,line=0,outer=TRUE,cex=0.7)
  dev.off()
  detach(rec)
}
