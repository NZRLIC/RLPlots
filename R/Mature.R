#' Maturity
#' 
#' @export
#' 
Mature <- function(stock,source.dir,target.dir = source.dir, PlotOptions = .PlotOptions)
{
  #Read file
  mature<-read.table(paste(source.dir,"/",stock,"Maturity.out",sep=""),header=T,as.is=T)
  #Set graphics parameters
  PlotType(paste(target.dir, "/", stock, "Maturity", sep = ""), PlotOptions, width = 170, height = 200)
  par(las=1,oma=c(1,1,1,1),mar=c(4,4.5,1,1),mgp=c(2.5,1,0))

  #Maturity plot
  plot(Maturity~size,data=mature,type="l", lwd = PlotOptions$thin, xlab="",ylab="Maturation rate ",xaxt="s",ylim=c(0,max(mature$Maturity)))  
  mtext("Size (mm TW)",1,2)
  
  if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Maturity"),side=1,line=0,outer=TRUE,cex=0.7)
  dev.off()
}
