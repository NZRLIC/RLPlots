#' LFzero
#'
#' @export
#' 
LFzero <- function(stock,source.dir,target.dir = source.dir)
{
  #Read file
  lfzero<-read.table(paste(source.dir,"/",stock,"LFzero.out",sep=""),header=T,as.is=T)
  
  #Plot obs v pred
  #Set graphics parameters
  PlotType(paste(target.dir, "/", stock, "LFzero", sep = ""), width = 170, height = 200)
  par(las=1,oma=c(1,1,1,1),mar=c(4,4.5,1,1),mgp=c(3.5,1,0))
  #plot
  plot(Nmale/1000~size,data=lfzero,type="l",lwd = PlotOptions$thin,ylab="Number (thousands)",xlab="",cex=0.8,ylim=c(0,max(lfzero$Nmale,lfzero$Nfemale,lfzero$Nfemmat)/1000))
  lines(Nfemale/1000~size,data=lfzero, lwd = PlotOptions$thin,lty=2)
  lines(Nfemmat/1000~size,data=lfzero, lwd = PlotOptions$thick,lty=2)
  legend("topright",legend=c("Males","Immature females","Mature females"),lwd=c(PlotOptions$thin,PlotOptions$thin,PlotOptions$thick),lty=c(1,2,2),bty="n",cex=0.8)  #  AS edit : replaced legend coordinates "70,max(lfzero$Nmale,lfzero$Nfemale,lfzero$Nfemmat)/1000"
  mtext("Size (mm TW)",1,2)
  #Texts
  if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Initial length structure"),side=1,line=0,outer=TRUE,cex=0.7)
  dev.off()
}
