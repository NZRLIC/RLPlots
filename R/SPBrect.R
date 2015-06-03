#' Plot of surplus production versus biomass
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @param PlotOptions
#' @export
#' 
SPBrect <- function(stock,source.dir,target.dir = source.dir, PlotOptions = .PlotOptions)
{  
  SP<-read.table(paste(source.dir,"/",stock,"SP.out",sep=""),header=T,as.is=T)
  data<-read.table(paste(source.dir,"/",stock,"Biomass.out",sep=""),header=T,as.is=T)
#  AW<-seq(1,nrow(data)-3,2)
#  SS<-seq(2,nrow(data)-2,2)
#  dataAV<-(data[AW,]+data[SS,])/2
   dataAV<-data[data$Season==1,] 
#  x<-1945+floor((dataAV$Period-1)/2)
   x<-seq(min(data$Year),max(data$Year),1)
  #Set graphics parameters
  PlotType(paste(target.dir, "/", stock, "SPBrect", sep = ""), PlotOptions, width = 170, height = 200)
  par(las=1,oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(3,1,0))
#  plot(dataAV$Brectd,SP$SP[1:(nrow(SP)-1)],type="b",pch=" ",xlim=range(dataAV$Brectd),ylim=range(SP$SP[1:(nrow(SP)-1)]),ylab="Surplus production ",xlab="Recruited biomass")
#  text(dataAV$Brectd,SP$SP[1:(nrow(SP)-1)],labels=substring(x,3))
  plot(dataAV$Brectd/1000,SP$SP/1000,type="b",pch=" ",xlim=range(dataAV$Brectd/1000),ylim=range(SP$SP/1000),ylab="Surplus production (thousands t) ",xlab="Recruited biomass (beginning year)")
  text(dataAV$Brectd/1000,SP$SP/1000,labels=substring(x,3))
  dev.off()
}
