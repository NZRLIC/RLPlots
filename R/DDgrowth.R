#' DDgrowth plots
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @param PlotOptions
#' @export
#' 
DDgrowth<-function(stock,source.dir,target.dir = source.dir,PlotOptions=.PlotOptions){  
  dat<-read.table(paste(source.dir,"/",stock,"DDgrowth.out",sep=""),header=T,as.is=T)
  size <- unique(dat$Size)
  if(length(size)>1) stop('>1 size \n')
  x<-dat$Year
  #Set graphics parameters
  PlotType(paste(target.dir, "/", stock, "DDgrowth", sep = ""), PlotOptions, width = 270, height = 200)
  par(las=1,oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(3,1,0)) 
  plot(dat$B.Bo,dat$maleinc,type="b",pch=21,ylim=range(dat$maleinc,dat$feminc),ylab="Growth increment",xlab="Depletion relative to total equilibrium biomass")
  #text(dat$B.Bo,dat$maleinc,labels=substring(x,3))
  points(dat$B.Bo,dat$feminc,type="b",pch=19)
  legend('topright',c('Female','Male'),lty=2,pch=c(19,21),bty='n')
  if(PlotOptions$Captions) {
    mtext(paste(source.dir," ",stock,": Growth increments at size",size),side=1,line=0,outer=TRUE,cex=0.7)
  }
  dev.off()
}
