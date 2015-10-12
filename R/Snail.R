#' Snail trail plots
#'
#' @param stock a label for the stock (e.g. CRA1)
#' @param source.dir the directory containing the ADMB output files
#' @param target.dir the directory to save the plots to
#' @param PlotOptions plot options
#' @export
#' 
Snail <- function(stock, source.dir = ".", target.dir = source.dir, PlotOptions = .PlotOptions)
{
  
    qsnail.all <- read.table(paste(source.dir, "/snail.out", sep = ""), header = TRUE, as.is = TRUE)
  
  for (ss in 1:length(stock)) {
      
      qsnail <- subset(qsnail.all, region == ss)
      
  qminyr <- min(qsnail$year)
  qmaxyr <- max(qsnail$year)
  qyrs <- qmaxyr-qminyr+1
  qouts <- matrix(0, nrow = qyrs, ncol = 2)
  #ireg=1
  #qsnail <- qsnail[qsnail$region==ireg,]
  for (ii in 1:qyrs)
  {
     qy<-qminyr+ii-1
     qtmp<-qsnail[qsnail$year==qy,]
     qouts[ii,1]<-median(qtmp$SSB.y..SSB0)
     qouts[ii,2]<-median(qtmp$Fmult.Fmsy)
  }
  rownames(qouts) <- qminyr:qmaxyr
  colnames(qouts) <- c('SSB/SSB0','F/Fmsy')
  qymax<-1.1*max(qouts[,2])
  qxmax<-1.1*max(qouts[,1])

  qx1 <- quantile(qtmp$SSBmsy.SSB0,c(0.05,0.05,0.95,0.95))
  qy1 <- c(-0.1,1.05*qymax,1.05*qymax,-0.1)

  qtmp<-qsnail[qsnail$year==qmaxyr,]
  qx<-quantile(qtmp$SSB.y..SSB0,c(0.05,0.95))
  qy<-quantile(qtmp$Fmult.Fmsy,c(0.05,0.95))
  
  if (qx[1] == qx[2]) warning(paste(stock[ss], ": Upper and lower bounds for final year SSB/SSB0 are identical\n"))
  if (qy[1] == qy[2]) warning(paste(stock[ss], ": Upper and lower bounds for final year F/Fmsy are identical\n"))

    dat <- data.frame(qouts)
    names(dat) <- c("x","y")
    dat2 <- data.frame(qx1, qy1)
  
    # Do the plot
    PlotType(paste(target.dir, "/", stock[ss], "Snail", sep = ""), PlotOptions,
             width = PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])

    #ggplot() +
        #geom_rect(data = dat2, aes(x = qx, y = qy)) +
        #geom_rect(xmin = qx1[1], xmax = qx1[2], ymin = qy1[1], ymax = qy1[2]) +
        #geom_hline(yintercept = 1) +
        #geom_vline(xintercept = median(qtmp$SSBmsy.SSB0)) +
          #scale_colour_gradient(low = "green", high = "red") +
          #geom_path(data = qsnail, aes(x = SSB.y..SSB0, y = Fmult.Fmsy, group = trial, colour = year), alpha = 0.05) +
          #geom_point(data = dat, aes(x = x, y = y)) +
          #geom_path(data = dat, aes(x = x, y = y)) +
          #theme_lobview(PlotOptions) +
          #labs(x = "\nSSB/SSB0", y = "Fishing intensity (F/Fmsy)\n")
  
  plot(qouts[,1],qouts[,2],type='n',ylim=c(0,qymax),xlim=c(0,qxmax),pch=16, xlab="SSB/SSB0",ylab="Fishing Intensity (F/Fmsy)",cex.lab=1.0)
  polygon(qx1,qy1,col='light grey',border=NA)
  abline(h=1,lwd=1.2,lty=1.2,col='dark grey')
  abline(v=median(qtmp$SSBmsy.SSB0),lwd=1.2,lty=1,col='dark grey')
  box()
  points(qouts[,1],qouts[,2],type='l',pch=16, col="blue",lwd=1.3)
  points(qouts[,1],qouts[,2],type='p',pch=16, cex=1.0)
  # insert year labels
  text(qouts[as.character(qminyr),1],qouts[as.character(qminyr),2],qminyr,cex=0.8,pos=4)
  text(qouts['1975',1],qouts['1975',2],'1975',cex=0.8,pos=1)
  text(qouts['1980',1],qouts['1980',2],'1980',cex=0.8,pos=4)
  text(qouts['1990',1],qouts['1990',2],'1990',cex=0.8,pos=2)
  text(qouts['2000',1],qouts['2000',2],'2000',cex=0.8,pos=3)
  segments(qx[1],qouts[qyrs,2],qx[2],qouts[qyrs,2],lwd=1.5)
  segments(qouts[qyrs,1],qy[1],qouts[qyrs,1],qy[2],lwd=1.5)
  
    #if ( PlotOptions$Captions )
    #{
    #    p <- p + ggtitle(paste(source.dir, " ", stock, ": Snail trail plot")) +
    #        theme(plot.title = element_text(size = 9, vjust = 2.7))
    #}
    
    #print(p)
    dev.off()
  }
} 
