#' Plot tag fits
#' 
#' Tag plots when there are 2 tag files
#'
#' @export
#' 
Tag <- function(stock, source.dir, target.dir = source.dir)
{
    # Read files
    allFiles <- list.files(source.dir)
    i <- grep("TagResids.out", allFiles)
    Files <- allFiles[i]
    nFiles <- length(Files)
    # Combine data
    tag <- NULL
    for (I in 1:nFiles)
    {
        tmp <- read.table(paste(source.dir, "/", Files[I], sep = ""), header = TRUE, as.is = TRUE)
        tmp <- data.frame(file = I, tmp)
        tag <- rbind(tag, tmp)
    }
    tag2 <- 1
  
    nmoult <- (tag$perrec - tag$perrel)
    nmoult[tag$sex==2] <- 1+floor(nmoult[tag$sex==2]/2)-(tag$perrec[tag$sex==2]+1)%%2
    nmoult[tag$sex==2 & (tag$perrel+1) %% 2==(tag$perrec+1)%%2] <- (tag$perrec[tag$sex==2 & (tag$perrel+1)%%2==(tag$perrec+1)%%2]-tag$perrel[tag$sex==2 & (tag$perrel+1)%%2==(tag$perrec+1)%%2])/2
    #write.table(cbind(tag,nmoult),"moulttest.csv",sep=",",row.names=F)

    #tag$sex[tag$sex == 1] <- "Male"
    #tag$sex[tag$sex == 2] <- "Female"
    
  # ==============================================================================
  # StdResid vs Pred
  # ==============================================================================
  PlotType(paste(target.dir, "/", stock, "TagResid", sep = ""),
           width = 2 * PlotOptions$plotsize[1],
           height = 10 + nFiles * PlotOptions$plotsize[2])

    #ggplot(data = tag, aes(x = size2Pred, y = StdRes)) +
    #    geom_point() + facet_grid(.~sex) + labs()
    
  par(mfrow = c(nFiles,2), oma = c(2,2,1,1), mar=c(4,2,1,1), mgp=c(4,1,0))
  for ( ff in 1:nFiles )
  {
      for( sex in 1:2 )
      {
          if ( sex==1 )
          {
              xlim <- TagOptions$xlim.male
          } else {
              xlim <- TagOptions$xlim.female
          }
          plot(StdRes ~ size2Pred, data = tag[tag$sex==sex & tag$file==ff,],
               pch = 1, ylab = "", xlab = "", xaxt = "s", cex = 0.7, xlim = xlim, las = 1)
          abline(h = 0, lty = 2)
          mtext(c("Male","Female")[sex], side = 3, line = 0.1, las = 1, adj = 0)
      }
  }
  mtext("Predicted", side = 1, line = -1, outer = TRUE)
  mtext("Standardised residual", side = 2, line = 1, outer = TRUE)
  if ( PlotOptions$Captions) mtext(paste(source.dir," ",stock,": Tags Plot"),
                                   side = 1, line = 1, outer = TRUE, cex = 0.7)
  dev.off()
  
  # ==============================================================================
  # Plot obs vs pred
  # ==============================================================================
  PlotType(paste(target.dir, "/", stock, "TagPredObs", sep = ""),
           width = 2 * PlotOptions$plotsize[1],
           height = 10 + nFiles * PlotOptions$plotsize[2])
  par(mfrow = c(nFiles,2), oma = c(2,2,1,1), mar=c(4,2,1,1), mgp=c(4,1,0))
  for ( ff in 1:nFiles )
  {
      for ( sex in 1:2 )
      {
        if ( sex == 1 )
        {
            xlim <- TagOptions$xlim.male
        } else {
            xlim <- TagOptions$xlim.female
        }
        plot(size2Pred~size2Obs,data=tag[tag$sex==sex & tag$file==ff,],
             pch = 1, ylab = "", xlab = "", xaxt = "s", xlim = xlim, cex = 0.7, las = 1)
        abline(a=0,b=1)
        mtext(c("Male","Female")[sex], side = 3, line = 0.1, las = 1, adj = 0)
      }
  }
  mtext("Observed", side = 1, line = -1, outer = TRUE)
  mtext("Predicted", side = 2, line = 1, outer = TRUE)
  if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Increment vs size at release plot"),
                                    side = 1, line = 1, outer = TRUE, cex = 0.7)
  dev.off()
  
  # ==============================================================================
  # QQ normal plots by sex
  # ==============================================================================
  PlotType(paste(target.dir, "/", stock, "TagQQ", sep = ""),
           width = 2 * PlotOptions$plotsize[1],
           height = 10 + nFiles * PlotOptions$plotsize[2])
  par(mfrow = c(nFiles,2), oma = c(2,2,1,1), mar=c(4,2,1,1), mgp=c(4,1,0))
  for ( ff in 1:nFiles )
  {
      for ( sex in 1:2 )
      {
          qqnorm(tag$StdRes[tag$sex==sex & tag$file==ff],xlab="",ylab="",main="",las=1)
          abline(a = 0, b = 1)
          abline(h=quantile(tag$StdRes,p=c(5,10,25,50,75,90,95)/100),lty=2)#Plot quntiles of the stdevs
          mtext(c("Male","Female")[sex],side=3,line=0.1,las=1,adj=0)
      }
  }
  mtext("Theoretical quantiles", side = 1, line = -1, outer = TRUE)
  mtext("Standardised residual", side = 2, line = 1, outer = TRUE)
  if ( PlotOptions$Captions) mtext(paste(source.dir," ",stock,": Tag resid Plot"),
                                   side = 1, line = 1, outer = TRUE, cex = 0.7)
  dev.off()
  
  # ==============================================================================
  # Size
  # ==============================================================================
  tag$sc <- floor(tag$size2Pred / 4) * 4 + 2
  if ( TagOptions$StdResSize )
  {
      PlotType(paste(target.dir, "/", stock, "TagResSize", sep = ""),
               width = 2 * PlotOptions$plotsize[1],
               height = 10 + nFiles * PlotOptions$plotsize[2])
      par(mfrow = c(nFiles,2), oma = c(2,2,1,1), mar=c(4,2,1,1), mgp=c(4,1,0))
      for (ff in 1:nFiles)
      {
          for ( sex in 1:2 )
          {
              nn <- nrow(tag[tag$sex == sex & tag$file == ff,])
              boxplot(StdRes ~ sc, data = tag[tag$sex==sex & tag$file==ff,],
                      ylab = "", xlab = "", line = 2, medlwd = 1, las = 1,
                      varwidth = TRUE)
              abline(h = 0, lty = 2)
              legend("top", legend = paste("N: ", nn, sep = ""), bty = "n")
              mtext(c("Male","Female")[sex], side = 3, line = 0.1,
                    las = 1, adj = 0)
          }
      }
      mtext("Size class", side = 1, line = -1, outer = TRUE)
      mtext("Standardised residual", side = 2, line = 1, outer = TRUE)
      if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Standardised residuals by 4mm size classes for each sex"),
                                        side = 1, line = 1, outer = TRUE, cex = 0.7)
      dev.off()
  }

  # ==============================================================================
  # Release Count
  # ==============================================================================
  if ( TagOptions$StdResRel )
  {
      PlotType(paste(target.dir, "/", stock, "TagResRel", sep = ""),
               width = 2 * PlotOptions$plotsize[1],
               height = 10 + nFiles * PlotOptions$plotsize[2])
      par(mfrow = c(nFiles,2), oma = c(2,2,1,1), mar=c(4,2,1,1), mgp=c(4,1,0))
      for (ff in 1:nFiles)
      {
          for(sex in 1:2)
          {
                    nn <- nrow(tag[tag$sex == sex & tag$file == ff,])
                    boxplot(StdRes~release,data=tag[tag$sex==sex & tag$file==ff,],
                            ylab="",xlab="",line=2, medlwd = 1, varwidth = TRUE, las = 1)
                    abline(h=0,lty=2)
                    legend("top", legend = paste("N: ", nn, sep = ""), bty = "n")
                    mtext(c("Male","Female")[sex],side=3,line=0.1,las=1,adj=0)
          }
          mtext("Release", side = 1, line = -1, outer = TRUE)
          mtext("Standardised residual", side = 2, line = 1, outer = TRUE)
      }
      if(PlotOptions$Captions) mtext(paste(source.dir," ",stock,": Standardised residuals by release number for each sex"),
                                     side = 1, line = 1, outer = TRUE, cex = 0.7)
    dev.off()
  }

  # ==============================================================================
  # Area
  # ==============================================================================
  if( TagOptions$StdResArea )
  {
      PlotType(paste(target.dir, "/", stock, "TagResArea", sep = ""),
               width = 2 * PlotOptions$plotsize[1],
               height = 10 + nFiles * PlotOptions$plotsize[2])
      par(mfrow = c(nFiles,2), oma = c(2,2,1,1), mar=c(4,2,1,1), mgp=c(4,1,0))
      for (ff in 1:nFiles)
      {
          for( sex in 1:2 )
          {
              nn <- nrow(tag[tag$sex == sex & tag$file == ff,])
              boxplot(StdRes ~ area, data = tag[tag$sex == sex & tag$file == ff,],
                      ylab = "", xlab = "", line = 2,
                      medlwd = 1, las = 1, varwidth = TRUE)
              abline(h = 0, lty = 2)
              legend("top", legend = paste("N: ", nn, sep = ""), bty = "n")
              mtext(c("Male","Female")[sex], side = 3,
                    line = 0.1, las = 1, adj = 0)
          }
      }
      mtext("Area", side = 1, line = -1, outer = TRUE)
      mtext("Standardised residual", side = 2, line = 1, outer = TRUE)
      if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Standardised residuals by area for each sex"),
                                        side = 1, line = 1, outer = TRUE, cex = 0.7)
      dev.off()
  }
    
  #Condition
  #if(TagOptions$StdResCond==TRUE){
  #  par(las=1,mfrow=c(2,2),oma=c(1,1,1,1),mar=c(3,3,1,1),mgp=c(2,1,0)) 
  #  for(sex in 1:2){
  #    boxplot(StdRes~cond,data=tag[tag$sex==sex & tag$file==1,],ylab="Standardised residual",xlab="Condition",line=2,medlwd=1)
  #    abline(h=0,lty=2)
  #    mtext(c("Male (File 1)","Female (File 1)")[sex],side=3,line=0.1,las=1,adj=0)
  #  }
  #for(sex in 1:2){
  #    boxplot(StdRes~cond,data=tag[tag$sex==sex & tag$file==2,],ylab="Standardised residual",xlab="Condition",line=2,medlwd=1)
  #    abline(h=0,lty=2)
  #    mtext(c("Male (File 2)","Female (File 2)")[sex],side=3,line=0.1,las=1,adj=0)
  #  }
  #  if(Captions) mtext(paste(source.dir," ",stock,": Standardised residuals by condition code for each sex"),side=1,line=0,outer=TRUE,cex=0.7)
  #  savePlot(filename=paste(target.dir,"/",stock,"TagResCond.wmf",sep=""),type=plottype,device=dev.cur())
  #}
  #Type
  #if(TagOptions$StdResType==TRUE){
  #  par(las=1,mfrow=c(2,2),oma=c(1,1,1,1),mar=c(3.5,3,1,1),mgp=c(2,1,0)) 
  #  for(sex in 1:2){
  #    type<-factor(c("NULL","WRL","SPYR","TP","HALL","Disc")[tag$type[tag$sex==sex]])
  #    boxplot(tag$StdRes[tag$sex==sex & tag$file==1]~type,ylab="Standardised residual",xlab="Type",line=2,medlwd=1)
  #    abline(h=0,lty=2)
  #    mtext(c("Male (File 1)","Female (File 1)")[sex],side=3,line=0.1,las=1,adj=0)
  #  }
  #  for(sex in 1:2){
  #    type<-factor(c("NULL","WRL","SPYR","TP","HALL","Disc")[tag$type[tag$sex==sex]])
  #    boxplot(tag$StdRes[tag$sex==sex & tag$file==2]~type,ylab="Standardised residual",xlab="Type",line=2,medlwd=1)
  #    abline(h=0,lty=2)
  #    mtext(c("Male (File 2)","Female (File 2)")[sex],side=3,line=0.1,las=1,adj=0)
  #  }
  #  if(Captions) mtext(paste(source.dir," ",stock,": Standardised residuals by tag type for each sex"),side=1,line=0,outer=TRUE,cex=0.7)
  #  savePlot(filename=paste(target.dir,"/",stock,"TagResType.wmf",sep=""),type=plottype,device=dev.cur())
  #}
  
  # ==============================================================================
  # Histograms by sex
  # ==============================================================================
  if(TagOptions$Reshist)
  {
      PlotType(paste(target.dir, "/", stock, "TagBHistSex", sep = ""),
               width = 2 * PlotOptions$plotsize[1],
               height = 10 + nFiles * PlotOptions$plotsize[2])
      par(mfrow = c(nFiles,2), oma = c(2,2,1,1), mar=c(4,2,1,1), mgp=c(4,1,0))
      for ( ff in 1:nFiles )
      {
          for( sex in 1:2 )
          {
              sub<-tag[tag$sex==sex & tag$file==ff,]
              brmin<-floor(min(sub$StdRes))
              brmax<-ceiling(max(sub$StdRes))
              by<-0.1
              br<-seq(brmin-by/2,brmax+by/2,by=by)
              aa<-hist(sub$StdRes,br=br,right=F,plot=F)
              plot(aa$mids,aa$counts/sum(aa$counts),xlim=c(brmin,brmax),
                   ylim=range(aa$counts/sum(aa$counts)),xlab="",ylab="", las = 1)
              abline(v=0,lty=2)
              x<-seq(brmin,(brmax+by),by=by)
              #a<-pnorm(x, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)        
              a<-pnorm(x, mean=mean(sub$StdRes), sd=sqrt(var(sub$StdRes)), lower.tail = TRUE, log.p = FALSE)        
              b<-rep(0,length(a))
              b[2:length(a)]<-a[2:length(a)]-a[1:(length(a)-1)]
              lines(br,b,col="blue",lwd=2)
              mtext(c("Male","Female")[sex], side = 3, line = 0.1, las = 1, adj = 0)
          }
      }
      mtext("Standardised residual", side = 1, line = -1, outer = TRUE)
      mtext("Frequency", side = 2, line = 1, outer = TRUE)
      if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Histograms of residuals for tag by sex"),
                                        side = 1, line = 1, outer = TRUE, cex = 0.7)      
    dev.off()
  }
  
}
