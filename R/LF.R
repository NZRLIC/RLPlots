#' Length-frequency function
#'
#' To not have size limit of graph LFOptions$SizeLim=0
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @export
#' 
LF <- function(stock, source.dir, target.dir = source.dir)
{
    
    # Read file
    data <- read.table(paste(source.dir, "/", stock, "LFR.out", sep = ""), header = TRUE, as.is = TRUE) 
    # Names = sample tau period type Season sex size Obs Pred StdRes  LL 
    maxtype <- max(data$type)
    sexcode <- LFOptions$sexcode

  # ==============================================================================
  # Plot obs vs pred
  # ==============================================================================
  if ( LFOptions$ObsPred )
  {
    #Calculate y axis limits based on option
    ylims <- list()
    for (sex in sexcode) ylims[[sex]] <- c(0, max(data[,c("Pred","Obs")]))
    i <- 0
    #For each sample and sex      
    for ( sample in unique(data$sample) )
    {
      #Make a new window if needed
      n.LF <- LFOptions$SamplesPerPage
      if ( is.na(match(sample, 0:8*n.LF+1)) != TRUE )
      {
        PlotType(paste(target.dir, "/", stock, "LFfit", i, sep = ""), width = 170, height = 200)
        par(mfrow = c(n.LF, length(sexcode)), oma = c(2,3,1,4), mar = c(1,3,0.5,2), lab = c(5,4,5), cex = 2)
        i <- i + 1
      } 
      if ( is.na(match(sample,c(1:8)*n.LF)) != TRUE | sample == max(data$sample) ) xaxt <- "s" else xaxt <- "n"
      for (sex in sexcode)
      {
          #Take dat subset
          sub <- data[data$sample==sample & data$sex==sex,]
          if (length(sub[,1]) > 0) empty <- FALSE else empty <- TRUE
          #Plot
          if (!empty)
          {
            if (LFOptions$ObsPredYFixed)
            {
              ylim <- ylims[[sex]]
              xlim <- c(30, 90)
              yaxt <- "s"
            } else {
              ylim <- c(0, max(sub$Pred, sub$Obs))
              xlim <- c(40, 75)
              yaxt <- "n"
            }
            plot(Pred ~ size, data = sub, type = "l", lwd = PlotOptions$thin, xaxt = xaxt, xlim = xlim, ylab = "", ylim = ylim, yaxt = yaxt, bty = "L", las = 1)
            legend("topright",legend=as.character(round(unique(data[data$sample==sample & data$sex==sex,"effN"]),2)),bty="n",title="eff. N:", cex = 0.7)
            if ( !LFOptions$ObsPredYFixed )  axis(2, c(0, round(max(sub$Obs, sub$Pred), 2)), las = 1)
            points(Obs ~ size, data = sub, pch = 1)
          } else {
            plot(-10, -10, xaxt = xaxt, xlim = xlim, ylim = c(0, 0.3), ylab = "", las = 1, yaxt = "n")
            axis(2, c(0, 0.3), las = 1)
          }
          abline(v = LFOptions$SizeLim[sex], lty = 2)
          if ( sex == 3 ) mtext(paste("N:", unique(data[data$sample == sample, "N"])), side = 4, line = 2, las = 1, cex = 1.5, at = 0)
          if ( sex == 3 ) mtext(paste(sub$Year, c("1","2")[sub$Season], c("LB","CS","CN","CM","MS")[sub$type]), side = 4, line = 2, las = 1, cex = 1.5, at = ylim[2]*0.2)
          if ( xaxt == "s" && sex == 2 ) mtext("Size (mm TW)", side = 1, line = 3, las = 1, cex = 2.5)
      }
      mtext("Proportion at size", side = 2, line = 1, outer = TRUE, cex = 3)
      if ( PlotOptions$Captions ) mtext(paste(source.dir, " ", stock, ": Observed versus predicted for size frequency fits"), side = 1, line = 4, outer = TRUE, cex = 0.7)
      if ( xaxt == "s" ) dev.off()
    }
  }
  
  # ==============================================================================
  # Plotting Cumulative frequencies (for all)
  # ==============================================================================
  if ( LFOptions$Cum )
  { 
    i <- 0
    #For each sample and sex
    for ( sample in unique(data$sample) )
    {
      #Make a new window if needed
      n.LF <- LFOptions$SamplesPerPage
      if ( is.na(match(sample, 0:8*n.LF+1)) != TRUE )
      {
        PlotType(paste(target.dir, "/", stock, "LFcum" , i, sep = ""), width = 170, height = 200)
        par(mfrow = c(n.LF,length(sexcode)), oma = c(1,1,1,1), mar = c(1,1,1,1), lab = c(5,4,7))
        i <- i + 1
      }
      subdata <- data[data$sample == sample,]
      subdata$cumpred <- cumsum(subdata$Pred)
      subdata$cumobs <- cumsum(subdata$Obs)   
     
      for( sex in sexcode )
      {
        #Take dat subset
        sub1 <- subdata[subdata$sample==sample & subdata$sex==sex,]
        if( length(sub1[,1]) > 0 ) empty <- FALSE else empty <- TRUE
        #Plot
        if( sex == 1 & mod(sample,2) == 0 ) yaxt <- "s" else yaxt <- "n"
        if( is.na(match(sample, 1:8*n.LF)) != TRUE | sample == max(data$sample) ) xaxt <- "s" else xaxt <- "n"
        ylim <- c(min(sub1$cumpred,sub1$cumobs), max(sub1$cumpred,sub1$cumobs))
        if( !empty )
        {
          plot(cumpred~size,data=sub1,type="l",lwd=PlotOptions$thin,xaxt=xaxt,xlim=xlim,ylab = "",ylim = ylim,
                               las=1,yaxt="n")
          if ( sex == 1 ) minaxis <- 0 else minaxis <- round(min(sub1$cumpred, sub1$cumobs), 4)
          axis(2, c(minaxis,round(max(sub1$cumpred,sub1$cumobs),4)), las = 1)
          points(cumobs~size, data = sub1, pch = 16)
        } else {
          plot(-10,-10,xaxt=xaxt,xlim=xlim,ylim=c(0,1),ylab="",las=1,yaxt="n")
          axis(2, c(0,1), las = 1)
        }
        abline(v = LFOptions$SizeLim[sex], lty = 2)
        if ( sex == 3 ) mtext(paste(sub1$Year, c("1","2")[sub$Season], c("LB","CS","CN,CM","MS")[sub$type])[1], side = 4, line = LFOptions$line, las = 1, cex = 0.7, at = 0)
        if ( xaxt == "s" & sex == 2 ) mtext("Size (mm TW)", side = 1, line = 3, las = 1, cex = 0.7) 
      }
      if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Observed versus predicted for size cumulative frequency fits"),side=1,line=4,outer=TRUE,cex=0.7) 
      if ( xaxt == "s" ) dev.off()
    }
  }

  # ==============================================================================
  # Plot StdRes v size
  # ==============================================================================
  if ( LFOptions$StdRes )
  {
    i <- 0
    #For each sample and sex
    for ( sample in unique(data$sample) )
    {
      #Make a new window if needed
      n.LF <- LFOptions$SamplesPerPage
      if ( is.na(match(sample, c(0:8)*n.LF+1)) != TRUE )
      {
        PlotType(paste(target.dir, "/", stock, "LFresid" , i, sep = ""), width = 170, height = 200)
        par(mfrow=c(n.LF,length(sexcode)),oma=c(1,5,1,5),mar=c(1,1,1,1), cex = 3)
        i<-i+1
      }
      for(sex in sexcode)
      {
        #Take dat subset
        sub <- data[data$sample==sample & data$sex==sex,]
        if(length(sub[,1])>0) empty<-FALSE else empty<-TRUE
        #Plot
        if(sex==1 & mod(sample,2)==0) yaxt<-"s" else xaxt<-"n" 
        if(is.na(match(sample,c(1:8)*n.LF))!=TRUE | sample==max(data$sample)) xaxt<-"s" else xaxt<-"n"
        if( !empty )
        {
          plot(StdRes~size,data=sub,pch=1,xaxt=xaxt,xlim=c(30,90),bty="L",yaxt=yaxt,ylim=c(-5,5),ylab="")
        } else {
          plot(-10,-10,xaxt=xaxt,xlim=c(30,90),yaxt=yaxt,ylim=c(-5,5),bty="l",las=1)
        }
        abline(v = LFOptions$SizeLim[sex], lty = 2)
        if ( sex == 3 ) mtext(paste(sub$Year,c("1","2")[sub$Season], c("LB","CS","CN,CM","MS")[sub$type]),
                              side = 4, line = LFOptions$line, las = 1, cex = 2, at = 0)
        if ( xaxt == "s" & sex == 2 )  mtext("Size (mm TW)",side=1,line=3,las=1,cex=2) 
      }
      mtext("Standardised residuals",side=2,line=1,outer=TRUE,cex=0.8)
      if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Standardised residual versus size for size frequency fits"),side=1,line=4,outer=TRUE,cex=2)
      if ( xaxt == "s" ) dev.off()
    }
  }

  # ==============================================================================
  # Plot resid v pred by sex
  # ==============================================================================
  if( LFOptions$ResPred )
  {
    PlotType(paste(target.dir, "/", stock, "LFresSex", sep = ""), width = 170, height = 200)
    par(mfrow=c(3,1),oma=c(4,1,1,1),mar=c(5,5,1,5),cex = 3)
      for( sex in 1:3 )
      {
        plot(StdRes~Pred,data=data[data$sex==sex,],pch=c(1,16)[Season],lwd=PlotOptions$thin,ylab="Standardised residual",xlab="Predicted proportion",las=1) 
        abline(h = 0, lty = 2) 
        mtext(PlotOptions$SexNames[sex],side=4,line=1,las=1,cex=2)
      }
    if( PlotOptions$Captions ) 
    {
      mtext(paste(source.dir," ",stock,": Standardised residual versus predicted proportion for size frequency fits"),side=1,line=0,outer=TRUE,cex=2)
      mtext("Closed circles: logbook 1; Open circles: observer 2",side=1,line=1,outer=TRUE,cex=2) ## AS edit : added "logbook" and "observer"
    }
    dev.off()
  }

  # ==============================================================================
  # Plot boxplots by sex
  # ==============================================================================
  if ( LFOptions$BoxSize )
  {
    PlotType(paste(target.dir, "/", stock, "LFresboxSex", sep = ""), width = 170, height = 200)
    par(mfrow=c(max(LFOptions$sexcode),1),oma=c(4,1,1,1),mar=c(5,5,1,5), cex = 2)
    for( sex in LFOptions$sexcode )
    {
      boxplot(StdRes~size,data=data[data$sex==sex,],ylab="Stdandardised residual",xlab="Size (mm TW)",las=1,medlwd=PlotOptions$thin) 
      abline(h = 0, lty = 2)
      mtext(PlotOptions$SexNames[sex],side=4,line=1,las=1,cex=3)
    }
    if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Box plots of standardised residuals of LF for each sex and size class"),side=1,line=0,outer=TRUE,cex=2)
    dev.off()

  # ==============================================================================
  # Plot Arniplots by sex by season
  # ==============================================================================
    PlotType(paste(target.dir, "/", stock, "LFresboxSexSea", sep = ""), width = 170, height = 200)
    par(mfrow=c(max(LFOptions$sexcode),2),oma=c(4,1,1,4),mar=c(4,4,2,1), cex = 2)
      for(sex in LFOptions$sexcode)
      {
        for(Season in 1:2)
        {
          qtmp<-data[data$sex==sex & data$Season==Season,]
          if(length(qtmp[,1])>0)
          { 
             boxplot(StdRes~size,data=data[data$sex==sex & data$Season==Season,],ylab="Standardised residual",xlab="Size (mm TW)",las=1,medlwd=PlotOptions$thin) 
          }
          else
          {
            plot(-10,-10,xaxt=xaxt,xlim=c(30,90),yaxt=yaxt,ylim=c(-6,6),las=1)
          }
          abline(h=0,lty=2) 
          if(sex==1) mtext(c("AW","SS")[Season],side=3,line=1,las=1,cex=2)
          if(Season==2) mtext(PlotOptions$SexNames[sex],side=4,line=1,las=1,cex=2)
        }
      }
    if ( PlotOptions$Captions ) mtext(paste(source.dir," ",stock,": Box plots of standardised residuals for each sex and size class"),side=1,line=0,outer=TRUE,cex=2)
    dev.off()
  }
  
  # ==============================================================================
  # QQ normal plots by sex
  # ==============================================================================
  if ( LFOptions$ResQQ ) subsub <- data[abs(data$StdRes)>0.05,] else subsub <- data
  if ( LFOptions$QQ )
  {
    PlotType(paste(target.dir, "/", stock, "LFQQsex", sep = ""), width = 170, height = 200)
    par(mfrow=c(3,1),oma=c(4,1,1,1),mar=c(5,5,1,5))
      for(sex in 1:3)
      {
        qqnorm(subsub$StdRes[subsub$sex==sex],xlab="Theoretical quantiles",ylab="Standardised residual",main="",las=1,xlim=c(-3,3))
        abline(a=0,b=1)
        abline(h=quantile(subsub$StdRes,p=c(5,25,50,75,95)/100),lty=2)#Plot quntiles of the stdevs
        mtext(PlotOptions$SexNames[sex],side=4,line=1,las=1,cex=0.7)
      }
    if( PlotOptions$Captions ) 
    {
      mtext(paste(source.dir," ",stock,": Quantile-quantile plots for size frequencies by sex"),side=1,line=0,outer=TRUE,cex=0.7)
      mtext("Horizontal lines are 5, 25, 50, 75, 95 percent of residuals",side=1,line=1,outer=TRUE,cex=0.7)
    }
    dev.off()
  }
 
  # ==============================================================================
  # Bubble plots by sex
  # ==============================================================================
  if ( LFOptions$Bubble )
  {
    PlotType(paste(target.dir, "/", stock, "LFBubbleSex", sep = ""), width = 170, height = 200)
    par(mfrow=c(3,1),oma=c(4,1,1,1),mar=c(5,5,1,5))
      for(sex in 1:3)
      {
        #Set up plot blank plot
        plot(c(31,91),c(-10,-10),ylim=c(0,max(data$sample)),ylab="Sample",xlab="Size (mm TW)",las=1)
        for(sample in unique(data$sample))
        {
          sub<-data[data$sample==sample & data$sex==sex,]
          points(sub$size,rep(sample,length(sub$size)),cex=sqrt(abs(sub$StdRes)),pch=c(1,16)[as.integer(sub$StdRes>0)+1])  
        }
        mtext(PlotOptions$SexNames[sex],side=4,line=1,las=1,cex=0.7)
      } 
    if( PlotOptions$Captions )
    {
      mtext(paste(source.dir," ",stock,": Bubble plots of residuals for size frequencies by sex"),side=1,line=0,outer=TRUE,cex=0.7)
      mtext(paste("closed circle: positive residuals, open circle: negative residuals"),side=1,line=1,outer=TRUE,cex=0.7)
    }
    dev.off()
  }   
  
  # ==============================================================================
  # Histograms by sex
  # ==============================================================================
  if ( LFOptions$Reshist )
  {
    PlotType(paste(target.dir, "/", stock, "LFHistSex", sep = ""), width = 170, height = 200)
    par(mfrow = c(3,1), oma = c(4,1,1,1), mar = c(5,5,1,5))
      for ( sex in 1:3 )
      {
        sub<-data[data$sex==sex & data$Obs!=0,]
        brmin<-floor(min(sub$StdRes))
        brmax<-ceiling(max(sub$StdRes))
        by<-0.1
        br<-seq(brmin-by/2,brmax+by/2,by=by)
        aa <- hist(sub$StdRes, br = br, right = F, plot = F)
        plot(aa$mids,aa$counts/sum(aa$counts),xlim=c(brmin,brmax),ylim=range(aa$counts/sum(aa$counts)),xlab="std. residuals",ylab="Frequency")
        abline(v=0,lty=2)
        x<-seq(brmin,(brmax+by),by=by)
        a<-pnorm(x, mean=mean(sub$StdRes), sd=sqrt(var(sub$StdRes)), lower.tail = TRUE, log.p = FALSE)        
        b<-rep(0,length(a))
        b[2:length(a)]<-a[2:length(a)]-a[1:(length(a)-1)]
        lines(br,b,col="blue",lwd=2)
        mtext(PlotOptions$SexNames[sex],side=4,line=1,las=1,cex=0.7)
      }
    if ( PlotOptions$Captions ) 
    {
      mtext(paste(source.dir," ",stock,": Histograms of residuals for size frequencies by sex"),side=1,line=0,outer=TRUE,cex=0.7)      
    }
    dev.off()
  }

}
