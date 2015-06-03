
#######################################################################################################
#Arni plots
#######################################################################################################
BvulnArni<-function(stock,source.dir,target.dir = source.dir){
  Bvuln<-read.table(paste(source.dir,"/",stock,"bvulnref.out",sep=""),header=T,as.is=T)## AS edit ##[c(firstrow:lastrow),]
    
  BvulnAW<-Bvuln[,PeriodToSeason(as.numeric(substring(names(Bvuln),2,10)))==1]
  BvulnSS<-Bvuln[,PeriodToSeason(as.numeric(substring(names(Bvuln),2,10)))==2]

  x<-PeriodToFishingYear(as.numeric(substring(names(Bvuln),2,10)))[PeriodToSeason(as.numeric(substring(names(Bvuln),2,10)))==1]
  x2<-PeriodToFishingYear(as.numeric(substring(names(Bvuln),2,10)))[PeriodToSeason(as.numeric(substring(names(Bvuln),2,10)))==2]
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  
  #Biomass Plot
  windows(10,4)
  #Set graphics parameters
  par(las=1,mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,0),mgp=c(3,0.5,0))
  plot(x,BvulnAW[1,],type="n",pch=16,xlim=xlim,ylim=c(0,max(BvulnAW,BvulnSS)),ylab="Vulnerable biomass (tonnes)",xlab="",main="AW",)
  b21<-Arniplot(BvulnAW,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b21$stats[3,],lwd=thin) 
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
  #mtext("AW",side=4,line=1)  
  mtext("Fishing year",side=1,line=2)
  plot(x2,BvulnSS[1,],type="n",pch=16,xlim=xlim,ylim=c(0,max(BvulnAW,BvulnSS)),ylab="",xlab="", main="SS")
  b22<-Arniplot(BvulnSS,range=0,xaxt="n",ylab=" ",xlab=" ",at=x2,add=TRUE,medlwd=1)
  lines(x2,b22$stats[3,],lwd=thin) 
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
  #mtext("SS",side=4,line=1)
  mtext("Fishing year",side=1,line=2)
  if(Captions) mtext(paste(source.dir," ",stock,": Bvuln Arni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"BvulnArni.wmf",sep=""),type="wmf",device=dev.cur())
}

ErateArni<-function(stock,source.dir,target.dir = source.dir){
  windows(10,12)

  ### Ulegal
  Ulegal <- read.table(paste(source.dir,"/",stock,"Ulegalpost.out",sep=""),header=T,as.is=T)## AS edit ##[c(firstrow:lastrow),]
  UlegalAW <- Ulegal[,PeriodToSeason(as.numeric(substring(names(Ulegal),2,10)))==1]
  UlegalSS <- Ulegal[,PeriodToSeason(as.numeric(substring(names(Ulegal),2,10)))==2]
  x_legal <- PeriodToFishingYear(as.numeric(substring(names(Ulegal),2,10)))[PeriodToSeason(as.numeric(substring(names(Ulegal),2,10)))==1]
  x2_legal <- PeriodToFishingYear(as.numeric(substring(names(Ulegal),2,10)))[PeriodToSeason(as.numeric(substring(names(Ulegal),2,10)))==2]
  names(UlegalAW) <- x_legal
  names(UlegalSS) <- x2_legal
  attach(UlegalAW)
  attach(UlegalSS)
  UlegalAW_early <- UlegalAW[,1:(switch_period-1)]
  UlegalAW_recent <- UlegalAW[,switch_period:dim(UlegalAW)[2]]
  
  ### Uillegal
  Uillegal<-read.table(paste(source.dir,"/",stock,"Uillegalpost.out",sep=""),header=T,as.is=T)## AS edit ##[c(firstrow:lastrow),]
  UillegalAW<-Uillegal[,PeriodToSeason(as.numeric(substring(names(Uillegal),2,10)))==1]
  UillegalSS<-Uillegal[,PeriodToSeason(as.numeric(substring(names(Uillegal),2,10)))==2]
  #x_illegal<-PeriodToFishingYear(as.numeric(substring(names(Uillegal),2,10)))[PeriodToSeason(as.numeric(substring(names(Uillegal),2,10)))==1]
  #x2_illegal<-PeriodToFishingYear(as.numeric(substring(names(Uillegal),2,10)))[PeriodToSeason(as.numeric(substring(names(Uillegal),2,10)))==2]
  #names(UillegalAW) <- x_illegal
  names(UillegalAW) <- x_legal
  #names(UillegalSS) <- x2_illegal
  names(UillegalSS) <- x2_legal
  attach(UillegalAW)
  attach(UillegalSS)
  UillegalAW_early <- UillegalAW [,1:(switch_period-1)]
  UillegalAW_recent <- UillegalAW [,switch_period:dim(UillegalAW)[2]]
  
  # Calculation of the xlim for the graphs
  x_early <- x_legal[1:(switch_period-1)]
  x_recent <- x2_legal
  xlim_early<-c(min(x_early)-0.5,max(x_early)+0.5)  
  xlim_recent<-c(min(x_recent)-0.5,max(x_recent)+0.5)  

  # Calculation of the ylim for the graphs
  tempSLAW <- matrix(0, nr = 1, nc = dim (UlegalAW)[2])
  tempSLSS <- matrix(0, nr = 1, nc = dim (UlegalSS)[2])
  tempNSLAW <- matrix(0, nr = 1, nc = dim (UillegalAW)[2])
  tempNSLSS <- matrix(0, nr = 1, nc = dim (UillegalSS)[2])
  for (i in 1 : dim (UlegalAW)[2]){
    tempSLAW[i] <- quantile(UlegalAW[,i], probs=0.95)
    tempNSLAW[i] <- quantile(UillegalAW[,i], probs=0.95)
    }
  for (i in 1 : dim (UlegalSS)[2]){
    tempSLSS[i] <- quantile(UlegalSS[,i], probs=0.95)
    tempNSLSS[i] <- quantile(UillegalSS[,i], probs=0.95)
    }
  max_ylim_SL<-max(tempSLAW, tempSLSS)
  max_ylim_NSL<-max(tempNSLAW, tempNSLSS)

  par(las=1,mfrow=c(3,2),oma=c(1,1,1,1),mar=c(3,5,1,0),mgp=c(3,0.5,0))
  
  # plot SL first year to 1978
  plot(x_early,UlegalAW_early[1,],type="n",pch=16,xlim=xlim_early,ylim=c(0,(max_ylim_SL+(max_ylim_SL*0.1))),ylab="SL exploitation rate",xlab="",main="Full year")
  SL_early<-Arniplot(UlegalAW_early,range=0,xaxt="n",ylab=" ",xlab=" ",at=x_early,add=TRUE,medlwd=1)
  lines(x_early,SL_early$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
    
  # plot NSL first year to 1978  
  plot(x_early,UillegalAW_early[1,],type="n",pch=16,xlim=xlim_early,ylim=c(0,(max_ylim_NSL+(max_ylim_NSL*0.1))),ylab="NSL exploitation rate",xlab="",main="Full year")
  NSL_early<-Arniplot(UillegalAW_early,range=0,xaxt="n",ylab=" ",xlab=" ",at=x_early,add=TRUE,medlwd=1)
  lines(x_early,NSL_early$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
    
  # plot SL AW 1979 till last year of short term projection
  plot(x_recent,UlegalAW_recent[1,],type="n",pch=16,xlim=xlim_recent,ylim=c(0,(max_ylim_SL+(max_ylim_SL*0.1))),ylab="SL exploitation rate",xlab="",main="AW")
  SLAW_recent<-Arniplot(UlegalAW_recent,range=0,xaxt="n",ylab=" ",xlab=" ",at=x_recent,add=TRUE,medlwd=1)
  lines(x_recent,SLAW_recent$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
    
  # plot NSL AW 1979 till last year of short term projection
  plot(x_recent,UillegalAW_recent[1,],type="n",pch=16,xlim=xlim_recent,ylim=c(0,(max_ylim_NSL+(max_ylim_NSL*0.1))),ylab="NSL exploitation rate",xlab="", main="AW")
  NSLAW_recent<-Arniplot(UillegalAW_recent,range=0,xaxt="n",ylab=" ",xlab=" ",at=x_recent,add=TRUE,medlwd=1)
  lines(x_recent,NSLAW_recent$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
   
  # plot SL SS 1979 till last year of short term projection
  plot(x_recent,UlegalSS[1,],type="n",pch=16,xlim=xlim_recent,ylim=c(0,(max_ylim_SL+(max_ylim_SL*0.1))),ylab="SL exploitation rate",xlab="", main="SS")
  SLSS<-Arniplot(UlegalSS,range=0,xaxt="n",ylab=" ",xlab=" ",at=x_recent,add=TRUE,medlwd=1)
  lines(x_recent,SLSS$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
  mtext("Fishing year",1,2)

  # plot NSL SS 1979 till last year of short term projection
  plot(x_recent,UillegalSS[1,],type="n",pch=16,xlim=xlim_recent,ylim=c(0,(max_ylim_NSL+(max_ylim_NSL*0.1))),ylab="NSL exploitation rate",xlab="", main="SS")
  NSLSS<-Arniplot(UillegalSS,range=0,xaxt="n",ylab=" ",xlab=" ",at=x_recent,add=TRUE,medlwd=1)
  lines(x_recent,NSLSS$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": ErateArni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"ErateArni.wmf",sep=""),type="wmf",device=dev.cur())
}

RdevArni<-function(stock,source.dir,target.dir = source.dir){
  Rdev<-read.table(paste(source.dir,"/",stock,"Rdev.out",sep=""),as.is=T,header=T)## AS edit ##[c(firstrow:lastrow),]

  windows(7,4)
  par(las=1,oma=c(1,1,1,1),mar=c(3,3,0,0),mgp=c(2,0.5,0))
  x<-as.numeric(substring(names(Rdev),2,10))
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  plot(x,Rdev[1,],type="n",pch=16,xlim=xlim,ylim=range(Rdev),ylab="Rdev  ",xlab="")
  b6<-Arniplot(Rdev,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b6$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": RdevArni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"RdevArni.wmf",sep=""),type="wmf",device=dev.cur())
}

RectArni<-function(stock,source.dir,target.dir = source.dir){
  Rect<-read.table(paste(source.dir,"/",stock,"Rect.out",sep=""),as.is=T,header=T)## AS edit ##[c(firstrow:lastrow),]

  windows(7,4)
  par(las=1,oma=c(1,1,1,1),mar=c(3,3,0,0),mgp=c(2,0.5,0))
  x<-as.numeric(substring(names(Rect),2,10))
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  plot(x,Rect[1,],type="n",pch=16,xlim=xlim,ylim=range(Rect)/2.5,ylab="Recruitment  ",xlab="")
  b6<-Arniplot(Rect,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b6$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": RectArni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"RectArni.wmf",sep=""),type="wmf",device=dev.cur())
}

SPArni<-function(stock,source.dir,target.dir = source.dir){
  SPtemp<-read.table(paste(source.dir,"/",stock,"surprod.out",sep=""),header=T,as.is=T)## AS edit ##[c(firstrow:lastrow),]
  SP<-SPtemp[,c(1:(ncol(SPtemp)-1))]
  x<-as.numeric(substring(names(SP),2,10))
  xlim<-c(min(x)-0.5,max(x)+0.5)  

  windows(7,4)
  par(las=1,oma=c(1,1,1,1),mar=c(3,3.5,0,0),mgp=c(2.5,0.5,0))
  plot(x,SP[1,],type="n",pch=16,xlim=xlim,ylim=range(SP),ylab="Surplus production",xlab="")
  b6<-Arniplot(SP,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b6$stats[3,],lwd=thin)  
  abline(v=(PeriodToFishingYear(ModelEndPeriod)+1)+0.5,lty=2)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": SPArni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/SPArni.wmf",sep=""),type="wmf",device=dev.cur())
}

#Function for printing and closing all graphs
CPUEArni <- function(stock,source.dir,target.dir = source.dir){
  CPUE<-read.table(paste(source.dir,"/",stock,"CPUEpost.out",sep=""),header=T,as.is=T) ## AS edit ## [c(firstrow:lastrow),]
  CPUEperiod <- as.numeric(substring(names(CPUE),2,10))   ## AS edit : added CPUEperiod object
  CPUEAW<-CPUE[,PeriodToSeason(CPUEperiod)==1]            ## AS edit : CPUEperiod
  CPUESS<-CPUE[,PeriodToSeason(CPUEperiod)==2]

  x<-PeriodToFishingYear(CPUEperiod)[PeriodToSeason(CPUEperiod)==1]     ## AS edit : CPUEperiod
  x2<-PeriodToFishingYear(CPUEperiod)[PeriodToSeason(CPUEperiod)==2]    ## AS edit : CPUEperiod
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  
  #Set graphics parameters
  windows(10,4)
    par(las=1,mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,0),mgp=c(3,0.5,0))
   plot(x,CPUEAW[1,],type="p",pch=16,xlim=xlim,ylim=range(CPUEAW),ylab="CPUE index",xlab="",main="AW")
  b41<-Arniplot(CPUEAW[2:nrow(CPUEAW),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b41$stats[3,],lwd=thin)  
  #mtext("AW",side=4,line=1)
  mtext("Fishing year",1,2)
  plot(x2,CPUESS[1,],type="p",pch=16,ylim=range(CPUESS),ylab="",xlab="", main="SS")
  b42<-Arniplot(CPUESS[2:nrow(CPUESS),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x2,add=TRUE,medlwd=1)
  lines(x2,b42$stats[3,],lwd=thin)  
  #mtext("SS",side=4,line=1)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": CPUE Arni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"CPUEArni.wmf",sep=""),type="wmf",device=dev.cur())
}


CPUEresArni<-function(stock,source.dir,target.dir = source.dir){
  CPUEres<-read.table(paste(source.dir,"/",stock,"CPUErespost.out",sep=""),header=T,as.is=T) ## AS edit ## [c(firstrow:lastrow),]

  CPUEresAW<-CPUEres[,PeriodToSeason(as.numeric(substring(names(CPUEres),2,10)))==1]
  CPUEresSS<-CPUEres[,PeriodToSeason(as.numeric(substring(names(CPUEres),2,10)))==2]

  x<-PeriodToFishingYear(as.numeric(substring(names(CPUEres),2,10)))[PeriodToSeason(as.numeric(substring(names(CPUEres),2,10)))==1]
  x2<-PeriodToFishingYear(as.numeric(substring(names(CPUEres),2,10)))[PeriodToSeason(as.numeric(substring(names(CPUEres),2,10)))==2]
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  
  #Set graphics parameters
   windows(10,4)
    par(las=1,mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,0),mgp=c(3,0.5,0))
  
  plot(x,CPUEresAW[1,],type="n",pch=16,xlim=xlim,ylim=range(CPUEresAW),ylab="Standardised residuals",xlab="", main="AW")
  b41<-Arniplot(CPUEresAW,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  #mtext("AW",side=4,line=1)
  mtext("Fishing year",1,2)
  plot(x2,CPUEresSS[1,],type="n",pch=16,xlim=xlim,ylim=range(CPUEresSS),ylab="",xlab="", main="SS")
  b42<-Arniplot(CPUEresSS,range=0,xaxt="n",ylab=" ",xlab=" ",at=x2,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  #mtext("SS",side=4,line=1)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": CPUEres Arni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"CPUEresArni.wmf",sep=""),type="wmf",device=dev.cur())
}

CRArni<-function(stock,source.dir,target.dir = source.dir){
  CR<-read.table(paste(source.dir,"/",stock,"CRpost.out",sep=""),header=T,as.is=T)## AS edit ##[c(1,(firstrow+1):(lastrow+1)),]

  CRAW<-CR[,PeriodToSeason(as.numeric(substring(names(CR),2,10)))==1]
  CRSS<-CR[,PeriodToSeason(as.numeric(substring(names(CR),2,10)))==2]

  x<-PeriodToFishingYear(as.numeric(substring(names(CR),2,10)))[PeriodToSeason(as.numeric(substring(names(CR),2,10)))==1]
  x2<-PeriodToFishingYear(as.numeric(substring(names(CR),2,10)))[PeriodToSeason(as.numeric(substring(names(CR),2,10)))==2]
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  
  #Set graphics parameters
  
   windows(10,4)
    par(las=1,mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,0),mgp=c(3,0.5,0))
 
  plot(x,CRAW[1,],type="p",pch=16,xlim=xlim,ylim=range(CRAW),ylab="CR index",xlab="",main="AW")
  b41<-Arniplot(CRAW[2:nrow(CRAW),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b41$stats[3,],lwd=thin)  
  #mtext("AW",side=4,line=1)
  mtext("Fishing year",1,2)
  ###### No data for SS so no plotting possible #######
  ##plot(x2,CRSS[1,],type="p",pch=16,xlim=xlim,ylim=range(CRSS),ylab="",xlab="",main="SS")
  ##b42<-Arniplot(CRSS[2:nrow(CRSS),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x2,add=TRUE,medlwd=1)
  ##lines(x2,b42$stats[3,],lwd=thin)  
  #mtext("SS",side=4,line=1)
  ##mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": CR Arni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"CRArni.wmf",sep=""),type="wmf",device=dev.cur())
}

CRresArni<-function(stock,source.dir,target.dir = source.dir){
  CRres<-read.table(paste(source.dir,"/",stock,"CRrespost.out",sep=""),header=T,as.is=T)## AS edit ##[c(firstrow:lastrow),]

  CRresAW<-CRres[,PeriodToSeason(as.numeric(substring(names(CRres),2,10)))==1]
  CRresSS<-CRres[,PeriodToSeason(as.numeric(substring(names(CRres),2,10)))==2]

  x<-PeriodToFishingYear(as.numeric(substring(names(CRres),2,10)))[PeriodToSeason(as.numeric(substring(names(CRres),2,10)))==1]
  x2<-PeriodToFishingYear(as.numeric(substring(names(CRres),2,10)))[PeriodToSeason(as.numeric(substring(names(CRres),2,10)))==2]
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  
  #Set graphics parameters
    windows(10,4)
    par(las=1,mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,0),mgp=c(3,0.5,0))
   
  plot(x,CRresAW[1,],type="n",pch=16,xlim=xlim,ylim=range(CRresAW),ylab="Standardised residuals",xlab="", main="AW")
  b41<-Arniplot(CRresAW,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  #mtext("AW",side=4,line=1)
  mtext("Fishing year",1,2)
  plot(x2,CRresSS[1,],type="n",pch=16,xlim=xlim,ylim=range(CRresSS),ylab="",xlab="",main="SS")
  b42<-Arniplot(CRresSS,range=0,xaxt="n",ylab=" ",xlab=" ",at=x2,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  #mtext("SS",side=4,line=1)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": CRres Arni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"CRresArni.wmf",sep=""),type="wmf",device=dev.cur())
}

PRIArni<-function(stock,source.dir,target.dir = source.dir){
  #PRIArni
  PRI<-read.table(paste(source.dir,"/",stock,"PRIpost.out",sep=""),header=T,as.is=T)## AS edit ##[c(1,(firstrow+1):(lastrow+1)),]

  PRIAW<-PRI[,PeriodToSeason(as.numeric(substring(names(PRI),2,10)))==1]
  PRISS<-PRI[,PeriodToSeason(as.numeric(substring(names(PRI),2,10)))==2]

  x<-PeriodToFishingYear(as.numeric(substring(names(PRI),2,10)))[PeriodToSeason(as.numeric(substring(names(PRI),2,10)))==1]
  x2<-PeriodToFishingYear(as.numeric(substring(names(PRI),2,10)))[PeriodToSeason(as.numeric(substring(names(PRI),2,10)))==2]
  xlim<-c(min(x2,x)-0.5,max(x2,x)+0.5)  
   windows(10,4)
    par(las=1,mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,0),mgp=c(3,0.5,0))
   plot(x,PRIAW[1,],type="p",pch=16,xlim=xlim,ylim=range(PRIAW),ylab="PRI index",xlab="",main="AW")
  b41<-Arniplot(PRIAW[2:nrow(PRIAW),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b41$stats[3,],lwd=thin)  
  #mtext("AW",side=4,line=1)
  mtext("Fishing year",1,2)
  plot(x2,PRISS[1,],type="p",pch=16,xlim=xlim,ylim=range(PRISS),ylab="",xlab="",main="SS")
  b42<-Arniplot(PRISS[2:nrow(PRISS),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x2,add=TRUE,medlwd=1)
  lines(x2,b42$stats[3,],lwd=thin)  
  #mtext("SS",side=4,line=1)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": PRI Arni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"PRIArni.wmf",sep=""),type="wmf",device=dev.cur())
}

PRIresArni<-function(stock,source.dir,target.dir = source.dir){
  #PRIresArni
  PRIres<-read.table(paste(source.dir,"/",stock,"PRIrespost.out",sep=""),header=T,as.is=T)## AS edit ##[c(firstrow:lastrow),]

  PRIresAW<-PRIres[,PeriodToSeason(as.numeric(substring(names(PRIres),2,10)))==1]
  PRIresSS<-PRIres[,PeriodToSeason(as.numeric(substring(names(PRIres),2,10)))==2]

  x<-PeriodToFishingYear(as.numeric(substring(names(PRIres),2,10)))[PeriodToSeason(as.numeric(substring(names(PRIres),2,10)))==1]
  x2<-PeriodToFishingYear(as.numeric(substring(names(PRIres),2,10)))[PeriodToSeason(as.numeric(substring(names(PRIres),2,10)))==2]
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  
  #Set graphics parameters
   windows(10,4)
    par(las=1,mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,0),mgp=c(3,0.5,0))
 
   
  plot(x,PRIresAW[1,],type="n",pch=16,xlim=xlim,ylim=range(PRIresAW),ylab="Standardised residuals",xlab="",main="AW")
  b41<-Arniplot(PRIresAW,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  #mtext("AW",side=4,line=1)
  mtext("Fishing year",1,2)
  plot(x2,PRIresSS[1,],type="n",pch=16,xlim=xlim,ylim=range(PRIresSS),ylab="",xlab="",main="SS")
  b42<-Arniplot(PRIresSS,range=0,xaxt="n",ylab=" ",xlab=" ",at=x2,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  #mtext("SS",side=4,line=1)
  mtext("Fishing year",1,2)
  if(Captions) mtext(paste(source.dir," ",stock,": PRIres Arni",sep=""),side=1,line=0,outer=T,cex=0.7)
  savePlot(filename=paste(target.dir,"/",stock,"PRIresArni.wmf",sep=""),type="wmf",device=dev.cur())
}

LFArni<-function(stock,source.dir,target.dir = source.dir){
  LF1<-read.table(paste(source.dir,"/",stock,"LF1post.out",sep=""),header=T)## AS edit ##[c(1,(firstrow+1):(lastrow+1)),]
  LF2<-read.table(paste(source.dir,"/",stock,"LF2post.out",sep=""),header=T)## AS edit ##[c(1,(firstrow+1):(lastrow+1)),]
  LF3<-read.table(paste(source.dir,"/",stock,"LF3post.out",sep=""),header=T)## AS edit ##[c(1,(firstrow+1):(lastrow+1)),]

  x<-as.numeric(substring(colnames(LF1),2))
  xlim<-c(min(x)-0.5,max(x)+0.5)  

  #Biomass Plot
  #Make new window
  windows()
  #Set graphics parameters
  par(las=1,mfrow=c(3,1),oma=c(0,0,1,0),mar=c(5,5,1,1),lab=c(3,5,5),cex=0.8)

  plot(x,LF1[1,],pch=16,xlim=xlim,ylim=c(0,max(LF1)),ylab="",xlab="Length (mm)")
  b8<-Arniplot(LF1[2:nrow(LF1),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b8$stats[3,],lwd=thin) 
  mtext("Proportion",side=2,line=4,las=0,cex=0.8)
  mtext("Male",side=3,adj=0,cex=0.8)
  plot(x,LF2[1,],pch=16,xlim=xlim,ylim=c(0,max(LF2)),ylab="",xlab="Length (mm)")
  b8<-Arniplot(LF2[2:nrow(LF2),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b8$stats[3,],lwd=thin) 
  mtext("Proportion",side=2,line=4,las=0,cex=0.8)
  mtext("Immature female",side=3,adj=0,cex=0.8)
  plot(x,LF3[1,],pch=16,xlim=xlim,ylim=c(0,max(LF3)),ylab="",xlab="Length (mm)")
  b8<-Arniplot(LF3[2:nrow(LF3),],range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  lines(x,b8$stats[3,],lwd=thin) 
  mtext("Proportion",side=2,line=4,las=0,cex=0.8)
  mtext("Mature female",side=3,adj=0,cex=0.8)

  mtext(LFArniCaption,side=3,outer=T)
  savePlot(filename=paste(target.dir,"/",stock,"LFArni.wmf",sep=""),type="wmf",device=dev.cur())
}

LFresArni<-function(stock,source.dir,target.dir = source.dir){
  LF1resid<-read.table(paste(source.dir,"/",stock,"LF1respost.out",sep=""),header=T)## AS edit ##[c(firstrow:lastrow),]
  LF2resid<-read.table(paste(source.dir,"/",stock,"LF2respost.out",sep=""),header=T)## AS edit ##[c(firstrow:lastrow),]
  LF3resid<-read.table(paste(source.dir,"/",stock,"LF3respost.out",sep=""),header=T)## AS edit ##[c(firstrow:lastrow),]
  
  x<-as.numeric(substring(colnames(LF1resid),2))
  xlim<-c(min(x)-0.5,max(x)+0.5)  
  #Make new window
  windows()
  #Set graphics parameters
  par(las=1,mfrow=c(3,1),oma=c(0,0,1,0),mar=c(5,5,1,1),lab=c(3,5,5),cex=0.8)
  
  plot(x,LF1resid[1,],type="n",xlim=xlim,ylim=range(LF1resid),ylab="Standard residuals",xlab="Length (mm)")
  b8<-Arniplot(LF1resid,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  mtext("Male",side=3,adj=0,cex=0.8)
  plot(x,LF2resid[1,],type="n",xlim=xlim,ylim=range(LF2resid),ylab="Standard residuals",xlab="Length (mm)")
  b8<-Arniplot(LF2resid,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  mtext("Immature female",side=3,adj=0,cex=0.8)
  plot(x,LF3resid[1,],type="n",xlim=xlim,ylim=range(LF3resid),ylab="Standard residuals",xlab="Length (mm)")
  b8<-Arniplot(LF3resid,range=0,xaxt="n",ylab=" ",xlab=" ",at=x,add=TRUE,medlwd=1)
  abline(h=0,lty=2)
  mtext("Mature female",side=3,adj=0,cex=0.8)

  mtext(LFArniCaption,side=3,outer=T)
  savePlot(filename=paste(target.dir,"/",stock,"LFresArni.wmf",sep=""),type="wmf",device=dev.cur())
}


## AS edit : the following function was removed by request of Breen, because it uses "TagResPost.out"
#
#QQposttagresid<-function(stock,source.dir,target.dir = source.dir)
#{
#  tagresid<-read.table(paste(source.dir,"/",stock,"Tagrespost.out",sep=""),header=F)[c(firstrow:lastrow),]
#  #Set graphics parameters
#  windows(7,4)
#  par(las=1,oma=c(0,0,1,0),mar=c(5,5,1,1))
#  QQpost(tagresid,xlab="Theoretical quantiles",ylab="Std. residuals")
#  mtext("Tag residual QQ plot",side=3,line=0,cex=0.7,outer=TRUE)
#  savePlot(filename=paste(source.dir,"/",stock,"QQposttagresid",sep=""),type="wmf",device=dev.cur())
#}




#------------------------------------------------------------------------------------------
#                      These are general code for Arniplot and QQpost                      
#Arniplot
Arniplot<-function (x, ..., range = 1.5, width = NULL, varwidth = FALSE, 
    notch = FALSE, names, boxwex = 0.8, data = parent.frame(), 
    plot = TRUE, border = par("fg"), col = NULL, log = "", pars = NULL, 
    horizontal = FALSE, add = FALSE, at = NULL) 
{
    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)) 
        attributes(args)$names != ""
    else rep(FALSE, length = length(args))
    pars <- c(args[namedargs], pars)
    groups <- if (is.language(x)) {
        warning(paste("Using `formula' in boxplot.default --", 
            "shouldn't boxplot.formula be called?"))
        if (inherits(x, "formula") && length(x) == 3) {
            groups <- eval(x[[3]], data, parent.frame())
            x <- eval(x[[2]], data, parent.frame())
            split(x, groups)
        }
    }
    else {
        groups <- args[!namedargs]
        if (length(groups) == 1 && is.list(x)) 
            x
        else groups
    }
    if (0 == (n <- length(groups))) 
        stop("invalid first argument")
    if (length(class(groups))) 
        groups <- unclass(groups)
    if (!missing(names)) 
        attr(groups, "names") <- names
    else {
        if (is.null(attr(groups, "names"))) 
            attr(groups, "names") <- 1:n
        names <- attr(groups, "names")
    }
    for (i in 1:n) groups[i] <- list(boxplot.stats(groups[[i]], 
        range))
    stats <- matrix(0, nr = 5, nc = n)    
    conf <- matrix(0, nr = 2, nc = n)
    ng <- out <- group <- numeric(0)
    ct <- 1
    for (i in 1:n) stats[,i]<-quantile(x[,i],prob=c(0.05,0.25,0.5,0.75,0.95))

    for (i in groups) {
        #i$stats<-stats[i]
        #stats[, ct] <- i$stats
        conf[, ct] <- i$conf
        ng <- c(ng, i$n)
        if ((lo <- length(i$out))) {
            out <- c(out, i$out)
            group <- c(group, rep(ct, lo))
        }
        ct <- ct + 1
    }
    z <- list(stats = stats, n = ng, conf = conf, out = out, 
        group = group, names = names)
    if (plot) {
        bxp(z, width, varwidth = varwidth, notch = notch, boxwex = boxwex, 
            border = border, col = col, log = log, pars = pars, 
            horizontal = horizontal, add = add, at = at)
        invisible(z)
    }
    else z
}
