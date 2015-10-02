#' Sex ratio fit plots
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @param SEXROptions
#' @param PlotOptions
#' @export
#' 
sexRatio <- function(stock, source.dir, target.dir = source.dir,
                     SEXROptions = .SEXROptions, PlotOptions = .PlotOptions)
{
    filename <- paste(source.dir, "/", stock, "SexRatio.out", sep = "")
    
    if ( file.exists(filename) )
    {
        sexr <- read.table(filename, header = TRUE, as.is = TRUE)
        sexr$Sex <- factor(c("Male", "Immature female", "Mature female")[sexr$sex])
    } else {
        stop(paste("can't find:", filename, "\n"))
    }

    if ( PlotOptions$UsePeriod )
    {
        sexr$x <- sexr$period
        xlab <- "\nPeriod"     
    } else {
        sexr$x <- PeriodToFishingYear(sexr$period)
        xlab <- "\nFishing year"
    }

    SD <- sqrt(sexr$Pred*(1-sexr$Pred)/sexr$effN)
    sexr$LB <- sexr$Obs - 1.96*SD
    sexr$UB <- sexr$Obs + 1.96*SD
    
    sex.lab  <- c('Male','Imm. Female','Mat. Female')
    type.lab <- c('LB','CS')

    #===============================================================================
    # Plot obs v pred
    #===============================================================================
    if ( SEXROptions$ObsPred )
    {
        #d <- sexr[,c('Year','Obs','Season','sex','type','LB','UB')]
        #d <- melt(d, id.vars = list('Year','Season','sex','type'), as.is = list('Obs','LB','UB'))
        #d$type[d$type == 1] <- "CS"
        #d$type[d$type == 2] <- "LB"
        #d$Season[d$Season == 1] <- "AW"
        #d$Season[d$Season == 2] <- "SS"
        #d$sex[d$sex == 1] <- "Male"
        #d$sex[d$sex == 2] <- "Imm. Female"
        #d$sex[d$sex == 3] <- "Mat. Female"
        #p <- ggplot() +
        #    geom_point(data = subset(d, d$variable == 'Obs'), aes(x = Year, y = value)) +
        #                                #, group = Epoch, color = factor(Epoch)), size = 1.5) + 
        #    scale_colour_manual(values = PlotOptions$colourPalette) +
        #    facet_grid(Season + type  ~ sex) + xlab(xlab) + ylab("Proportion\n") + expand_limits(y = c(0,1)) +
        #    #guides(color = guide_legend(title = "Epoch")) + 
        #    theme_lobview(PlotOptions)
        #if ( PlotOptions$Captions )
        #{
        #    p <- p + ggtitle(paste(source.dir, " ", stock, ": Selectivity curve by Epoch by sex")) +
        #        theme(plot.title = element_text(size = 9, vjust = 2.7))
        #}
        # AW
        PlotType(paste(target.dir, "/", stock, "SEXRObsPredAW", sep = ""), PlotOptions, width = 170, height = 200)
        par(las = 1, oma = c(1,1,0,1), mar = c(5,4,1,1), mfrow = c(3,2)) 
        for ( sex in 1:3 )
        {
            for ( type in 1:2 )
            {
                plot(Obs ~ Year, data = sexr[sexr$Season==1 & sexr$sex==sex & sexr$type==type,], pch = 1, ylab = "Proportion",
                     xlim = range(Year), ylim = c(min(sexr$LB), max(sexr$UB)), xlab = xlab)
                lines(Pred ~ Year, data = sexr[sexr$Season==1 & sexr$sex==sex & sexr$type==type,],lwd=PlotOptions$thin)
                prange <- unique(sexr$Year[sexr$Season==1 & sexr$sex==sex & sexr$type==type])
                for (p in prange) lines(c(LB, UB) ~ c(p,p), data = sexr[sexr$Year==p & sexr$Season==1 & sexr$sex==sex & sexr$type==type,], pch = "-", type = "o", lwd = PlotOptions$thin)
                legend('top', legend = paste(sex.lab[sex],";",type.lab[type]), lty = 0, bty = "n")
            }
        }
        if ( PlotOptions$Captions ) 
        {
            mtext(paste(source.dir," ",stock,": Observed and predicted for AW Sex Ratio fits."),side=1,line=-0.7,outer=TRUE,cex=0.7)
            mtext(paste(" Points: Observed; Lines: Predicted  "),side=1,line=0,outer=TRUE,cex=0.7)
        }
        dev.off()
    
        # SS
        PlotType(paste(target.dir, "/", stock, "SEXRObsPredSS", sep = ""), PlotOptions, width = 170, height = 200)
        par(las=1,oma=c(1,1,0,1),mar=c(5,4,1,1),mfrow=c(3,2)) 
        for ( sex in 1:3 )
        {
            for ( type in 1:2 )
            {
                plot(Obs~Year,data=sexr[sexr$Season==2 & sexr$sex==sex & sexr$type==type,],pch=1,ylab="Proportion",
                     xlim=range(Year),ylim=c(min(sexr$LB),max(sexr$UB)),xlab=xlab)
                lines(Pred~Year,data=sexr[sexr$Season==2 & sexr$sex==sex & sexr$type==type,],lwd=PlotOptions$thin)
                prange<-unique(sexr$Year[sexr$Season==2 & sexr$sex==sex & sexr$type==type])
                for(p in prange) lines(c(LB,UB)~c(p,p),data=sexr[sexr$Year==p & sexr$Season==2 & sexr$sex==sex & sexr$type==type,],pch="-",type="o",lwd=PlotOptions$thin)
                legend('top',legend=paste(sex.lab[sex],";",type.lab[type]),lty=0,bty="n")
            }
        }
        if ( PlotOptions$Captions ) 
        {
            mtext(paste(source.dir," ",stock,": Observed and predicted for SS Sex Ratio fits."),side=1,line=-0.7,outer=TRUE,cex=0.7)
            mtext(paste(" Points: Observed; Lines: Predicted  "),side=1,line=0,outer=TRUE,cex=0.7)
        }
        dev.off()
    }
  

    #===============================================================================
    # Residuals
    #===============================================================================
    if ( SEXROptions$Resid )
    {
        # Resids v period
        PlotType(paste(target.dir, "/", stock, "SEXRResid", sep = ""), PlotOptions, width = 170, height = 200)
        par(las=1,oma=c(1,1,1,1),mar=c(4,3,1,1),mgp=c(2,1,0)) 
        plot(StdRes~Year,data=sexr,pch=c(1,16)[Season],lwd=PlotOptions$thin,ylab="Standardised residual",
             xlab=xlab)
        abline(h=0,lty=2)
        if(PlotOptions$Captions) 
        {
            mtext(paste(source.dir," ",stock,": Standardised residual for Sex Ratio fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
            mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
        }
        dev.off()

        # Resids v sex
        dfm <- sexr[,c('StdRes','Sex','Year','Season')]
        dfm$Season <- factor(c("AW","SS")[dfm$Season])
        dfm$Sex <- factor(dfm$Sex, levels = c("Male","Immature female","Mature female"))
        PlotType(paste(target.dir, "/", stock, "SEXRResidSex", sep = ""), PlotOptions,
                 width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
        p <- ggplot(data = dfm, aes(x = Year, y = StdRes)) +
            geom_hline(yintercept = 0) +
                geom_point(size = 3) +
                    scale_colour_grey() +
                        facet_grid(Season ~ Sex) +
                            xlab(xlab) + ylab("Standardised residual") + theme_lobview(PlotOptions)
        if ( PlotOptions$Captions )
        {
            p <- p + ggtitle(paste(source.dir, " ", stock, ": Standardised residual for Sex Ratio fits")) +
                theme(plot.title = element_text(size = 9, vjust = 2.7))
        }
        print(p)
        dev.off()
        
        #Resids v predicted
        PlotType(paste(target.dir, "/", stock, "SEXRResidPred", sep = ""), PlotOptions, width = 170, height = 200)
        plot(StdRes~Pred,data=sexr,pch=c(1,16)[Season],lwd=PlotOptions$thin,ylab="Standardised residual",xlab="Predicted")
        abline(h=0,lty=2)
        if(PlotOptions$Captions) 
        {
            mtext(paste(source.dir," ",stock,": Standardised residual for Sex Ratio fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
            mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
        }
        dev.off()
    
        #qq plot
        PlotType(paste(target.dir, "/", stock, "SEXRQQ", sep = ""), PlotOptions, width = 170, height = 200)
        qqnorm(sexr$StdRes, pch = c(1,16)[sexr$Season], xlab = "Theoretical quantiles", ylab = "Standardised residual", main = "", las = 1)
        qqline(sexr$StdRes)
        abline(h=quantile(sexr$StdRes,p=c(5,25,50,75,95)/100),lty=2)#Plot quntiles of the stdevs
        if(PlotOptions$Captions) 
        {
            mtext(paste(source.dir," ",stock,": Standardised residual for Sex Ratio fits"),side=1,line=-0.7,outer=TRUE,cex=0.7)
            mtext(paste("Closed circles: SS; Open circles: AW"),side=1,line=0,outer=TRUE,cex=0.7)
        }
        dev.off()
    }
}
