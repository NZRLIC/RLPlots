#' Posterior biomass plots
#' 
#' Plots the vulnerable biomass (for Autumn Winter and Spring Summer) and the
#' total biomass over time. Median and the 90% credibility intervals are
#' shown. For the total biomass, 50% intervals are also given.
#'
#' @author Charles Edwards, D'Arcy Webber
#' @param stock character string: label for the stock (e.g. CRA1)
#' @param source.dir character string: the directory containing the ADMB output files
#' @param target.dir character string: the directory to save the plots to
#' @export
#' 
Bio_posterior <- function(stock, source.dir, target.dir = source.dir)
{
    # ==============================================================================
    # Plot VulnB by season
    # ==============================================================================
    if ( BioOptions$VulnB )
    {
        # organise data
        dat<-as.matrix(read.table(paste(source.dir,"/",stock,"bvulnref.out",sep=""),header=T,as.is=T))
        colnames(dat) <- as.character(scan(paste(source.dir,"/",stock,"bvulnref.out",sep=""),nlines=1, quiet = TRUE))
        names(dimnames(dat)) <- c("iter","year")
        dat <- melt(dat)
        dat <- data.frame(dat,season=2)
        dat$season[as.numeric(dat$year)%%1==0] <- 1
        dat$season[dat$season==1] <- "AW"
        dat$season[dat$season==2] <- "SS"
        dat$year <- as.integer(dat$year)
        dat <- subset(dat, year<=PeriodToFishingYear(PlotOptions$ModelEndPeriod))
    
        # plot
        p <- ggplot(dat, aes(x = year, y = value, col = season, fill = season)) + 
            stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = 'ribbon', alpha = 0.4, colour = NA) +
            stat_summary(fun.y = function(x) quantile(x, 0.5), geom = 'line', lwd = 1) +
            scale_colour_manual(values = PlotOptions$colourPalette) +
            scale_fill_manual(values = PlotOptions$colourPalette) +
            geom_vline(xintercept = max(dat$year) - 3.5, size = 0.1, linetype = "longdash") +
            theme_lobview(PlotOptions) + scale_y_continuous(limits = range(0, dat$value)) +
            labs(x = '\nYear', y = 'Vulnerable biomass (tonnes)\n', fill = 'Season', col = 'Season')
    
        if ( PlotOptions$Captions )
        {
            p <- p + ggtitle(paste(source.dir, " ", stock, ": Posterior vulnerable biomass trajectory")) +
                theme(plot.title = element_text(size = 9, vjust = 2.7))
        }
        
        PlotType(paste(target.dir, "/", stock, "bvulnref_posterior", sep = ""),
                 width = 2*PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
        print(p)
        dev.off()
    }
  
    # ==============================================================================
    # Plot Ball for AW (season 1) only
    # ==============================================================================
    if ( BioOptions$TotalB )
    {
        # organise data
        dat<-as.matrix(read.table(paste(source.dir,"/",stock,"Ball.out",sep=""),header=T,as.is=T))
        colnames(dat) <- as.character(scan(paste(source.dir,"/",stock,"Ball.out",sep=""),nlines=1, quiet = TRUE))
        names(dimnames(dat)) <- c("iter","year")
        dat <- melt(dat)
        dat <- data.frame(dat,season=2)
        dat$season[as.numeric(dat$year)%%1==0] <- 1
        dat$season[dat$season==1] <- "AW"
        dat$season[dat$season==2] <- "SS"
        dat$year <- as.integer(dat$year)
        dat <- subset(dat,year<=PeriodToFishingYear(PlotOptions$ModelEndPeriod))
        dat <- subset(dat,season=="AW")
        
        # plot
        p <- ggplot(dat, aes(x = year, y = value)) + 
            stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
            stat_summary(fun.ymin = function(x) quantile(x, 0.25),fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
            stat_summary(fun.y = function(x) median(x), geom = "line", lwd = 1) +
            geom_vline(xintercept = max(dat$year)-3.5, size = 0.1, linetype = "longdash") +
            theme_lobview(PlotOptions) + scale_y_continuous(limits = range(0, dat$value)) +
            labs(x = "\nYear", y = "Total biomass (tonnes)\n")
        
        if ( PlotOptions$Captions )
        {
            p <- p + ggtitle(paste(source.dir, " ", stock, ": Posterior total biomass trajectory")) +
                theme(plot.title = element_text(size = 9, vjust = 2.7))
        }
        
        PlotType(paste(target.dir, "/", stock, "Ball_posterior", sep = ""),
                 width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
        print(p)
        dev.off()
    }
}
