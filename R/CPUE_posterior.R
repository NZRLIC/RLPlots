#' CPUE posterior plots
#' 
#' Plots the posterior fit to CPUE data and residual diagnostics
#'
#' @param stock character string: label for the stock (e.g. CRA1)
#' @param source.dir character string: the directory containing the ADMB output files
#' @param target.dir character string: optional directory to save the plots to
#' @param CPUEOptions list: CPUE options
#' @param MCMCOptions list: MCMC options
#' @param PlotOptions list: plot options
#' @export
#' 
CPUE_posterior <- function(stock, source.dir, target.dir,
                           CPUEOptions = .CPUEOptions, MCMCOptions = .MCMCOptions, PlotOptions = .PlotOptions)
{
    # Load posterior
    dat <- as.matrix(read.table(paste(source.dir, "/", stock, "CPUEpost.out", sep = ""), header = TRUE, as.is = TRUE))
    colnames(dat) <- as.character(scan(paste(source.dir, "/", stock, "CPUEpost.out", sep = ""), nlines = 1, quiet = TRUE))
    names(dimnames(dat)) <- c("iter","year")
    dat <- melt(dat)
    dat <- data.frame(dat, season = NA)
    dat$season[dat$year %% 1 == 0] <- 1
    dat$season[dat$year %% 1 != 0] <- 2
    dat$season[dat$season == 1] <- "AW"
    dat$season[dat$season == 2] <- "SS"
    dat$year <- as.integer(dat$year)

    # Load observations if the file exists in the source directory
    if ( file.exists(paste(source.dir, "/", stock, "CPUEResids.out", sep = "")) )
    {
        cpue <- read.table(paste(source.dir, "/", stock, "CPUEResids.out", sep = ""), header = TRUE, as.is = TRUE)
        if ( PlotOptions$UsePeriod )
        {
            cpue$x<-cpue$Period
            xlab<-"Period"     
        } else {
            cpue$x<-PeriodToFishingYear(cpue$Period)
            xlab<-"Fishing year"
        }
        cpue$logObs <- log(cpue$Obs)
        cpue$LB <- exp(cpue$logObs - cpue$Sigma)
        cpue$UB <- exp(cpue$logObs + cpue$Sigma)
        cpue$season <- cpue$Season
        cpue$season[cpue$season == 1] <- "AW"
        cpue$season[cpue$season == 2] <- "SS"
    } else {
        cat("Warning: no CPUEResids.out file in the source directory, observations will not be plotted.\n")
    }

    p <- ggplot(data = dat, aes(x = year, y = value)) + 
          stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
          stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
          stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
          facet_grid(. ~ season) +
          theme_lobview(PlotOptions) +
          labs(x = "\nFishing year", y = "CPUE index\n", fill = "Season", col = "Season")
    
    if ( file.exists(paste(source.dir, "/", stock, "CPUEResids.out", sep = "")) )
    {
          p <- p + geom_pointrange(data = cpue, aes(x = Year, y = Obs, ymax = UB, ymin = LB))
    }
    
    if ( PlotOptions$Captions )
    {
        p <- p + ggtitle(paste(source.dir, " ", stock, ": Posterior CPUE fit")) +
            theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
    
    if(missing(target.dir))
    {
        print(p)
    } else {
        PlotType(paste(target.dir, "/", stock, "CPUE_posterior", sep = ""), PlotOptions,
                 width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
        print(p)
        dev.off()
    }
}
  
