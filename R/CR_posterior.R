#' Catch rate (CR) posterior plots
#' 
#' Plots the posterior fit to historic catch rate data and residual diagnostics
#'
#' @author Darcy Webber and Charles Edwards
#' @param stock character string: label for the stock (e.g. CRA1)
#' @param source.dir character string: the directory containing the ADMB output files
#' @param target.dir character string: the directory to save the plots to
#' @export
#' 
CR_posterior <- function(stock, source.dir, target.dir = source.dir)
{
    # Load posterior
    dat <- as.matrix(read.table(paste(source.dir, "/", stock, "CRpost.out", sep = ""), header = TRUE, as.is = TRUE))
    colnames(dat) <- as.character(scan(paste(source.dir, "/", stock, "CRpost.out", sep = ""), nlines = 1, quiet = TRUE))
    names(dimnames(dat)) <- c("iter","year")
    dat <- melt(dat)
    dat <- data.frame(dat, season = NA)
    dat$season[dat$year %% 1 == 0] <- 1
    dat$season[dat$year %% 1 != 0] <- 2
    dat$season[dat$season == 1] <- "AW"
    dat$season[dat$season == 2] <- "SS"
    dat$year <- as.integer(drop(dat$year))

    # Load observations
    if ( file.exists(paste(source.dir, "/", stock, "CRResids.out", sep = "")) )
    {
        cpue <- read.table(paste(source.dir, "/", stock, "CRResids.out", sep = ""), header = TRUE, as.is = TRUE)
        if ( PlotOptions$UsePeriod )
        {
            cpue$x <- cpue$Period
            xlab <- "Period"     
        } else {
            cpue$x <- PeriodToFishingYear(cpue$Period)
            xlab<-"Fishing year"
        }
        cpue$logObs <- log(cpue$Obs)
        cpue$LB <- exp(cpue$logObs - cpue$Sigma)
        cpue$UB <- exp(cpue$logObs + cpue$Sigma)
        cpue$season <- cpue$Season
        cpue$season[cpue$season == 1] <- "AW"
        cpue$season[cpue$season == 2] <- "SS"
    } else {
        cat("Warning: no CRResids.out file in the source directory, observations will not be plotted.\n")
    }

    # Do the plot
    p <- ggplot(data = dat, aes(x = year, y = value)) +
          scale_x_continuous(breaks = unique(dat$year)) +
          stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
          stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
          stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
          facet_grid(. ~ season) +
          theme_lobview(PlotOptions) +
          labs(x = "\nFishing year", y = "CR index\n")
    if ( file.exists(paste(source.dir, "/", stock, "CRResids.out", sep = "")) )
    {
        p <- p + geom_pointrange(data = cpue, aes(x = Year, y = Obs, ymax = UB, ymin = LB))
    }
    if ( PlotOptions$Captions )
    {
        p <- p + ggtitle(paste(source.dir, " ", stock, ": Posterior CPUE fit")) +
            theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
    
    PlotType(paste(target.dir, "/", stock, "CR_posterior", sep = ""),
             width = 2*PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
    print(p)
    dev.off()
}
