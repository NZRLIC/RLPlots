#' Posterior recruitment plots
#'
#' @param stock a label for the stock (e.g. CRA1)
#' @param source.dir the directory containing the ADMB output files
#' @param target.dir the directory to save the plots to
#' @return a plot
#' @export
#' 
Rec_posterior <- function(stock, source.dir, target.dir = source.dir)
{
    dat <- read.table(paste(source.dir, "/parampost.out", sep = ""), header = TRUE, as.is = TRUE, row.names = NULL)
    dat <- dat[,-ncol(dat)] # This removes the final year as the model spits out something weird
    lnR0 <- dat[,regexpr("lnR0", colnames(dat)) > 0, drop = FALSE]
    sigmaR <- dat$sigmaR
    
    for (i in 1:length(stock))
    {
        dat <- as.matrix(read.table(paste(source.dir, "/", stock[i], "Rdev.out", sep = ""), header = TRUE, as.is = TRUE))
        colnames(dat) <- as.character(scan(paste(source.dir, "/", stock[i], "Rdev.out", sep = ""), nlines = 1, quiet = TRUE))
        dat <- dat[,-ncol(dat)]
        names(dimnames(dat)) <- c("iter", "year")
        dat <- melt(dat, value.name = "Rdev")
        dat <- data.frame(dat, lnR0 = lnR0[,i], sigmaR = sigmaR)
        dat <- subset(dat, year <= PeriodToFishingYear(PlotOptions$ModelEndPeriod))
        dat <- data.frame(dat, R = exp(dat$lnR0 + dat$Rdev - 0.5*dat$sigmaR^2)/1e6)
    
        p <- ggplot(dat, aes(x = year, y = R)) + 
            stat_summary(fun.ymin = function(x) quantile(x, 0.05),fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
            stat_summary(fun.ymin = function(x) quantile(x, 0.25),fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
            stat_summary(fun.y = function(x) median(x), geom = "line", lwd = 1) +
            geom_vline(xintercept = max(dat$year)-3.5, size = 0.1, linetype = "longdash") +
            theme_lobview(PlotOptions) + scale_y_continuous(limits = range(0, quantile(dat$R, 0.99))) +
            labs(x = "\nYear", y = "Numbers recruited (millions)\n")

        # Add caption
        if ( PlotOptions$Captions )
        {
            p <- p + ggtitle(paste(source.dir, " ", stock[i], ": Posterior recruitment trajectory")) +
                theme(plot.title = element_text(size = 9, vjust = 2.7))
        }

        # Do the plot
        PlotType(paste(target.dir, "/", stock[i], "Rec_posterior", sep = ""),
                 width = 2*PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
        print(p)
        dev.off()
    }
}
