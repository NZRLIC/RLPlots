#' Posterior surplus production plot
#'
#' @param stock a label for the stock (e.g. CRA1)
#' @param source.dir the directory containing the ADMB output files
#' @param target.dir the directory to save the plots to
#' @param PlotOptions plot options
#' @param MCMCOptions mcmc plot options
#' @export
#' 
SP_posterior <- function(stock, source.dir, target.dir = source.dir, MCMCOptions = .MCMCOptions, 
                         PlotOptions = .PlotOptions)
{
    # organise data
    dat <- as.matrix(read.table(paste(source.dir,"/",stock,"surprod.out", sep = ""), header = TRUE, as.is = TRUE))
    colnames(dat) <- as.character(scan(paste(source.dir,"/",stock,"surprod.out", sep = ""), nlines = 1, quiet = TRUE))
    dat <- dat[,-ncol(dat)]
    names(dimnames(dat)) <- c("iter","year")
    dat <- melt(dat,value.name="SP")
    dat <- subset(dat,year<=PeriodToFishingYear(PlotOptions$ModelEndPeriod))
    
    # plot
    p <- ggplot(dat,aes(x=year,y=SP)) + 
        stat_summary(fun.ymin=function(x) quantile(x,0.05),fun.ymax=function(x) quantile(x,0.95),geom='ribbon',alpha=0.25) +
        stat_summary(fun.ymin=function(x) quantile(x,0.25),fun.ymax=function(x) quantile(x,0.75),geom='ribbon',alpha=0.5) +
        stat_summary(fun.y=function(x) mean(x),geom='line',lwd=1) +
        geom_vline(xintercept=max(dat$year)-3.5,size=0.1,linetype = "longdash") +
        theme_lobview(PlotOptions) + scale_y_continuous(limits=range(0, quantile(dat$SP,0.99))) +
        labs(x='\nYear', y='Surplus production (tonnes)\n')
  
    if ( PlotOptions$Captions )
    {
        p <- p + ggtitle(paste(source.dir, " ", stock, ": Posterior surplus production trajectory")) +
            theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
    
    PlotType(paste(target.dir, "/", stock, "SP_posterior", sep = ""), PlotOptions,
             width = 2*PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
    print(p)
    dev.off()
}
