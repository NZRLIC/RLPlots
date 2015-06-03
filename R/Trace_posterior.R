#' Trace plots
#' 
#' Use reader.exe to view .psv file and create a more complete parampost.out
#' file (THK) Traces
#'
#' @export
#' 
Trace_posterior <- function(stock, source.dir, target.dir = source.dir[1],
                            MCMCOptions = .MCMCOptions, PlotOptions = .PlotOptions)
{
    # How many chains are we plotting?
    Nchain <- length(source.dir)
    data <- NULL
    for (Chain in 1:Nchain)
    {
        parameter <- read.table(paste(source.dir[Chain], "/parampost.out", sep = ""), header = TRUE, as.is = TRUE)
        nam1 <- as.character(scan(paste(source.dir[Chain], "/parampost.out", sep = ""), nlines = 1, what = "character", quiet = TRUE))
        indicators <- read.table(paste(source.dir[Chain], "/indicpost.out", sep = ""), header = TRUE, as.is = TRUE)
        nam2 <- as.character(scan(paste(source.dir[Chain], "/indicpost.out", sep = ""), nlines = 1, what = "character", quiet = TRUE))
        Nsim <- nrow(parameter)
        d1 <- data.frame(parameter, indicators, as.factor(Chain), 1:Nsim)
        names(d1) <- c(nam1, nam2, "Chain", "sample")
        data <- rbind(data, d1)
    }
    
    # Delete constant columns/parameters
    loc.del <- c()
    for ( datacol in 1:ncol(data) )
    {
      if ( length(unique(data[[datacol]])) == 1 & names(data[datacol]) != "Chain" )
      {
        loc.del <- c(loc.del,datacol)
      }
    }
    data <- data[,-loc.del]
    nam <- names(data)
    Nsim <- nrow(data)
  
    dfm <- melt(data, id.vars = c("Chain","sample"))

    # Add a running mean line but only if a single chain is being plotted
    if (Nchain == 1)
    {
        i <- which(!names(data) %in% c("Chain","sample"))
        ma <- apply(data[,i], 2, moving.average, n = MCMCOptions$running.mean)
        ma <- melt(ma)
        dfm$ma <- ma$value
        #dfm$variable <- factor(rep(nam, each = Nsim))
    }
    
    # Do the plots
    Nplots <- ceiling(ncol(data) / MCMCOptions$n.post)
    for ( pp in 1:Nplots )
    {
        PlotType(paste(target.dir, "/", stock, "Trace_posterior", pp, sep = ""), PlotOptions,
                 width = 1.6*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
        cvars <- nam[((MCMCOptions$n.post * (pp - 1)) + 1):(MCMCOptions$n.post * pp)]
        dat <- subset(dfm, subset = variable %in% cvars)
        p <- ggplot(data = dat, aes(x = sample, y = value, colour = Chain)) +
                 geom_line(alpha = 0.7) +
                 scale_colour_manual(values = PlotOptions$colourPalette) +
                 facet_wrap( ~ variable, nrow = 6, ncol = 2, scales = "free_y", drop = TRUE) +
                 xlab("\nSample") + ylab(NULL) + theme_lobview(PlotOptions)
        if (Nsim == 1) p <- p + geom_line(aes(x = sample, y = ma), colour = PlotOptions$colourPalette[2])
        print(p)
        dev.off()
    }

}
