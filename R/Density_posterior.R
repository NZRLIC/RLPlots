#' Density plots
#' 
#' Use reader.exe to view .psv file and create a more complete parampost.out
#' file (THK) Traces
#'
#' @author D'Arcy Webber
#' @export
#' 
Density_posterior <- function(stock, source.dir, target.dir = source.dir[1])
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
  
    dfm <- melt(data, id.vars = c("Chain","sample"))
    
    # Do the plots
    Nplots <- ceiling(ncol(data) / MCMCOptions$n.post)
    for ( pp in 1:Nplots )
    {
        PlotType(paste(target.dir, "/", stock, "Density_posterior", pp, sep = ""),
                 width = 1.6*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
        cvars <- nam[((MCMCOptions$n.post * (pp - 1)) + 1):(MCMCOptions$n.post * pp)]
        dat <- subset(dfm, subset = variable %in% cvars)
        p <- ggplot(data = dat, aes(x = value, colour = Chain, fill = Chain)) +
                 geom_density(alpha = 0.35) +
                 scale_colour_manual(values = PlotOptions$colourPalette) +
                 scale_fill_manual(values = PlotOptions$colourPalette) +
                 facet_wrap( ~ variable, nrow = 6, ncol = 2, scales = "free", drop = TRUE) +
                 xlab(NULL) + ylab("Density\n") + theme_lobview(PlotOptions)
        print(p)
        dev.off()
        
        PlotType(paste(target.dir, "/", stock, "Histogram_posterior", pp, sep = ""),
                 width = 1.6*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
        cvars <- nam[((MCMCOptions$n.post * (pp - 1)) + 1):(MCMCOptions$n.post * pp)]
        dat <- subset(dfm, subset = variable %in% cvars)
        p <- ggplot(data = dat, aes(x = value, colour = Chain, fill = Chain)) +
          geom_histogram(position="stack") +
          scale_colour_manual(values = PlotOptions$colourPalette) +
          scale_fill_manual(values = PlotOptions$colourPalette) +
          facet_wrap( ~ variable, nrow = 6, ncol = 2, scales = "free", drop = TRUE) +
          xlab("\nSample") + ylab(NULL) + theme_lobview(PlotOptions)
        suppressMessages(suppressWarnings(print(p)))
        dev.off()
    }

}
