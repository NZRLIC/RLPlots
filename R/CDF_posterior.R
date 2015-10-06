#' Cumulative density function plots of the posteriors of parameters
#' 
#' Use reader.exe to view .psv file and create a more complete parampost.out
#' file (THK) Traces
#'
#' @author Darcy Webber
#' @param stock character string: a label for the stock (e.g. CRA1)
#' @param source.dir character string: the source directory. Can be a single directory or a vector
#' of directories to plot multiple chains.
#' @param target.dir character string: the directory to save the plots to
#' @param MCMCOptions list: mcmc plot options
#' @param PlotOptions list: plot options
#' @export
#' 
CDF_posterior <- function(stock, source.dir, target.dir = source.dir,
                          MCMCOptions = .MCMCOptions, PlotOptions = .PlotOptions)
{
    # How many chains are we plotting?
    Nchain <- length(source.dir)
    data <- NULL
    for (Chain in 1:Nchain)
    {
        parameter <- read.table(paste(source.dir[Chain], "/parampost.out", sep = ""), header = TRUE, as.is = TRUE, row.names = NULL)
        nam1 <- as.character(scan(paste(source.dir[Chain], "/parampost.out", sep = ""), nlines = 1, what = "character", quiet = TRUE))
        colnames(parameter) <- nam1
        indicators <- read.table(paste(source.dir[Chain], "/indicpost.out", sep = ""), header = TRUE, as.is = TRUE, row.names = NULL)
        nam2 <- as.character(scan(paste(source.dir[Chain], "/indicpost.out", sep = ""), nlines = 1, what = "character", quiet = TRUE))
        colnames(indicators) <- nam2
        Nsim <- nrow(parameter)
        d1 <- data.frame(parameter, indicators, Chain = as.factor(Chain), sample = 1:Nsim)
        names(d1) <- c(nam1, nam2, "Chain", "sample")
        data <- rbind(data, d1)
    }
    
    # create appropriate stock label
    if (length(stock) == 1) stock.label <- stock
    if (length(stock) == 2) stock.label <- paste(stock[1],substr(stock[2],4,4), sep = "")
    if (length(stock) == 3) stock.label <- paste(stock[1],substr(stock[2],4,4),substr(stock[3],4,4), sep = "")

    # If only one chain then split this chain into 3 and plot cdfs of these three
    if (Nchain == 1)
    {
        N <- floor(nrow(data) / 3)
        data$Chain <- as.character(NA)
        #factor(data, levels = c("1","2","3"))
        data$Chain[1:N] <- "1"
        data$Chain[(N+1):(2*N)] <- "2"
        data$Chain[(2*N+1):(3*N)] <- "3"
        data$Chain <- as.factor(data$Chain)
    }
    data <- data[!is.na(data$Chain),]
    
    # Delete constant columns/parameters
    loc.del <- c()
    for ( datacol in 1:ncol(data) )
    {
      if ( length(unique(data[[datacol]])) == 1 & names(data[datacol]) != "Chain" )
      {
        loc.del <- c(loc.del,datacol)
      }
    }
    if (!is.null(loc.del)) data <- data[,-loc.del]
    nam <- names(data)
    dfm <- melt(data, id.vars = c("Chain","sample"))
    
    # Do the plots
    Nplots <- ceiling(ncol(data) / MCMCOptions$n.post)
    for ( pp in 1:Nplots )
    {
        PlotType(paste(target.dir, "/", stock.label, "CDF_posterior", pp, sep = ""), PlotOptions,
                 width = 1.6*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
        cvars <- nam[((MCMCOptions$n.post * (pp - 1)) + 1):(MCMCOptions$n.post * pp)]
        dat <- subset(dfm, subset = variable %in% cvars)
        p <- ggplot(data = dat, aes(x = value, colour = Chain)) +
                 stat_ecdf(alpha = 0.7, size = 1.1) +
                 scale_colour_manual(values = PlotOptions$colourPalette) +
                 facet_wrap( ~ variable, nrow = 6, ncol = 2, scales = "free_x", drop = TRUE) +
                 xlab(NULL) + ylab(NULL) + theme_lobview(PlotOptions)
        print(p)
        dev.off()
    }

}
