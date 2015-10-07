#' Trace plots and histograms
#' 
#' Use reader.exe to view .psv file and create a more complete parampost.out
#' file (THK) Traces
#'
#' @export
#' 
TraceHisto <- function(stock, source.dir, target.dir = source.dir,
                       MCMCOptions = .MCMCOptions, PlotOptions = .PlotOptions)
{
    parameter <- read.table(paste(source.dir, "/parampost.out", sep = ""), header = TRUE, as.is = TRUE, row.names = NULL)
    nam <- as.character(scan(paste(source.dir, "/parampost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
    colnames(parameter) <- nam
    indicators <- read.table(paste(source.dir, "/indicpost.out", sep = ""), header = TRUE, as.is = TRUE, row.names = NULL)
    nam <- as.character(scan(paste(source.dir, "/indicpost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
    colnames(indicators) <- nam
    data <- cbind(parameter, indicators)
    
    # create appropriate stock label
    if (length(stock) == 1) stock.label <- stock
    if (length(stock) == 2) stock.label <- paste(stock[1],substr(stock[2],4,4), sep = "")
    if (length(stock) == 3) stock.label <- paste(stock[1],substr(stock[2],4,4),substr(stock[3],4,4), sep = "")
    
    i <- grep("1B[0-9]", names(data))
    n <- names(data[,i])
    ii <- which.min(as.numeric(gsub("1B", "", n)))
    j <- i[ii]
    data$Bcurr_Bref <- data[,j] / data[,"1Bref"]
    data$Bcurr_Bmsy <- data[,j] / data[,"1Bmsy"]

    dataMPD <- data[1,]
  
    # delete constant columns
    loc.del <- c()
    for ( datacol in 1:ncol(data) )
    {
        if ( length(unique(data[[datacol]])) == 1 )
        {
            loc.del <- c(loc.del,datacol)
        }
    }
    data <- data[,-loc.del]
    dataMPD <- dataMPD[-loc.del]
    nam <- names(data)
    
    Nsim <- nrow(data)
    Nplots <- ceiling(ncol(data) / MCMCOptions$n.post)
    dfm <- data.frame(melt(data, id.vars = NULL), sample = 1:Nsim, chain = 1)
    dfm$mpd <- rep(as.numeric(dataMPD), each = Nsim)
    
    ma <- apply(data, 2, moving.average, n = MCMCOptions$running.mean)
    ma <- melt(ma)
    dfm$ma <- ma$value
    dfm$variable <- factor(rep(nam, each = Nsim))
    
    #===============================================================================
    # Trace plots
    #===============================================================================
    for ( pp in 1:Nplots )
    {
        PlotType(paste(target.dir, "/", stock.label, "Trace", pp, sep = ""), PlotOptions,
                 width = 1.5*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
        dat <- droplevels(dfm[((Nsim * MCMCOptions$n.post * (pp - 1)) + 1):(Nsim * MCMCOptions$n.post * pp),])
        if (any(is.na(dat$variable))) dat <- dat[!is.na(dat$variable),]
        p <- ggplot(data = dat) +
            geom_line(aes(x = sample, y = value)) + 
            geom_line(aes(x = sample, y = ma), colour = PlotOptions$colourPalette[2]) +
            scale_colour_grey() +
            facet_wrap( ~ variable, nrow = 6, ncol = 2, scales = "free_y", drop = TRUE) +
            xlab("Sample") + ylab(NULL) + theme_lobview(PlotOptions)
        print(p)
        dev.off()
    }
    
    #===============================================================================
    # Histograms
    #===============================================================================
    for ( pp in 1:Nplots )
    {
        PlotType(paste(target.dir, "/", stock, "Histo", pp, sep = ""), PlotOptions,
                 width = 1.5*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
        dat <- droplevels(dfm[((Nsim * MCMCOptions$n.post * (pp - 1)) + 1):(Nsim * MCMCOptions$n.post * pp),])
        if (any(is.na(dat$variable))) dat <- dat[!is.na(dat$variable),]
        p <- ggplot(data = dat, aes(x = value)) +
            geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
            scale_colour_grey() +
            facet_wrap( ~ variable, nrow = 6, ncol = 2, scales = "free", drop = TRUE) +
            xlab(NULL) + ylab(NULL) + theme_lobview(PlotOptions)
          {
              p <- p + geom_vline(data = dat, aes(xintercept = mpd), color = 2, size = 1)
          }
        suppressMessages(suppressWarnings(print(p)))
        dev.off()
    }
    
    #===============================================================================
    # Correlation plots
    #===============================================================================
    PlotType(paste(target.dir, "/", stock, "CrossCorr", sep = ""), PlotOptions,
             width = 2*PlotOptions$plotsize[1], height = 2*PlotOptions$plotsize[2])
    xx <- mcmc.list(as.mcmc(data))
    crosscorrelation.plot(xx)
    dev.off()
    
    xx <- list()
    xx[[1]] <- data[,2:7]
    xx[[2]] <- data[,c(8,10,11,12,14,15)]
    xx[[3]] <- data[,16:27]
    for ( pp in 1:length(xx) )
    {
        PlotType(paste(target.dir, "/", stock, "Corr", pp, sep = ""), PlotOptions,
                 width = 1.5*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
        #p <- ggpairs(data[,2:7])
        #        print(p)
        #        dev.off()
        pairs(xx[[pp]], lower.panel = panel.cor, cex = 0.6, pch = 16, las = 1, gap = 0.2)
        dev.off()
    }
}
