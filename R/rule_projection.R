#' Control rule projection
#' 
#' Produces a two dimensional histogram of CPUE/TACC values visited during MP projections
#'
#' @param stock a label for the stock (e.g. \code{"CRA1"})
#' @param control.file MPE control file (e.g. \code{"Base.mpe"})
#' @param source.dir the directory containing the ADMB output files
#' @param target.dir the directory to save the plots to
#' @param cpue.range range of offset CPUE values over which to plot
#' @return a plot
#' @export
#'
rule_projection <- function(stock, control.file, source.dir, target.dir = source.dir, cpue.range = seq(0, 4.5, length = 1001)) {
    
    # get current rule(s)
    mpe.pars <- read.mpe.pars(stock = stock, control.file = paste(source.dir, "/", control.file, sep = ""))
    
    mpe.pars.list <- list()
    
    for (s in stock) {
        mpe.pars.list[[s]] <- mpe.pars[[s]]
        
        # define rule
        cpue <- cpue.range
        tacc <- TACC(as.numeric(mpe.pars.list[[s]]), cpue = cpue)
        
        dfr <- data.frame(cpue, tacc)
        
        rf <- colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
        r <- rf(32)
        
        res <- read.table(paste(s, "allresults.out", sep = ""), header = TRUE)
        res <- data.frame(iter = res$count, year = res$year, cpue = res$OCPUE, tacc = res$TACC)

        # offset
        nyr <- length(unique(res$year))
        res <- plyr::ddply(res, "iter", plyr::summarize, year = year[1:(nyr - 1)], cpue = cpue[1:(nyr - 1)], tacc = tacc[2:nyr])
        
        # plot 2dhist
        gg <- ggplot(dfr) + 
            stat_bin2d(data = res, aes(x = cpue, y = tacc), bins = 60) + scale_fill_gradientn(colours = r)  + #guides(fill = FALSE) +
            geom_line(aes(x = cpue, y = tacc), size = 1.5) +
            #coord_cartesian(xlim = c(min(cpue) - 0.1, max(cpue) + 0.1), ylim = c(-6, 166)) +
            theme_lobview(PlotOptions) + labs(x = "CPUE", y = "TACC", fill = "Samples")
        
        PlotType(paste(target.dir, "/", stock[s],"rule_projection", sep = ""), width = 2 * PlotOptions$plotsize[1], height = 10 + PlotOptions$plotsize[2])
        print(gg)
        dev.off()
        
    }
}