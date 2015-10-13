#' Plot MPE diagnostic output against a 
#' single control rule parameter
#'
#' @author Charles Edwards, D'Arcy Webber
#' @export
#' 
mpeview.bypar <- function(dat, pars, stock, axis.labels, file.suffix = "plot1", target.dir = ".")
{
	
    # first argument in pars vector should
    # be "par1", "par2", .... , "par10"
    # to match parameters in MPE control file
  
    npar <- length(pars)
    nmdl <- nlevels(dat$model)
    
    if (npar == 1) stop("Only one diagnostic parameter\n")
  
    if (missing(axis.labels))
    {
        axis.labels <- list()
        axis.labels[['x']] <- pars[1]
        axis.labels[['y']] <- pars[-1]
    }
    
    dat[,pars[1]] <- as.factor(dat[,pars[1]])
  
    dfr <- data.frame(model = dat$model, par.y = pars[2], x = dat[,pars[1]], y = dat[,pars[2]])
    
    ggplot(dfr) + geom_boxplot(aes(x,y))
    
    if (npar > 2)
    {
        for(i in 3:npar)
        {
          dfr <- rbind(dfr, data.frame(model = dat$model, par.y = pars[i], x = dat[,pars[1]], y = dat[,pars[i]]))
        }
    }
    dfr$par.y <- factor(dfr$par.y, labels = axis.labels[['y']])
  
    p <- ggplot(dfr)
    
    if (nmdl > 1)
    {
        p <- p + geom_boxplot(aes(x, y, fill = model), alpha = 0.7) + labs(fill = 'Model') 
    } else {
        p <- p + geom_boxplot(aes(x, y), alpha = 0.7)
    }
    
    p <- p + facet_wrap(~ par.y, scales = "free_y") + 
        labs(x = axis.labels[['x']], y = "") + 
        theme_lobview(PlotOptions) +
        scale_colour_manual(values = PlotOptions$colourPalette) + 
        scale_fill_manual(values = PlotOptions$colourPalette)
	
    if (PlotOptions$Captions)
    {
        p <- p + ggtitle(paste(source.dir, "", stock, ": MPE output")) +
            theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
	
    PlotType(paste(target.dir, "/", stock, "mpe_", file.suffix, sep = ""),
             width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
    print(p)
    dev.off()
}
