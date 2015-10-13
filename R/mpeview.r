#' MPE output viewer
#'
#' Options for pars that are commonly plotted include the median of the average
#' commercial catch (AvComm_med), the median of the average recreational catch
#' (acRec_med), the AAV_med, the median of the average catch per unit effort
#' (avI_AW_med). 
#'
#' @author Charles Edwards, Darcy Webber
#' @param dat a data.frame read in using read.sims.sum
#' @param pars the parameters that can be plotted. The first par becomes the x
#' axis, the remaining pars different panels (choose up to 4 pars)
#' @param stock a stock label
#' @param axis.labels named list of the form list(x,y) with x and y character vectors containing the x and y labels
#' @param point.col the variable used to colour the points, can be coloured by
#' "rules" or any of the pars (e.g. "par1", "par2", ..., "par10")
#' @param file.suffix a suffix to append to the end of the saved plot name
#' @param target.dir the directory to save the plot to
#' @param PlotOptions plot options
#' 
#' @examples
#' \dontrun{
#'  # read in the data
#'  dat <- read.sims.sum(stock = c("CRA7", "CRA8"))
#'  
#'  # plot all rules for both CRA7 and CRA8
#'  mpeview(dat, pars = c('AvComm_med', 'AAV_med', 'AvPCPUE_med'), stock = c("CRA7", "CRA8"), axis.labels = list(x = "Commercial Catch", y = c("AAV", "CPUE")))
#'  
#'  # plot all rules for CRA7 only
#'  mpeview(dat, pars = c('AvComm_med', 'AAV_med', 'AvPCPUE_med'), stock = "CRA7", axis.labels = list(x = "Commercial Catch", y = c("AAV", "CPUE")))
#' }
#' 
#' @export
#' 
mpeview <- function(dat, pars, stock, axis.labels, point.col = "rule",
                    file.suffix = "plot1", target.dir,
                    PlotOptions = .PlotOptions)
{
    npar <- length(pars)
    nmdl <- nlevels(dat$model)
    nstk <- nlevels(dat$stock)

    if (npar == 1) stop("Only one diagnostic parameter\n")
    
    if (nstk > 1 & length(stock) == 1) {
        dat  <- subset(dat, stock == stock)
        nstk <- 1
    }

    if (missing(axis.labels))
    {
        axis.labels <- list()
        axis.labels[['x']] <- pars[1]
        axis.labels[['y']] <- pars[-1]
    }

    pt.par <- which(names(dat) == point.col)

    dfr <- data.frame(stock = dat$stock, model = dat$model, rule = dat$rule, par.col = dat[,pt.par], par.y = pars[2], x = dat[,pars[1]], y = dat[,pars[2]])
    if (npar > 2)
    {
        for(i in 3:npar)
        {
          dfr <- rbind(dfr, data.frame(stock = dat$stock, model = dat$model, rule = dat$rule, par.col = dat[,pt.par], par.y = pars[i], x = dat[,pars[1]], y = dat[,pars[i]]))
        }
    }
    dfr$par.y <- factor(dfr$par.y, labels = axis.labels[['y']])

    p <- ggplot(dfr)
   
    if (nmdl > 1)
    {
        p <- p + geom_point(aes(x, y, col = model), alpha = 0.7, size = 2.5) +
            labs(col = "Model") +
            scale_colour_manual(values = PlotOptions$colourPalette)
    } else if (point.col == "rule") {
        p <- p + geom_point(aes(x, y, group = rule, col = rule), alpha = 0.7, size = 2.5) +
            labs(col = "Rule") +
            scale_colour_gradient(low = PlotOptions$colourPalette[1], high = PlotOptions$colourPalette[2])
    } else {
        p <- p + geom_point(aes(x, y, group = par.col, col = par.col), alpha = 0.7, size = 2.5) +
            labs(col = point.col) +
            scale_colour_gradient(low = PlotOptions$colourPalette[1], high = PlotOptions$colourPalette[2])
    }
    
    if (nstk > 1) {
        p <- p + facet_grid(stock ~ par.y, scales = "free")
    } else {
        p <- p + facet_wrap(~ par.y, scales = "free_y")
    }
    p <- p + labs(x = axis.labels[['x']], y = "") + 
        theme_lobview(PlotOptions)

    #if (PlotOptions$Captions)
    #{
    #    p <- p + ggtitle(paste(": MPE output")) +
    #        theme(plot.title = element_text(size = 9, vjust = 2.7))
    #}

    # if target directory is not specified then print to screen only; otherwise save
    if (missing(target.dir))
    {
        print(p)
    } else {
        PlotType(paste(target.dir, "/", stock, "mpe_", file.suffix, sep = ""), PlotOptions,
                 width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
      print(p)
      dev.off()
    }
}
