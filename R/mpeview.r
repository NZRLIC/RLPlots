#' MPE output viewer
#'
#' Options for pars that are commonly plotted include the median of the average
#' commercial catch (AvComm_med), the median of the average recreational catch
#' (acRec_med), the AAV_med, the median of the average catch per unit effort
#' (AvI_AW_med)
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
#' @export
#' 
mpeview <- function(dat, pars, stock, axis.labels, point.col = c("rules"),
                    file.suffix = "plot1", target.dir,
                    PlotOptions = .PlotOptions)
{
    npar <- length(pars)
    nmdl <- nlevels(dat$model)

    if (npar == 1) stop("Only one diagnostic parameter\n")

    if (missing(axis.labels))
    {
        axis.labels <- list()
        axis.labels[['x']] <- pars[1]
        axis.labels[['y']] <- pars[-1]
    }

    # Do we want to plot a par as a colour? If not still need to set a default
    if (!grepl("par", point.col))
    {
        pt.par <- which(names(dat) == "par1")
    } else {
        pt.par <- which(names(dat) == point.col)
    }

    dfr <- data.frame(model = dat$model, rule = dat$rule, par.col = dat[,pt.par], par.y = pars[2], x = dat[,pars[1]], y = dat[,pars[2]])
    if (npar > 2)
    {
        for(i in 3:npar)
        {
          dfr <- rbind(dfr, data.frame(model = dat$model, rule = dat$rule, par.col = dat[,pt.par], par.y = pars[i], x = dat[,pars[1]], y = dat[,pars[i]]))
        }
    }
    dfr$par.y <- factor(dfr$par.y, labels = axis.labels[['y']])

    p <- ggplot(dfr)
   
    if (nmdl > 1)
    {
        p <- p + geom_point(aes(x, y, col = model), alpha = 0.7, size = 2.5) +
            labs(col = "Model") +
            scale_colour_manual(values = PlotOptions$colourPalette)
    } else if (point.col == "rules") {
        p <- p + geom_point(aes(x, y, group = rule, col = rule), alpha = 0.7, size = 2.5) +
            labs(col = "Rule") +
            scale_colour_gradient(low = PlotOptions$colourPalette[1], high = PlotOptions$colourPalette[2])
    } else {
        p <- p + geom_point(aes(x, y, group = par.col, col = par.col), alpha = 0.7, size = 2.5) +
            labs(col = point.col) +
            scale_colour_gradient(low = PlotOptions$colourPalette[1], high = PlotOptions$colourPalette[2])
    }

    p <- p + facet_wrap(~ par.y, scales = "free_y") + 
        labs(x = axis.labels[['x']], y = "") + 
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
