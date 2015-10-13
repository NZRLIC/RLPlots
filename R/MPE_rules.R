#' Plot control rules
#'
#' @author D'Arcy N. Webber, Charles Edwards
#' @param control.pars a matrix of control parameters with columns as parameters
#' and rows as different rules
#' @param obs.cpue the current CPUE
#' @param cpue a vector of the catch per unit effort (CPUE) to be plotted as the x
#' axis
#' @param target.dir a character string of the target directory to save the plot to
#' @param file.suffix a suffix to append to the end of the saved plot name
#' @return a plot
#' @export
#' 
MPE_rules <- function(control.pars, current.cpue = NA, cpue = seq(0, 2, 0.01), target.dir,
                      file.suffix = "plot1")
{
    # par1  rule type
    # par2  CPUE at TACC = 0
    # par3  CPUE at plateau left
    # par4  CPUE at plateau right
    # par5  plateau height
    # par6  slope
    # par6  step width
    # par7  step height
    # par8  minimum change
    # par9  maximum change
    # par10 latent year switch
    
    #if ( par[2] >= par[3] )
    #    stop("par2 < par3 condition not met.")
    #if ( par[3] > par[4] )
    #    stop("par3 <= par4 condition not met.")
    #if ( par[1] == 3 ) cat("Step rule\n")
    #if ( par[1] == 4 ) cat("Slope rule\n")

    outs <- apply(control.pars, 1, FUN = TACC, cpue = cpue)
    rules <- melt(outs, varnames = c("cpue", "rule"), value.name = "TACC")
    rules$cpue <- rep(cpue, nrow(control.pars))
    current.tacc <- as.numeric(apply(control.pars, 1, FUN = TACC, cpue = current.cpue))

    # If just one rule is being plotted then do that and plot the obs.cpue on the plot too
    if (nrow(control.pars) == 1)
    {
        d <- data.frame(cpue = current.cpue, tacc = current.tacc)
        p <- ggplot(rules, aes(x = cpue, y = TACC)) +
            geom_line() +
            geom_point(data = d, aes(x = cpue, y = tacc), size = 5, col = PlotOptions$colourPalette[2]) +
            theme_lobview(PlotOptions) + labs(x = "\nCPUE", y = "TACC (tonnes)\n", col = "Rule")
    } else {
        p <- ggplot(rules, aes(x = cpue, y = TACC, group = rule, col = rule)) +
            geom_line() +
            #guides(colour = guide_legend(reverse = FALSE), size = 2) +
            scale_colour_gradient(low = PlotOptions$colourPalette[1], high = PlotOptions$colourPalette[2]) +
            theme_lobview(PlotOptions) + labs(x = "\nCPUE", y = "TACC (tonnes)\n", col = "Rule")
    }

    #if ( PlotOptions$Captions )
    #{
    #    p <- p + ggtitle(paste(": control rules")) +
    #                 theme(plot.title = element_text(size = 9, vjust = 2.7))
    #}

    # if target directory is not specified then print to screen only; otherwise save
    if (missing(target.dir))
    {
        print(p)
    } else {
        PlotType(paste(target.dir, "/", "Control_rules_", file.suffix, sep = ""),
                 width = 1.1*PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
        print(p)
        dev.off()
    }
}
