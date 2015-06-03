#' MCMC multichain plots
#' 
#' Function which does everything, includes all the individual functions
#'
#' @author Darcy Webber
#' @export
#' 
Multichain.All <- function(stock, source.dir, target.dir = source.dir,
                           MCMCOptions = .MCMCOptions, PlotOptions = .PlotOptions)
{
    cat("Plotting traces\n")
    Trace_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    cat("Plotting densities and stacked histograms\n")
    Density_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    cat("Plotting cumulative density functions\n")
    CDF_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
}
