#' MCMC All
#' 
#' Function which does everything, includes all the individual functions
#'
#' @author Darcy Webber, Charles Edwards
#' @export
#' 
MCMC.All <- function(stock, source.dir, target.dir = source.dir, BioOptions = .BioOptions,
                     PlotOptions = .PlotOptions, MCMCOptions = .MCMCOptions)
{
    cat("Plotting traces\n")
    cat("Plotting histograms\n")
    cat("Plotting cross correlations\n")
    TraceHisto(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    cat("Plotting running mean and cumulative quantiles\n")
    RunningAvgShort(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    cat("Plotting CPUE posterior and fit\n")
    CPUE_posterior(stock, source.dir, target.dir, CPUEOptions, MCMCOptions, PlotOptions)
    cat("Plotting catch rate posterior and fit\n")
    CR_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    cat("Plotting biomass posteriors\n")
    Bio_posterior(stock, source.dir, target.dir, MCMCOptions, BioOptions, PlotOptions)
    cat("Plotting recruitment posteriors\n")
    Rec_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    cat("Plotting surplus production posteriors\n")
    SP_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    cat("Plotting maturity posteriors\n")
    Mature_posterior(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting selectivity posteriors\n")
    Select_posterior(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting cumulative density functions\n")
    CDF_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
}
