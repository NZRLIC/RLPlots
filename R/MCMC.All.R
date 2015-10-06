#' MCMC All
#' 
#' Function which does everything, includes all the individual functions
#'
#' @author D'Arcy Webber, Charles Edwards
#' @export
#' 
MCMC.All <- function(stock, source.dir = ".", target.dir = paste('../', stock, 'plots', sep = ''),
                     BioOptions = .BioOptions, CROptions = .CROptions,
                     PlotOptions = .PlotOptions, MCMCOptions = .MCMCOptions)
{
    emsg <- "OOPS - EITHER YOU FUCKED UP OR THE CODE DID, the original error message is:"
    efun <- function(e) { message(emsg); message(e) }
    
    if (!dir.exists(target.dir))
    {
        dir.create(target.dir)
        message('created target.dir ', target.dir)
    }

    message("Plotting traces")
    message("Plotting histograms")
    message("Plotting cross correlations")
    tryCatch(
        TraceHisto(stock, source.dir, target.dir, MCMCOptions, PlotOptions),
        error = efun, finally = NULL)
    message("Plotting running mean and cumulative quantiles")
    tryCatch(
        RunningAvgShort(stock, source.dir, target.dir, MCMCOptions, PlotOptions),
        error = efun, finally = NULL)
    
    for (i in 1:length(stock)) {
        message("Plotting CPUE posterior and fit")
        tryCatch(
            CPUE_posterior(stock[i], source.dir, target.dir, CPUEOptions, MCMCOptions, PlotOptions),
            error = efun, finally = NULL)
        message("Plotting catch rate posterior and fit")
        tryCatch(
            CR_posterior(stock[i], source.dir, target.dir, MCMCOptions, PlotOptions),
            error = efun, finally = NULL)
        message("Plotting biomass posteriors")
        tryCatch(
            Bio_posterior(stock[i], source.dir, target.dir, MCMCOptions, BioOptions, PlotOptions),
            error = efun, finally = NULL)
        message("Plotting recruitment posteriors")
        tryCatch(
            Rec_posterior(stock[i], source.dir, target.dir, MCMCOptions, PlotOptions),
            error = efun, finally = NULL)
        message("Plotting surplus production posteriors")
        tryCatch(
            SP_posterior(stock[i], source.dir, target.dir, MCMCOptions, PlotOptions),
            error = efun, finally = NULL)
        message("Plotting sex-ratio posteriors")
        tryCatch(
            SR_posterior(stock[i], source.dir, target.dir, PlotOptions),
            error = efun, finally = NULL)
        message("Plotting poo index")
        tryCatch(
            POO_posterior(stock, source.dir, target.dir, CROptions, PlotOptions),
            error = efun, finally = NULL)
    }
    
    message("Plotting maturity posteriors")
    tryCatch(
        Mature_posterior(stock, source.dir, target.dir, PlotOptions),
        error = efun, finally = NULL)
    message("Plotting selectivity posteriors")
    tryCatch(
        Select_posterior(stock, source.dir, target.dir, PlotOptions),
        error = efun, finally = NULL)
    message("Plotting cumulative density functions")
    tryCatch(
        CDF_posterior(stock, source.dir, target.dir, MCMCOptions, PlotOptions),
        error = efun, finally = NULL)
}
