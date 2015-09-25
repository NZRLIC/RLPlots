#' MPD All
#' 
#' Function which does everything, includes all the individual functions
#'
#' @export
#' 
MPD.All <- function(stock, source.dir = ".", target.dir = paste('../', stock, 'plots', sep = ''), PlotOptions = .PlotOptions,
                    CPUEOptions = .CPUEOptions, CROptions = .CROptions,
                    LFOptions = .LFOptions, TagOptions = .TagOptions,
                    BioOptions = .BioOptions, SEXROptions = .SEXROptions)
{
    if (!dir.exists(target.dir)) {
        dir.create(target.dir)
		message('created target.dir ', target.dir)
    }
	
    message("Plotting catches")
    Catches(stock, source.dir, target.dir, PlotOptions)
    message("Plotting CPUE")
    CPUE(stock, source.dir, target.dir, CPUEOptions, PlotOptions)
    message("Plotting catch rates")
    CR(stock, source.dir, target.dir, CROptions, PlotOptions)
    # vh: comment out PRI plots cause crashes when no PRI output files
    # POO(stock,source.dir,target.dir, CROptions, PlotOptions)
    message("Plotting length-frequencies")
    LF(stock, source.dir, target.dir, LFOptions, PlotOptions)
    message("Plotting tag-recaptures")
    Tag(stock, source.dir, target.dir, TagOptions, PlotOptions)
    message("Plotting Erate")
    Erate(stock, source.dir, target.dir, PlotOptions)
    message("Plotting recruitment")
    Rect(stock, source.dir, target.dir, PlotOptions)
    message("Plotting LFzero")
    LFzero(stock, source.dir, target.dir, PlotOptions)
    message("Plotting Maturity")
    Mature(stock, source.dir, target.dir, PlotOptions)
    message("Plotting Selectivity")
    Select(stock, source.dir, target.dir, PlotOptions)
    message("Plotting Moult")
    Moult(stock, source.dir, target.dir, PlotOptions)
    message("Plotting mean size")
    MeanSize(stock, source.dir, target.dir, LFOptions, PlotOptions)
    message("Plotting biomass")
    Bio(stock, source.dir, target.dir, BioOptions, PlotOptions)
    message("Plotting SPBrect")
    SPBrect(stock, source.dir, target.dir, PlotOptions)
    #DDgrowth(stock,source.dir,target.dir, PlotOptions)
    message("Plotting sex ratio")
    sexRatio(stock, source.dir, target.dir, SEXROptions, PlotOptions)
}
