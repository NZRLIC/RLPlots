#' MPD All
#' 
#' Function which does everything, includes all the individual functions
#'
#' @export
#' 
MPD.All <- function(stock, source.dir, target.dir = source.dir, PlotOptions = .PlotOptions,
                    CPUEOptions = .CPUEOptions, CROptions = .CROptions,
                    LFOptions = .LFOptions, TagOptions = .TagOptions,
                    BioOptions = .BioOptions, SEXROptions = .SEXROptions)
{
    cat("Plotting catches\n")
    Catches(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting CPUE\n")
    CPUE(stock, source.dir, target.dir, CPUEOptions, PlotOptions)
    cat("Plotting catch rates\n")
    CR(stock, source.dir, target.dir, CROptions, PlotOptions)
    # vh: comment out PRI plots cause crashes when no PRI output files
    # POO(stock,source.dir,target.dir, CROptions, PlotOptions)
    cat("Plotting length-frequencies\n")
    LF(stock, source.dir, target.dir, LFOptions, PlotOptions)
    cat("Plotting tag-recaptures\n")
    Tag(stock, source.dir, target.dir, TagOptions, PlotOptions)
    cat("Plotting Erate\n")
    Erate(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting recruitment\n")
    Rect(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting LFzero\n")
    LFzero(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting Maturity\n")
    Mature(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting Selectivity\n")
    Select(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting Moult\n")
    Moult(stock, source.dir, target.dir, PlotOptions)
    cat("Plotting mean size\n")
    MeanSize(stock, source.dir, target.dir, LFOptions, PlotOptions)
    cat("Plotting biomass\n")
    Bio(stock, source.dir, target.dir, BioOptions, PlotOptions)
    cat("Plotting SPBrect\n")
    SPBrect(stock, source.dir, target.dir, PlotOptions)
    #DDgrowth(stock,source.dir,target.dir, PlotOptions)
    cat("Plotting sex ratio\n")
    sexRatio(stock, source.dir, target.dir, SEXROptions, PlotOptions)
}
