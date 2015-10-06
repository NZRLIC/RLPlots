#=================================================================================

rm(list=ls())
require(RLPlots)

#stock <- "CRA5"
#source.dir <- paste0(stock,"/base2/")

stock <- "CRA5"
source.dir <- stock

MPD.All(stock, source.dir)
MCMC.All(stock, source.dir)



stock <- "CRA1"
source.dir <- c("/home/darcy/Documents/CRA/2014/model/CRA1/mcmc/base2",
                "/home/darcy/Documents/CRA/2014/model/CRA1/mcmc/recce")
Multichain.All(stock, source.dir)


target.dir <- "/home/darcy/Documents/CRA/2014/model/CRA1/mchains/base2-merged"


stock <- "CRA1"
source.dir <- c("/home/darcy/Documents/CRA/2014/model/CRA1/stackedinformed-snail")
Snail(stock, source.dir)

source.dir <- c("/home/darcy/Documents/CRA/2014/model/CRA1/stackeduniform-snail")
Snail(stock, source.dir)


#=================================================================================

MCMCOptions <- .MCMCOptions; PlotOptions <- .PlotOptions; BioOptions <- .BioOptions
target.dir <- source.dir[1]
    TraceHisto(stock, source.dir, target.dir, MCMCOptions, PlotOptions)
    RunningAvgShort(stock, source.dir, target.dir, MCMCOptions, 
        PlotOptions)
    CPUE_posterior(stock, source.dir, target.dir, CPUEOptions, 
        MCMCOptions, PlotOptions)
    CR_posterior(stock, source.dir, target.dir, MCMCOptions, 
        PlotOptions)
    Bio_posterior(stock, source.dir, target.dir, MCMCOptions, 
        BioOptions, PlotOptions)
    Rec_posterior(stock, source.dir, target.dir, MCMCOptions, 
        PlotOptions)
    SP_posterior(stock, source.dir, target.dir, MCMCOptions, 
        PlotOptions)
    Mature_posterior(stock, source.dir, target.dir, PlotOptions)
    Select_posterior(stock, source.dir, target.dir, PlotOptions)
