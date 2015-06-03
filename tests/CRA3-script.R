#=================================================================================

rm(list=ls())
library(lobview)

MPD.All(stock="CRA3",source.dir="CRA3")
MCMC.All(stock="CRA3",source.dir="CRA3")

# some MPE outputs
mpeview(pars=c("AvComm_med","avI_med","avBio_med"),stock="CRA3",source.dir="CRA3")

#=================================================================================

