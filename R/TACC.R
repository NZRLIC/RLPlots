#' Calculate the TACC given a vector of CPUE
#'
#' @export
#' 
TACC <- function(par, cpue)
{
    tacc <- rep(NA, length(cpue))
    if (par[1] == 3) # Plateau rule with slope
    {
        #tacc <- ifelse(cpue < par[3], par[2]*(cpue - par[5])/(par[3] - par[5]) , ifelse(cpue < par[4], par[2], par[2]*(1+0.5*(cpue - par[4]) / (par[6] - par[4]))))
        tacc[cpue <= par[2]] <- 0
        tacc[cpue > par[2] & cpue <= par[3]] <- par[5] * ((cpue[cpue > par[2] & cpue <= par[3]] - par[2]) / (par[3] - par[2]))
        tacc[cpue > par[3] & cpue <= par[4]] <- par[5]
        tacc[cpue > par[4]] <- (cpue[cpue > par[4]] - par[4]) * 0.5 * par[5] / (par[6] - par[4]) + par[5]
    }
    if (par[1] == 4) # Plateau rule with step function
    {
            #if (par[4] >= par[8])
            #    stop("par4 < par8 condition not met.")
        tacc[cpue <= par[2]] <- 0
        tacc[cpue > par[2] & cpue <= par[3]] <- par[5] * ((cpue[cpue > par[2] & cpue <= par[3]] - par[2]) / (par[3] - par[2]))
        tacc[cpue > par[3] & cpue <= par[4]] <- par[5]
        tacc[cpue > par[4]] <- par[5] * ( (1 + par[7])^as.integer(((cpue[cpue > par[4]] - par[4])/par[6]) + 1) )
    }
    return(tacc)
}
