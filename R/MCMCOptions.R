#' MCMC options
#'
#' @author Darcy Webber, Charlers Edwards
#' @param n.post number of plots in each window for histograms, trace plots and
#' running average/quantile plots
#' @param begin.newplot the column number where you want new plot
#' @param Rewrite use 1 to re-calculate running mean for diagnostic otherwise 0
#' @param running.mean the n for calculated running means in trace and running
#' average/quantile plots
#' @export
#' 
.MCMCOptions <- list(n.post = 12, begin.newplot = c(1,39), Rewrite = 1,
                     running.mean = 50)
