#' Biomass plot options
#'
#' List of options to be passed to biomass plotting functions. This list is loaded with \pkg{RLPlots} and
#' is called by the functions \code{\link{Bio}}, \code{\link{Bio_posterior}}, \code{\link{MPD.All}} 
#' and \code{\link{MCMC.All}}. It contains a list of logical values that can be modified by the user 
#' and passed to the plotting function. Elements in the list include the following logical values:
#' \describe{
#'   \item{\code{VulnB = TRUE}}{do the vulnerable biomass plot}
#'   \item{\code{TotalB = TRUE}}{do the total biomass plot}
#'   \item{\code{RecB = TRUE}}{do the recruited biomass plot}
#'   \item{\code{TotalRecB = TRUE}}{do the x plot}
#' }
#' 
#' @examples
#' \dontrun{
#' Bio("CRA5", source.dir = ".", BioOptions = list(VulnB = TRUE, 
#'                                                  TotalB = TRUE, 
#'                                                  RecB = TRUE, 
#'                                                  TotalRecB = TRUE))
#' }
BioOptions <- list(VulnB = TRUE, TotalB = TRUE, RecB = TRUE, TotalRecB = TRUE)
.BioOptions <- BioOptions