#' Read psv file
#'
#' @author Darcy Webber
#' @export
#' 
read.psv <- function(fn, nsamples = 1000)
{
    #This function reads the binary output from ADMB
    #-mcsave command line option.
    #fn = paste(ifile,'.psv',sep='')
    filen <- file(fn, "rb")
    nopar <- readBin(filen, what = integer(), n = 1)
    mcmc <- readBin(filen, what = numeric(), n = nopar * nsamples)
    mcmc <- matrix(mcmc, byrow = TRUE, ncol = nopar)
    close(filen)
    return(mcmc)
}
