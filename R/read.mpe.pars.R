#' Read harvest control rule pars in MPE control file
#'
#' @export
#' 
read.mpe.pars <- function(source.dir = ".", control.file = "MPE.ctl")
{
  nrules <- scan(paste(source.dir, control.file, sep = ""), what = double(), skip = 24, nlines = 1, quiet = TRUE)
  control.pars <- data.frame(matrix(scan(paste(source.dir, control.file, sep = ""), what = double(), skip = 26, nlines = nrules, quiet = TRUE), ncol = 10, byrow = TRUE))
  return(control.pars)
}
