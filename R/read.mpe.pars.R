#' Read harvest control rule pars in MPE control file
#'
#' @export
#' 
read.mpe.pars <- function(stock, source.dir = "./", control.file = "MPE.ctl")
{
  nrules <- scan(paste(source.dir, control.file, sep = ""), what = double(), skip = 32, nlines = 1, quiet = TRUE)
  pars <- data.frame(matrix(scan(paste(source.dir, control.file, sep = ""), what = double(), skip = 34, nlines = nrules, quiet = TRUE), ncol = 10 * length(stock), byrow = TRUE))
  
  if (length(stock) == 2) {
      control.pars <- list()
      control.pars[[stock[1]]] <- pars[, 1:10]
      control.pars[[stock[2]]] <- pars[,11:20]
  } else control.pars <- pars
  
  return(control.pars)
}
