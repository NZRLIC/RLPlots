#' Moving average
#'
#' @author Darcy Webber
#' @export
#' 
moving.average <- function(x, n = 11)
{
    filter(x, rep(1/n, n), sides = 1)
}
