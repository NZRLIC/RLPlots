#' Moving average
#'
#' @author D'Arcy Webber
#' @export
#' 
moving_average <- function(x, n = 11)
{
    stats::filter(x, rep(1/n, n), sides = 1)
}
