#' Calculate a cumulative quantile
#'
#' @author Darcy Webber
#' @export
#' 
cumulative.quantile <- function(x, probs = 0.5)
{
    cquant <- rep(NA, length(x))
    for (i in 2:length(x))
    {
        cquant[i] <- quantile(x[1:i], probs = probs)
    }
    return(cquant)
}
