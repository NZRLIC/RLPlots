#' MOD function
#'
#' @export
#' 
mod <- function(x, d)
{
    ( (x / d) - floor(x / d) ) * d
}
