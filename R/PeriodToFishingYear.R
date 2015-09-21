#' PAB changed this
#' 
#' @export
#' 
PeriodToFishingYear <- function(period)
{
    year <- rep(NA, times=length(period))
    year[period<=35] <- 1944+period[period<=35]; 
    year[period>35] <- 1961+ceiling((period[period>35])/2);
    #year[period<=6] <- 1973+period[period<=6];
    #year[period>6] <- 1976+ceiling((period[period>6])/2);
    year
}
