#' PAB revision
#' 
#' I changed these two routines to accom start 1974, start 2 seasons in 1979
#' 
#' @export
#' 
PeriodToSeason<-function(period)
{
    season <- rep(NA, times = length(period))
    season[period<35] <- 1
    season[(period>=35) & (period/2) == ceiling(period/2)] <- 1
    season[(period>=35) & (period/2) != ceiling(period/2)] <- 2
    season[period>=35] <- mod(period[period >= 35] - 1, 2) + 1
    season[season==3] <- 1
    #season[period<6] <- 1
    #season[(period>=6) & (period/2)==ceiling(period/2)]<-2
    #season[(period>=6) & (period/2)!=ceiling(period/2)]<-1
    season
}
