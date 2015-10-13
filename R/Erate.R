#' Exploitation rate plots
#'
#' Exploitation rate by year, season and SL/NSL
#'
#' @param stock the name of the stock (e.g. CRA1)
#' @param source.dir the directory containing the ADMB output files
#' @param target.dir the directory to save the plots to
#' @export
#' 
Erate <- function(stock, source.dir, target.dir = source.dir)
{
  erateall <- read.table(paste(source.dir, "/", stock, "Erate.out", sep = ""), header = TRUE, as.is = TRUE)
  erate <- erateall[erateall$Period <= PlotOptions$ModelEndPeriod,]

  #ifelse(PlotOptions$UsePeriod==TRUE,{
  #    erate$x <- erate$Period
  #  xlab<-"Period"},
  #  {erate$x<-PeriodToFishingYear(erate$Period)
  #  xlab <- "Fishing year"})

  # Format for ggplot
  names(erate) <- c("Period","Year","Season","SL","NSL")
  erate$Season <- c("AW","SS")[erate$Season]
  xx <- melt(erate, id.vars = c("Period","Year","Season"))

  PlotType(paste(target.dir, "/", stock, "Erate", sep = ""),
           width = 2*PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
  p <- ggplot(data = xx) +
             geom_line(aes(x = Year, y = value, group = c(Season), color = factor(Season)), size = 1.5) + 
             scale_colour_manual(values = PlotOptions$colourPalette) +
             facet_grid(. ~ variable) + xlab("Year") + ylab("Exploitation rate") +
             guides(color = guide_legend(title = "Season")) + 
             theme_lobview(PlotOptions)
  print(p)
  dev.off()
}  
