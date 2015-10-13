#' Running average function
#'
#' @export
#' 
RunningAvgShort <- function(stock, source.dir, target.dir = source.dir)
{
  parameter <- read.table(paste(source.dir, "/parampost.out", sep = ""), header = TRUE, as.is = TRUE)
  nam <- as.character(scan(paste(source.dir, "/parampost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
  colnames(parameter) <- nam
  indicators <- read.table(paste(source.dir, "/indicpost.out", sep = ""), header = TRUE, as.is = TRUE)
  nam <- as.character(scan(paste(source.dir, "/indicpost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
  colnames(indicators) <- nam
  data <- cbind(parameter, indicators)
    
  # create appropriate stock label
  if (length(stock) == 1) stock.label <- stock
  if (length(stock) == 2) stock.label <- paste(stock[1],substr(stock[2],4,4), sep = "")
  if (length(stock) == 3) stock.label <- paste(stock[1],substr(stock[2],4,4),substr(stock[3],4,4), sep = "")
  
  # delete constant columns
  loc.del <- c()
  for ( datacol in 1:ncol(data) )
  {
    if ( length(unique(data[[datacol]])) == 1 )
    {
      loc.del <- c(loc.del,datacol)
    }
  }
  data <- data[,-loc.del]
  nam <- names(data)
  
  Nsim <- nrow(data)
  Nplots <- ceiling(ncol(data) / MCMCOptions$n.post)

  # Calculate the moving average
  ma <- apply(data, 2, moving_average, n = MCMCOptions$running.mean)
  
  # Calculate the cumulative quantiles
  q5 <- data.frame(apply(data, 2, cumulative.quantile, probs = 0.05), sample = 1:Nsim)
  q50 <- data.frame(apply(data, 2, cumulative.quantile, probs = 0.5), sample = 1:Nsim)
  q95 <- data.frame(apply(data, 2, cumulative.quantile, probs = 0.95), sample = 1:Nsim)

  dfm <- melt(q50, id.vars = "sample")
  dfm <- cbind(dfm, q5 = melt(q5, id.vars = "sample")[,'value'])
  dfm <- cbind(dfm, q95 = melt(q95, id.vars = "sample")[,'value'])
  dfm <- cbind(dfm, ma = melt(ma, id.vars = "sample")[,'value'])
  dfm$variable <- factor(rep(nam, each = Nsim))
  
  #===============================================================================
  # Running average plots
  #===============================================================================
  if ( TRUE )
  {
      for ( pp in 1:Nplots )
      {
          dat <- na.omit(droplevels(dfm[((Nsim * MCMCOptions$n.post * (pp - 1)) + 1):(Nsim * MCMCOptions$n.post * pp),]))
          p <- ggplot(data = dat) +
                   geom_line(aes(x = sample, y = ma), colour = PlotOptions$colourPalette[2]) +
                   geom_line(aes(x = sample, y = value)) +
                   geom_line(aes(x = sample, y = q5)) +
                   geom_line(aes(x = sample, y = q95)) +
                   scale_colour_grey() +
                   facet_wrap( ~ variable, nrow = 6, ncol = 2, scales = "free_y", drop = TRUE) +
                   xlab("Sample") + ylab(NULL) + theme_lobview(PlotOptions)
          PlotType(paste(target.dir, "/", stock.label, "RunningAvg", pp, sep = ""),
                   width = 1.5*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
          print(p)
          dev.off()
      }
  }
}
