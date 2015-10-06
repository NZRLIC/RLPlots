#' Maturity posterior
#'
#' @author Darcy Webber
#' @export
#' 
Mature_posterior <- function(stock, source.dir, target.dir = source.dir,
                             PlotOptions = .PlotOptions)
{
  parameter <- read.table(paste(source.dir, "/parampost.out", sep = ""), header = TRUE, as.is = TRUE, row.names = NULL)
  nam <- as.character(scan(paste(source.dir, "/parampost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
  colnames(parameter) <- nam
  indicators <- read.table(paste(source.dir, "/indicpost.out", sep = ""), header = TRUE, as.is = TRUE, row.names = NULL)
  nam <- as.character(scan(paste(source.dir, "/indicpost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
  colnames(indicators) <- nam
  data <- cbind(parameter, indicators)
  Nsim <- nrow(data)
  
  # create appropriate stock label
  if (length(stock) == 1) stock.label <- stock
  if (length(stock) == 2) stock.label <- paste(stock[1],substr(stock[2],4,4), sep = "")
  if (length(stock) == 3) stock.label <- paste(stock[1],substr(stock[2],4,4),substr(stock[3],4,4), sep = "")
  
  # Identify the maturity posterior columns
  id <- c("mat50_","mat95add")
  data <- data[,id]
  dataMPD <- data[1,]
  dataMPD <- dataMPD[id]

  o.logistic <- function (x, a50, a95) 
  {
      return(1/(1 + 19^((a50 - x)/a95)))
  }

  Length <- seq(30, 90, by = 1)
  Maturity <- matrix(NA, nrow = Nsim, ncol = length(Length))
  colnames(Maturity) <- Length
  for (i in 1:Nsim) Maturity[i,] <- o.logistic(Length, data[i,1], data[i,2])
  Mat <- melt(Maturity)
  MatMPD <- o.logistic(Length, as.numeric(dataMPD[1]), as.numeric(dataMPD[2]))
  MatMPD <- data.frame(Length = Length, value = MatMPD)
  
          PlotType(paste(target.dir, "/", stock.label, "Maturity_posterior", sep = ""), PlotOptions,
                   width = PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
          p <- ggplot(data = Mat, aes(x = Var2, y = value)) +
              stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
              stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
              stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
              scale_x_continuous(breaks = seq(30, 90, 10)) +
              xlab("\nSize (mm TW)") + ylab("Maturity\n") + theme_lobview(PlotOptions) +
              geom_line(dat = MatMPD, aes(x = Length, y = value), linetype = "longdash", size = 1.5, colour = PlotOptions$colourPalette[2])
          print(p)
          dev.off()
  
}
