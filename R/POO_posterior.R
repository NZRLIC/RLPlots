#' POO posterior plot
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @export
#' 
POO_posterior <- function(stock, source.dir, target.dir = source.dir)
{
    po <- read.table(paste(source.dir,"/",stock,"PooResids.out", sep = ""), header = TRUE, as.is = TRUE)
    
    if (nrow(po) == 0) stop(paste(source.dir,"/",stock,"PooResids.out is empty\n", sep = ""))
    
    if ( CROptions$IsAnnual )
    {
        po$x <- po$Year
        xlab <- "Fishing year"
    } else {
        po$x <- po$Period
        xlab <- "Period"
    } 
    po$LB <- exp(log(po$Obs) - po$Sigma)
    po$UB <- exp(log(po$Obs) + po$Sigma)

    # Read posterior file
    post <- read.table(paste(source.dir,"/",stock,"Poopost.out", sep = ""), header = TRUE, as.is = TRUE)
    post <- data.frame(t(post))
    post$Year <- po$Year
    post <- melt(post, id.vars = "Year")

    # Plot
    p <- ggplot(data = post, aes(x = Year, y = value)) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        geom_pointrange(data = po, aes(x = Year, y = Obs, ymin = LB, ymax = UB)) +
        theme_lobview(PlotOptions) +
        labs(x = "\nFishing year", y = "Puerulus index\n")
    if ( PlotOptions$Captions )
    {
        p <- p + ggtitle(paste(source.dir, " ", stock, ": Observed and predicted for POO fits. Points: Observed; Lines: Predicted")) +
            theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
    
    PlotType(paste(target.dir, "/", stock, "Poo_posterior", sep = ""),
                 width = 2*PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
    print(p)
    dev.off()
}
