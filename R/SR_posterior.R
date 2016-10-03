#' Sex ratio fit plots
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @export
#' 
SR_posterior <- function(stock, source.dir, target.dir = source.dir)
{
    # Load observations if the file exists in the source directory    
    filename <- paste(source.dir, "/", stock, "SexRatio.out", sep = "")
    if ( file.exists(filename) )
    {
        sexr <- read.table(filename, header = TRUE, as.is = TRUE)
        sexr$Sex <- factor(c("Male","Immature female","Mature female")[sexr$sex])
    } else {
        stop(paste("can't find:", filename, "\n"))
    }

    if ( PlotOptions$UsePeriod )
    {
        sexr$x <- sexr$period
        xlab <- "\nPeriod"     
    } else {
        sexr$x <- PeriodToFishingYear(sexr$period)
        xlab <- "\nFishing year"
    }

    SD <- sqrt(sexr$Pred*(1-sexr$Pred)/sexr$effN)
    sexr$LB <- sexr$Obs - 1.96*SD
    sexr$UB <- sexr$Obs + 1.96*SD
    
    d <- sexr[,c('Year','Obs','Season','sex','type','LB','UB')]
    d <- melt(d, id.vars = list('Year','Season','sex','type'), as.is = list('Obs','LB','UB'))
    d$type[d$type == 1] <- "LB"
    d$type[d$type == 2] <- "CS"
    d$Season[d$Season == 1] <- "AW"
    d$Season[d$Season == 2] <- "SS"
    d$sex[d$sex == 1] <- "Male"
    d$sex[d$sex == 2] <- "Imm. Female"
    d$sex[d$sex == 3] <- "Mat. Female"
    d$LB <- d$value - 1.96*SD
    d$UB <- d$value + 1.96*SD
    
    # Load posterior
    dat <- as.matrix(read.table(paste(source.dir, "/", stock, "SexRatiopost.out", sep = ""), header = TRUE, as.is = TRUE))
    dat <- t(dat)
    colnames(dat) <- 1:ncol(dat)
    dat <- data.frame(dat)
    dat$sex <- sexr$sex
    dat$type <- sexr$type
    dat$year <- sexr$Year
    dat$Season <- sexr$Season
    dat <- melt(dat, id.vars = c("sex","type","year","Season"))
    dat$type[dat$type == 1] <- "CS"
    dat$type[dat$type == 2] <- "LB"
    dat$sex[dat$sex == 1] <- "Male"
    dat$sex[dat$sex == 2] <- "Imm. Female"
    dat$sex[dat$sex == 3] <- "Mat. Female"
    dat$Season[dat$Season == 1] <- "AW"
    dat$Season[dat$Season == 2] <- "SS"
    dat$sex <- factor(dat$sex, levels = c('Male','Imm. Female','Mat. Female'))
    
    # Plot obs v pred
    p <- ggplot(data = dat, aes(x = year, y = value)) +
        stat_summary(data = dat, fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
        stat_summary(data = dat, fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        geom_pointrange(data = subset(d, d$variable == 'Obs'), aes(x = Year, y = value, ymin = LB, ymax = UB)) +
        scale_colour_manual(values = PlotOptions$colourPalette) +
        facet_grid(Season + type  ~ sex, scales = "fixed") +
        xlab(xlab) + ylab("Proportion\n") + #expand_limits(y = c(0,1)) +
        theme_lobview(PlotOptions)
    
    if ( PlotOptions$Captions )
    {
        p <- p + ggtitle(paste(source.dir, " ", stock, ": Selectivity curve by Epoch by sex with 0.05 and 0.95 percentiles")) +
            theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
    
    #PlotType(paste(target.dir, "/", stock, "SR_posterior", sep = ""), PlotOptions, width = 350, height = 250)
    PlotType(paste(target.dir, "/", stock, "SR_posterior", sep = ""), width = 2*PlotOptions$plotsize[1], height = 1.5*PlotOptions$plotsize[2])
    print(p)
    dev.off()
}
