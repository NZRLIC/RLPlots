#' Mean size (tail width, mm) over time
#'
#' @param source.dir the directory where the outputs for a run are sitting
#' @param target.dir the directory where the outputs should be saved
#' @param stock the name of the stock (e.g. CRA1)
#' @export
#' 
MeanSize <- function(stock, source.dir, target.dir = source.dir)
{
    # Read file
    dat <- read.table(paste(source.dir, "/", stock, "FrancisLFs.out", sep = ""), header = TRUE, as.is = TRUE) 
    # Names = sample tau period type Season sex size Obs Pred StdRes  LL 
    maxtype <- max(dat$Type)
    sexcode <- LFOptions$sexcode
    
    # label factors
    dat$Sex[dat$Sex==1] <- "Males"
    dat$Sex[dat$Sex==2] <- "Immature females"
    dat$Sex[dat$Sex==3] <- "Mature females"
    dat$Season[dat$Season==1] <- "AW"
    dat$Season[dat$Season==2] <- "SS"
    dat$Type[dat$Type==1] <- "CS"
    dat$Type[dat$Type==2] <- "LB"
    
    # re-order factors
    dat$Sex <- factor(dat$Sex, levels = c("Males","Immature females","Mature females"))
    
    # ==============================================================================
    # Plot obs vs pred
    # ==============================================================================
    if ( LFOptions$ObsPred )
    {
        PlotType(paste(target.dir, "/", stock, "MeanSize" , sep = ""),
                 width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
        p <- ggplot(dat) + 
            geom_point(aes(x = Year, y = Obs, color = Type), size = 3) + 
            geom_line(aes(x = Year, y = Pred, color = "Pred."), size = 1.1) + 
            geom_line(aes(x = Year, y = Pred + Wt, color = "One s.d."), size = 0.5) + 
            geom_line(aes(x = Year, y = Pred - Wt, color = "One s.d."), size = 0.5) +
            facet_grid(Season ~ Sex) + 
            labs(y = "Mean width (mm)\n", x = "\nYear", color = "") +
            theme_lobview(PlotOptions) +
            scale_colour_manual(values = c("Pred." = "black", "One s.d." = "black", "CS" = cbPalette1[1], "LB" = cbPalette1[2])) +
            #scale_size_manual(values = c("Pred." = 1.5, "One s.d." = 0.5), guide = FALSE) +
            guides(color = guide_legend(override.aes = list(size = c(1.5,0.5,3,3), shape = c(NA,NA,16,16), linetype = c(1,1,0,0)), reverse = TRUE))
        if ( PlotOptions$Captions )
        {
            p <- p + ggtitle(paste(source.dir, " ", stock, ": size")) +
                theme(plot.title = element_text(size = 9, vjust = 2.7))
        }
        print(p)
        dev.off()
    }
}
