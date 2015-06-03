#' Selectivity
#'
#' @param run the directory where the outputs for a run are sitting
#' @param stock the name of the stock (e.g. CRA1)
#' @param PlotOptions
#' @export
#' 
Select <- function(stock, source.dir, target.dir = source.dir, PlotOptions = .PlotOptions)
{
    select <- read.table(paste(source.dir, "/", stock, "Select.out", sep = ""), header = TRUE, as.is = TRUE)
    select$sex[select$sex == 1] <- "Male"
    select$sex[select$sex == 2] <- "Female"
    select$sex <- factor(select$sex, levels = c("Male","Female"))

    # Do the plot
    PlotType(paste(target.dir, "/", stock, "Select" , sep = ""), PlotOptions,
             width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
    p <- ggplot() +
             geom_line(data = select, aes(x = size, y = Selectivity, group = Epoch, color = factor(Epoch)), size = 1.5) + 
             scale_colour_manual(values = PlotOptions$colourPalette) +
             facet_grid(. ~ sex) + xlab("\nSize (mm TW)") + ylab("Selectivity\n") +
             scale_x_continuous(breaks = seq(30, 90, 10)) +
             guides(color = guide_legend(title = "Epoch")) + 
             theme_lobview(PlotOptions)
    if ( PlotOptions$Captions )
    {
        p <- p + ggtitle(paste(source.dir, " ", stock, ": Selectivity curve by Epoch by sex")) +
                     theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
    print(p)
    dev.off()
    
}
