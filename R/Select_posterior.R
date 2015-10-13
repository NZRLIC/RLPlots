#' Selectivity posterior
#'
#' @author Darcy Webber
#' @export
#' 
Select_posterior <- function(stock, source.dir, target.dir = source.dir)
{
    parameter <- read.table(paste(source.dir, "/parampost.out", sep = ""), header = TRUE, as.is = TRUE)
    nam <- as.character(scan(paste(source.dir, "/parampost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
    colnames(parameter) <- nam
    indicators <- read.table(paste(source.dir, "/indicpost.out", sep = ""), header = TRUE, as.is = TRUE)
    nam <- as.character(scan(paste(source.dir, "/indicpost.out", sep = ""), nlines = 1, what = 'character', quiet = TRUE))
    colnames(indicators) <- nam
    data <- cbind(parameter, indicators)
    Nsim <- nrow(data)
    
    o.double.normal <- function (x, a1, sL, sR) 
    {
        fx <- rep(NA, length(x))
        fx[x <= a1] <- 2^-((x[x <= a1] - a1)/sL)^2
        fx[x > a1] <- 2^-((x[x > a1] - a1)/sR)^2
        return(fx)
    }
    
    if (length(stock) == 1)
    {    
        Length <- seq(31, 91, by = 1)
        Selectivity <- matrix(NA, nrow = Nsim, ncol = length(Length))
        colnames(Selectivity) <- Length
        
        for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,"SelectMax1M"], data[i,"VL1M"], data[i,"VR1M"])
        Sel1M <- data.frame(melt(Selectivity), sex = "Male", epoch = "1")
        for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,"SelectMax1F"], data[i,"VL1F"], data[i,"VR1F"])
        Sel1F <- data.frame(melt(Selectivity), sex = "Female", epoch = "1")
        for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,"SelectMax2M"], data[i,"VL2M"], data[i,"VR2M"])
        Sel2M <- data.frame(melt(Selectivity), sex = "Male", epoch = "2")
        for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,"SelectMax2F"], data[i,"VL2F"], data[i,"VR2F"])
        Sel2F <- data.frame(melt(Selectivity), sex = "Female", epoch = "2")
        sel <- rbind(Sel1M, Sel1F, Sel2M, Sel2F)
        
        p <- ggplot(sel, aes(x = Var2, y = value, col = epoch, fill = epoch)) +
            stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
            scale_colour_manual(values = PlotOptions$colourPalette) +
            scale_fill_manual(values = PlotOptions$colourPalette) +
            scale_x_continuous(breaks = seq(30, 90, 10)) +
            facet_grid(. ~ sex) +
            theme_lobview(PlotOptions) + labs(x = "\nSize (mm TW)", y = "Selectivity\n", fill = "Epoch", col = "Epoch")
        if ( PlotOptions$Captions )
        {
            p <- p + ggtitle(paste(source.dir, " ", stock, ": Selectivity curve by Epoch by sex")) +
                theme(plot.title = element_text(size = 9, vjust = 2.7))
        }
        
        PlotType(paste(target.dir, "/", stock, "Selectivity_posterior", sep = ""),
                 width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
        print(p)
        dev.off()
    } else {
        for (stk in 1:length(stock))
        {
            Length <- seq(31, 91, by = 1)
            Selectivity <- matrix(NA, nrow = Nsim, ncol = length(Length))
            colnames(Selectivity) <- Length
            
            for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,paste("SelectMax1M",stk,sep="")], data[i,paste("VL1M",stk,sep="")], data[i,paste("VR1M",stk,sep="")])
            Sel1M <- data.frame(melt(Selectivity), sex = "Male", epoch = "1")
            for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,paste("SelectMax1F",stk,sep="")], data[i,paste("VL1F",stk,sep="")], data[i,paste("VR1F",stk,sep="")])
            Sel1F <- data.frame(melt(Selectivity), sex = "Female", epoch = "1")
            for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,paste("SelectMax2M",stk,sep="")], data[i,paste("VL2M",stk,sep="")], data[i,paste("VR2M",stk,sep="")])
            Sel2M <- data.frame(melt(Selectivity), sex = "Male", epoch = "2")
            for (i in 1:Nsim) Selectivity[i,] <- o.double.normal(Length, data[i,paste("SelectMax2F",stk,sep="")], data[i,paste("VL2F",stk,sep="")], data[i,paste("VR2F",stk,sep="")])
            Sel2F <- data.frame(melt(Selectivity), sex = "Female", epoch = "2")
            sel <- rbind(Sel1M, Sel1F, Sel2M, Sel2F)
            
            p <- ggplot(sel, aes(x = Var2, y = value, col = epoch, fill = epoch)) +
                stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
                stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
                stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
                scale_colour_manual(values = PlotOptions$colourPalette) +
                scale_fill_manual(values = PlotOptions$colourPalette) +
                scale_x_continuous(breaks = seq(30, 90, 10)) +
                facet_grid(. ~ sex) +
                theme_lobview(PlotOptions) +
                labs(x = "\nSize (mm TW)", y = "Selectivity\n", fill = "Epoch", col = "Epoch")
            if ( PlotOptions$Captions )
            {
                p <- p + ggtitle(paste(source.dir, " ", stock[stk], ": Selectivity curve by Epoch by sex")) +
                    theme(plot.title = element_text(size = 9, vjust = 2.7))
            }
            
            PlotType(paste(target.dir, "/", stock[stk], "Selectivity_posterior", sep = ""),
                     width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
            print(p)
            dev.off()
        }
    }
}
