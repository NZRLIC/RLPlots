#' Size Increment per moult
#'
#' Punt's Fig 7
#' 
#' @export
#' 
Moult <- function(stock, source.dir, target.dir = source.dir)
{
    nFiles <- 1 #scan(paste(source.dir,"/",stock,"Moult.out",sep=""), nlines = 1, quiet = TRUE)
    # currently the ADMB model will only output nFiles in the first line if there is >1 growth epoch
    # VH is going to fix this 25-09-2015
    moult <- list()
    if ( nFiles == 1 )
    {
        moult[[1]] <- read.table(paste(source.dir,"/",stock,"Moult.out",sep=""), header = TRUE, as.is = TRUE, skip = 1)
        moult[[1]] <- cbind(moult[[1]], file = 1)
        moult <- moult[[1]]
    }
    if ( nFiles == 2 )
    {
        find_break <- read.table(paste(source.dir,"/",stock,"Moult.out",sep=""), header = TRUE, as.is = TRUE, skip = 1)[,1]
        nBreak <- which(rowSums(sapply(letters, FUN = grepl, find_break)) != 0)
        moult[[1]] <- read.table(paste(source.dir,"/",stock,"Moult.out",sep=""), header = TRUE, as.is = TRUE, skip = 1, nrows = nBreak - 1)
        moult[[2]] <- read.table(paste(source.dir,"/",stock,"Moult.out",sep=""), header = TRUE, as.is = TRUE, skip = nBreak + 1)
        moult[[1]] <- cbind(moult[[1]], file = 1)
        moult[[2]] <- cbind(moult[[2]], file = 2)
        moult <- rbind(moult[[1]], moult[[2]])
    }
    nn <- names(moult)
    m1 <- data.frame(moult[,c(1,2,3,4,8)], sex = 1)
    m2 <- data.frame(moult[,c(1,5,6,7,8)], sex = 2)
    mm <- rbind(as.matrix(m1), as.matrix(m2))
    moult <- data.frame(mm)
    moult$sex[moult$sex == 1] <- "Male"
    moult$sex[moult$sex == 2] <- "Female"
    moult$file[moult$file == 1] <- "File 1"
    moult$file[moult$file == 2] <- "File 2"
    names(moult) <- c("size","low","increment","upp","file","sex")
    PlotType(paste(target.dir, "/", stock, "PredInc" , sep = ""),
             width = 2*PlotOptions$plotsize[1], height = 10+PlotOptions$plotsize[2])
    if(nFiles>1) facet_formula <- as.formula(file~sex) else facet_formula <- as.formula(.~sex)
    p <- ggplot() +
             geom_line(data = moult, aes(x = size, y = increment,color="Expectation"), size = 1.5) +
             geom_line(data = moult, aes(x = size, y = low,color="One s.d."), size = 0.5) +
             geom_line(data = moult, aes(x = size, y = upp,color="One s.d."), size = 0.5) +
             geom_hline(yintercept=0,linetype="longdash") +
             labs(x="Size (mm TW)",y="Increment (mm)",color="") +
             scale_x_continuous(breaks = seq(30, 90, 10)) +
             theme_lobview(PlotOptions) + facet_grid(facet_formula) +
             scale_colour_manual(values=c("Expectation"="black","One s.d."="black")) +
             scale_size_manual(values=c("Expectation"=1.5,"One s.d."=0.5),guide=FALSE) +
             guides(colour = guide_legend(override.aes = list(size = c(1.5,0.5))))
    if ( PlotOptions$Captions )
    {
        p <- p + ggtitle(paste(source.dir, " ", stock, ": Increment")) +
                     theme(plot.title = element_text(size = 9, vjust = 2.7))
    }
    print(p)
    dev.off()
}
