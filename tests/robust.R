require(RLPlots)

source.dir <- "CRA5/"
#source.dir <- "CRA1/mchains/base2-merged"
stock <- "CRA5"
dat <- read.table(paste(source.dir, "/parampost.out", sep = ""), header = TRUE, as.is = TRUE)
lnR0 <- dat$lnR0
sigmaR <- dat$sigmaR
dat <- as.matrix(read.table(paste(source.dir, "/", stock, "Rdev.out", sep = ""), header = TRUE, as.is = TRUE))
colnames(dat) <- as.character(scan(paste(source.dir, "/", stock, "Rdev.out", sep = ""), nlines = 1, quiet = TRUE))
names(dimnames(dat)) <- c("iter", "year")
dat <- melt(dat, value.name = "Rdev")
dat <- data.frame(dat, lnR0 = lnR0, sigmaR = sigmaR)
dat <- subset(dat, year <= PeriodToFishingYear(PlotOptions$ModelEndPeriod))
dat <- data.frame(dat, R = exp(dat$lnR0 + dat$Rdev - 0.5*dat$sigmaR^2))

#==================================================================================================

da <- aggregate(dat$R, by = list(dat$year), FUN = median)
d2 <- moving_average(da$x, n = 10)
min(d2/d2[which(da$Group.1 == 2011)]-1, na.rm = TRUE)

da$ma <- as.numeric(moving_average(da$x, n = 10))
da$diff <- d2/d2[which(da$Group.1 == 2011)]-1
names(da) <- c("Year","Recruitment","Moving average","Difference")
da$Recruitment <- da$Recruitment/1e+06
da$'Moving average' <- da$'Moving average'/1e+06
d2 <- melt(da, id.vars = "Year")
names(d2) <- c("Year","Variable","value")

p <- ggplot(data = d2, aes(x = Year, y = value, group = Variable, colour = Variable)) +
    geom_line(size = 1) +
    theme_lobview(PlotOptions) +
    scale_color_manual(values = cbPalette2) +
    labs(x = "\nYear", y = "Proportion difference       Numbers recruited (millions)\n")

    PlotType("Recruitment-robustness",
             width = PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
    print(p)
    dev.off()

#==================================================================================================

da <- aggregate(dat$Rdev, by = list(dat$year), FUN = median)
d2 <- moving_average(da$x, n = 10)
min(d2, na.rm = TRUE)

da$ma <- as.numeric(moving_average(da$x, n = 10))
names(da) <- c("Year","Rdev","Moving average")
da$Recruitment <- da$Recruitment
da$'Moving average' <- da$'Moving average'
d2 <- melt(da, id.vars = "Year")
names(d2) <- c("Year","Variable","value")

p <- ggplot(data = d2, aes(x = Year, y = value, group = Variable, colour = Variable)) +
    geom_line(size = 1) +
    theme_lobview(PlotOptions) +
    scale_color_manual(values = cbPalette2) +
    labs(x = "\nYear", y = "Recruitment deviation\n")

    PlotType("Recruitment-robustness2",
             width = PlotOptions$plotsize[1], height = PlotOptions$plotsize[2])
    print(p)
    dev.off()
