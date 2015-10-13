ggMCMC <- function(source.dir, target.dir = source.dir)
{
    require(reshape)
    require(ggmcmc)
    
  #flip-flop between data
  parameter <- read.table(paste(source.dir,"/parampost.out",sep=""), header = TRUE, as.is = TRUE)## AS edit ##[c(firstrow:lastrow),]
  indicators <- read.table(paste(source.dir,"/indicpost.out",sep=""), header = TRUE, as.is = TRUE)## AS edit ##[c(firstrow:lastrow),]
  data<-cbind(parameter,indicators) 
  
  dataMPD <- data[1,]
  attach(data)
  datalab<-c(paste(names(parameter)),paste(substring(names(indicators),first=3)))
  dimnames(data)<-list(dimnames(data)[[1]],datalab) 
  
  # delete constant columns
  loc.del <- c()
  for ( datacol in 1:ncol(data) )
  {
    if ( length(unique(data[[datacol]])) == 1 )
    {
      loc.del <- c(loc.del,datacol)
    }
  }
  datalab <- datalab[-loc.del]
  data <- data[,-loc.del]
  dataMPD <- dataMPD[-loc.del]
  Nsim <- nrow(data)
  Nplots <- ceiling(ncol(data) / MCMCOptions$n.post)
  dfm <- data.frame(melt(data), sample = 1:Nsim, chain = 1)
  dfm$mpd <- rep(as.numeric(dataMPD), each = Nsim)

    # Convert to a form that can be used by ggmcmc
    dat <- dfm[,c(3,4,1,2)]
    names(dat) <- c("Iteration","Chain","Parameter","value")
    head(dat)
    str(dat)
    attr(dat, "nChains") <- as.integer(2)
    attr(dat, "nParameters") <- as.integer(39)
    attr(dat, "nIterations") <- as.integer(233)
    attr(dat, "nBurnin") <- 1
    attr(dat, "nThin") <- 1
    attr(dat, "description") <- "s"
    attr(dat, "parallel") <- FALSE

    
    ggs_histogram(dat)
    ggs_traceplot(dat)
    ggs_crosscorrelation(dat)
    #f <- f + theme_presentation()
    #print(f)
    ggmcmc(dat, file = "/home/darcy/Documents/CRA/ggmcmc-output.pdf",
           plot = c("ggs_histogram","ggs_traceplot","ggs_running","ggs_autocorrelation"))
    ggmcmc(dat, file = "/home/darcy/Documents/CRA/ggmcmc-output.pdf",
           plot = "ggs_traceplot")
    ggmcmc(dat, file = "/home/darcy/Documents/CRA/ggmcmc-output.pdf",
           plot = "ggs_caterpillar")

}
