#' Merge posterior files and thin
#' 
#' @author Darcy Webber
#' @export
#' 
Merge_posterior <- function(source.dir, target.dir = ".", nThin = 2)
{
    # Do a check
    if (target.dir %in% source.dir)
    {
        stop("Your target directory is the same as one of your source directories!\n")
    }
    
    # How many chains are we merging?
    Nchain <- length(source.dir)
    d1 <- NULL
    d2 <- NULL

    # Stack the posterior chains on top of each other
    for (Chain in 1:Nchain)
    {
        parameter <- read.table(paste(source.dir[Chain], "/parampost.out", sep = ""), header = TRUE, as.is = TRUE)
        nam <- as.character(scan(paste(source.dir[Chain], "/parampost.out", sep = ""), nlines = 1, what = "character", quiet = TRUE))
        Nsim <- nrow(parameter)
        #parameter <- data.frame(parameter, as.factor(Chain), 1:Nsim)
        #names(parameter) <- c(nam, "Chain", "sample")
        #parameter <- data.frame(parameter, as.factor(Chain), 1:Nsim)
        names(parameter) <- nam
        d1 <- rbind(d1, parameter)
        
        indicators <- read.table(paste(source.dir[Chain], "/indicpost.out", sep = ""), header = TRUE, as.is = TRUE)
        nam <- as.character(scan(paste(source.dir[Chain], "/indicpost.out", sep = ""), nlines = 1, what = "character", quiet = TRUE))
        Nsim <- nrow(indicators)
        #indicators <- data.frame(indicators, as.factor(Chain), 1:Nsim)
        #names(indicators) <- c(nam, "Chain", "sample")
        #indicators <- data.frame(indicators, as.factor(Chain), 1:Nsim)
        names(indicators) <- nam
        d2 <- rbind(d2, indicators)
    }

    # Thin the posterior chains
    n1 <- nrow(d1)
    s1 <- seq(1, n1, nThin)
    d1 <- d1[s1,]

    n2 <- nrow(d2)
    s2 <- seq(1, n2, nThin)
    d2 <- d2[s2,]

    # Save the combined posterior chains to the target directory
    write.table(d1, file = paste(target.dir, "/parampost.out", sep = ""), sep = " ", quote = FALSE, row.names = FALSE)
    write.table(d2, file = paste(target.dir, "/indicpost.out", sep = ""), sep = " ", quote = FALSE, row.names = FALSE)
    
}
