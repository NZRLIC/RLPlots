#' Merge posterior files and thin
#'
#' Does not work
#' 
#' @author Darcy Webber
#' @export
#' 
Merge_psv <- function(source.dir, file.names, target.dir = ".", nThin = 2)
{
    # Do a check
    if (target.dir %in% source.dir)
    {
        stop("Your target directory is the same as one of your source directories!\n")
    }

    system(paste("cp ", source.dir[1], "rl_mod_v4.psv", " ", target.dir, "/rl_mod_v4.psv", sep = ""))
    
    # How many chains are we merging?
    Nchain <- length(source.dir)
    d1 <- NULL

    # Stack the posterior chains on top of each other
    for (Chain in 1:Nchain)
    {
        din <- read.psv(paste(source.dir[Chain], "rl_mod_v4.psv", sep = ""), 1000)
        d1 <- rbind(d1, din)
    }

    # Thin the posterior chains
    n1 <- nrow(d1)
    s1 <- seq(1, n1, nThin)
    d1 <- d1[s1,]

    # Save the combined posterior chains to the target directory
    out <- vector("list", nrow(d1))
    for (i in 1:nrow(d1)) out[[i]] <- d1[i,]
    writeBin(out, con = paste(target.dir, "/rl_mod_v4.psv", sep = ""))
    #file(paste(target.dir, "/rl_mod_v4.psv", sep = ""), "rb")

}
