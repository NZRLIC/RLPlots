#' MPD All
#' 
#' Function which does everything, includes all the individual functions
#'
#' @export
#' 
MPD.All <- function(stock, source.dir = ".", target.dir = source.dir)
{
    emsg <- "OOPS - EITHER YOU FUCKED UP OR THE CODE DID, the original error message is:"
    efun <- function(e) { message(emsg); message(e) }
    
    #if (!dir.exists(target.dir))
    #{
    #    dir.create(target.dir)
    #    message('created target.dir ', target.dir)
    #}
    
    message("Plotting catches")
    tryCatch(
        Catches(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting CPUE")
    tryCatch(
        CPUE(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting catch rates")
    tryCatch(
        CR(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting length-frequencies")
    tryCatch(
        LF(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting tag-recaptures")
    tryCatch(
        Tag(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting Erate")
    tryCatch(
        Erate(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting recruitment")
    tryCatch(
        Rect(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting LFzero")
    tryCatch(
        LFzero(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting Maturity")
    tryCatch(
        Mature(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting Selectivity")
    tryCatch(
        Select(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting Moult")
    tryCatch(
        Moult(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting mean size")
    tryCatch(
        MeanSize(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting biomass")
    tryCatch(
        Bio(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting SPBrect")
    tryCatch(
        SPBrect(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    #DDgrowth(stock,source.dir,target.dir)
    message("Plotting sex ratio")
    tryCatch(
        sexRatio(stock, source.dir, target.dir),
        error = efun, finally = NULL)
    message("Plotting poo index")
    tryCatch(
        POO(stock, source.dir, target.dir),
        error = efun, finally = NULL)
}
