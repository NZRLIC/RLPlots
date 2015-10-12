#' read MPE sims.sum output
#'
#' @export
#' 
read.sims.sum <- function(stock, source.dir = "./", model.name = "base")
{
    tryCatch(
        dat <- read.table(paste(source.dir, "Sims.sum", sep = "")), 
        finally = "check there are no -1.#IND in Sims.sum, else replace with NA\n"
    )
    
    if (length(stock == 1)) { 
        rule  <- 1:nrow(dat)
        stock <- rep(stock, times = nrow(dat))
    } else {
        rule  <- rep(1:(nrow(dat) / length(stock)), time = length(stock))
        stock <- rep(stock, each = nrow(dat) / length(stock))
    }
        
    dat <- data.frame(stock, model.name, rule, dat)
    
    colnames(dat) <- c("stock", "model", "rule", "ctl1","ctl2","ctl3","ctl4","ctl5","ctl6","ctl7","ctl8","ctl9","ctl10","Male AW MLS","par1","par2","par3","par4","par5","par6","par7","par8","par9","par10","region","avBio_low","avBio_med","avBio_upp","TermBio_low","TermBio_med","TermBio_upp","minComm_low","minComm_med","minComm_upp","AvComm_low","AvComm_med","AvComm_upp","av5yrCatch_low","av5yrCatch_med","av5yrCatch_upp","minRec_low","minRec_med","minRec_upp","avRec_low","avRec_med","avRec_upp","minl_low","minl_med","minl_upp","avI_low","avI_med","avI_upp","AAV_low","AAV_med","AAV_upp","avB/Bmsy_low","avB/Bmsy_med","avB/Bmsy_upp","P<Bref","P<Bmin","P<Bmsy","nchanges","P<SSB20%","P<SSB10%","P<0.5*Bref","P<0.25*Bref","P(<plateaux)","P(>plateaux)","P(AWCPUE>1.14)","P(AWCPUE>1.50)","P(AWCPUE>1.80)","AvPCPUE_low","AvPCPUE_med","AvPCPUE_upp","avPCPUE_low","minPCPUE_med","minPCPUE_upp","Btot_projyr_low","Btot_projyr_med","Btot_projyr_upp","Btot(proj_yr)/Btot0_low","Btot(proj_yr)/Btot0_med","Btot(proj_yr)/Btot0_upp","Ntot_projyr_low]","Ntot_projyr_med","Ntot_projyr_upp","Ntot(proj_yr)/Ntot0_low","Ntot(proj_yr)/Ntot0_med","Ntot(proj_yr)/Ntot0_upp")

    return(dat)
}
