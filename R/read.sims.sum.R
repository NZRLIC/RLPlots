#' read MPE sims.sum output
#'
#' @export
#' 
read.sims.sum <- function(source.dir = ".", model.name = "base")
{
  dat <- read.table(paste(source.dir, "Sims.sum", sep = ""))
  dat <- data.frame(model.name, dat)
  colnames(dat) <- c("model","ctl1","ctl2","ctl3","ctl4","ctl5","ctl6","ctl7","ctl8","ctl9","ctl10","years?","par1","par2","par3","par4","par5","par6","par7","par8","par9","par10","region","5%","avBio_med","95%","5%","TermBio_med","95%","5%","minComm_med","95%","AvComm_low","AvComm_med","95%","5%","5yrC%_med","95%","5%","minRec_med","95%","5%","avRec_med","95%","5%","minl_med","95%","5%","avI_med","95%","5%","AAV_med","95%","5%","avB/Bmsy_med","95%","P<Bref","P<Bmin","P<Bmsy","nchanges","P<SSB20%","P<SSB10%","P<0.5*Bref","P<0.25*Bref","P(<plateaux)","P(>plateaux)","P(AWCPUE>1.14)","P(AWCPUE>1.50)","P(AWCPUE>1.80)","AvPCPUE_low","AvPCPUE_med","AvPCPUE_upp","avPCPUE_low","minPCPUE_med","minPCPUE_upp","Btot_projyr_low","Btot_projyr_med","Btot_projyr_upp","Btot(proj_yr)/Btot0_low","Btot(proj_yr)/Btot0_med","Btot(proj_yr)/Btot0_upp","Ntot_projyr_low]","Ntot_projyr_med","Ntot_projyr_upp","Ntot(proj_yr)/Ntot0_low","Ntot(proj_yr)/Ntot0_med","Ntot(proj_yr)/Ntot0_upp")
  dat$rule <- 1:nrow(dat)
  return(dat)
}
