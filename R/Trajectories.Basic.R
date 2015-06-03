#' Traj basic
#' 
#' @export
#' 
Trajectories.Basic<-function(run, stock)
{
  #Rewrite<-as.numeric(readline(prompt="If this is the first time you running this program in this folder type 1,\n otherwise 0:  "))
  #BioArni(run,stock)
  #ErateArni(run,stock)
  RdevArni(run,stock)
  SPArni(run,stock)
  CPUEArni(run,stock)
#  CRArni(run,stock)  
#  PRIArni(run,stock)  
  LFArni(run,stock)
}
