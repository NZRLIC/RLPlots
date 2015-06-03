#' Trajectories all
#' 
#' @export
#' 
Trajectories.All<-function(run, stock)
{
  BvulnArni(run,stock)
  ErateArni(run,stock)
  RdevArni(run,stock)
  SPArni(run,stock)
  CPUEArni(run,stock)
  CPUEresArni(run,stock)
#  need to edit the headers in CPUEpost.out before doing CRArni
#       CRArni(run,stock)  
#  need to edit the headers in  CPUErepost.out before doing CRresArni
#       CRresArni(run,stock)
#  PRIArni(run,stock)  
#  PRIresArni(run,stock)
#  LFArni(run,stock) something wrong here.....
  LFresArni(run,stock)
  QQpostLFresid(run,stock)
#  QQposttagresid()  
}
