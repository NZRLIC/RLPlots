#*********************************************************************
#   THK changed some of teh figures October2010           
#Breen changes 23 Septembre 2010 from "viewer2008_28Sept09.r"
#removed Tag2 and commented out references to it; renamed Tag1 Tag and fixed reference to outfile
#removed refs to Moult2, renamed Moult1 Moult and fixed reference to outfile
#note that the tagfile should be called "CRA5"
#removed a hardwired ref to CRA3 and replaced with "stock"
#all the MPD functions workin
#renamed "Viewer2010_v1.r"
#turned on the std resids for LF, made it write "LFResid1.wmf" instead of "LFResid.wmf1"
#renamed Viewer2010_v101.r"
#had to rework the period to year and season functions to accommodate the 2010 model: running from 1945,
#   changing from yearly to seasonal time step in 1979; minor I/O changes
#hardwired the upper bound of poo plot to 6 to accommodate Kaikoura data, now v102
#*********************************************************************
# Susan Kim, Adam Smith, Vivian Haist, Terese Kendrock, Paul Breen and Marine Pomarede have worked on this
#*********************************************************************
#2011: MP improved the printing of names in McMC results, rewrote the ERate plots for us
#   to get rid of added Xs in the plots one has to edit the parampost and indicpost files to remove leading digits
#   PAB added a routine RectArni that reads a file called stockRect.out, and he made that file by hand from Rdevs and Ros
#Version Viewer2006a - in development                                                           
#date: 26 April 2006                                                                            
#A S/S-plus/R script for veiwing and analysing the outputs of the NZ rock lobster model         
#This goes with the new RL multi-area model.                                                    
#                                                                                               
#Use: to view the results for a particular run you can use commands like MPD.All("001","stock") or Catches("001","stock")
#Each function has a set of options for what graphs are to be viewed or analyses generated      
#These options are contained in a "helper" list called FunctionOptions                          
#You can set which plots get produced by changing options.  For example to turn off the CR residual plots you would set CROptions.Resid = FALSE
#Note that each call of a function creates a new graphics window.  These take up memory and you should not let too many pile up.  Use graphics.off() to close all graphics windows
#
### Updated during Sept/Oct 2006 assessment... primarily to deal with annual data...
### Big bad ugly kludges to get code working.... don't expect this stuff to make any sense (Vivian?)                                                                               
###
### Further changes made by Adam Smith, Oct 2006. 
### In most cases, my changes are  indicated in the code by "AS edit".
###  Functions now working: RdevArni, BvulnArni, CPUEArni, CPUEresArni, ErateArni, SPArni, TraceHisto, Erate
###    Not working: LFresArni and LFArni don't work
###    Not been fixed: PRI (and similar) 
###  Internal functions "PeriodToFishingYear" and "PeriodToSeason" have also been changed
#***********************************************************************************************

rm(list=ls())

filelist <- list.files("R/")
for (i in filelist) source(paste("R/", i, sep = ""))

#These need to be adjusted later!                                                               
ModelEndPeriod <- 120 #Period End of the model                                                    
firstrow <- 1 #MCMC posterior row beginning for plot                                              
lastrow <- 3000 #MCMC posterior row ending for plot                                               
#Fix this labels so that it reads same as "project.out". This is so that plot prints out symbols as symbols not "."
datalab <- c("f","lnStilde", "lnR0","M","InitER","sigmaR","SRSteepness","qCPUE","qCR","qPRI","qPoo","CPUEpow","mat50","mat95add",
             "GalphaM","GBetaM","GdiffM","GshapeM","GCVM",
             "GalphaF","GBetaF","GdiffF","GshapeF","GCVF",
             "StdMin","StdObs","GrowthDD",
             "1vuln","2vuln","3vuln","4vuln","VL1M","VR1M","SelectMax1M","VL1F","VR1F","SelectMax1F","VL2M","VR2M",
             "SelectMax2M","VL2F","VR2F","SelectMax2F",
             "Bref","Bmin","Bcurr","Bproj","Bmsy","U_SLcurr","U_SLproj","Fmult","MSY","CPUE_MSY","CPUEcurr","CPUEproj")
NoMPDpoints <- c("Ballproj","Brectproj","Bvulnproj","UNSLproj","USLproj",
                 "Bproj","Bproj/Bcurr","Bproj/Bmin","Bproj/Bref",
                 "Uproj","Uproj/Ucurrent") 
LFArniCaption <- "2001 SS CS"

# Parameters to modify if necessary for the ErateArni function
first_year <- 1944 # (First year -1) of the data used in the model to calculate the PeriodToFishingYear
year_2season <- 1961 # Year used to calculate the fishing years from when there are two seasons
year_switch <- 1979 # First year of the two season fishery
switch_period <- year_switch - first_year


#Helper functions and constants                                                                 
#CPUE, CR and PRI options                                                                       
#   ObsPred=TRUE plots Obs and Pred plot                                                        
# Resid=TRUE plots three different residual plots: Resid vs time-step, Resid vs Pred, and QQplot
#LF options
#   ObsPred=TRUE plots Obs and Pred plot
#   ObsPredYFixed=FALSE makes y-axis of plot floating from sample to sample
#   Cum=TRUE makes Cumulated LF plot
#   StdRes=TRUE makes StdRes vs Size plot for each sample
#   ResPred=TRUE makes StdRes vs Pred plot by sex
#   BoxSize=TRUE makes StdRes vs Size boxplot by sex and by sex & season
#   ResQQ=TRUE restricts residual data to greater than 0.05
#   QQ=TRUE makes QQplot
#   Bubble=TRUE makes bubble plot of StdRes
#   Reshist=TRUE makes distribution of StdRes by sex
#   SamplesPerPage: number of samples per page, default=10
#   SizeLim: Size limit of lobsters, default=54 for male, 60 for females
#Tag options
#   StdResSize=TRUE makes boxplot of StdRes vs Predicted size
#   StdResRel=TRUE makes boxplot of StdRes vs number of release
#   StdResArea=TRUE makes boxplot of StdRes vs released area
#   StdResCond=TRUE makes boxplot of StdRes vs condition
#   StdResType=TRUE makes boxplot of StdRes vs tag type
#   Reshist=TRUE makes distribution of StdRes by sex
#Biomass plot options
#   VulnB=TRUE makes Vulnerable biomass plots
#   TotalB=TRUE makes Total biomass plots
#   RecB=TRUE makes Recruited biomass plots
CPUEOptions<-list(ObsPred=TRUE,Resid=TRUE)
SEXROptions<-list(ObsPred=TRUE,Resid=TRUE)
CROptions<-list(ObsPred=TRUE,Resid=TRUE,IsAnnual=TRUE)
PRIOptions<-list(ObsPred=TRUE,Resid=TRUE)
LFOptions <- list(ObsPred=TRUE, ObsPredYFixed=FALSE, Cum=FALSE, StdRes=TRUE, ResPred=TRUE,
                  BoxSize=TRUE, ResQQ=TRUE, QQ=TRUE, Bubble=FALSE,Reshist=TRUE, 
                  SamplesPerPage=8,SizeLim=c(54,60,60), line = 1, sexcode = 1:3)
TagOptions <- list(StdResSize = TRUE, StdResRel = TRUE, StdResArea = TRUE,
                   StdResCond = TRUE, StdResType = TRUE, Reshist = TRUE,
                   xlim.male = c(40,90), xlim.female = c(40,90))
BioOption<-list(VulnB=TRUE,TotalB=TRUE,RecB=TRUE,TotalRecB=TRUE)

#These are all for graphics                                                                 
#Change this to use Period or Fishing Year for plots                                        
#A flag for turning off or on the captions on the graphs                                    
#Change this to change the line width                                            
#Sex: 1=male, 2=immature female, 3=mature female
PlotOptions <- list(UsePeriod = FALSE, Captions = TRUE, plottype = "png",
                    thick = 2, thin = 1, SexNames = c("Males","Immature\n\nfemales","Mature\n\nfemales"))


#For MCMC posteriors plots                                                                  
#Number of plots in each window                                                             
n.post<-12
begin.newplot<-c(1,39) #the column number where you want new plot
Rewrite<-1 # 1: to re-calculate running mean for diagnostic otherwise 0


#This viewer is divided up into seperate functions related to each type of data set.
#The functions are:
#MPD plots:
# Catches("foldername","stock") - plots of SLC and NSLC catches by period
# CPUE("foldername","stock") - plots of obs and pred CPUE by period (seperate lines), residuals by period, histogram of residuals
# CR("foldername","stock") - plots of obs and pred catch rates by period (seperate lines), residuals by period, histogram of residuals
# PRI("foldername","stock")  - plots of obs and pred pre-recruit index
# LF("foldername","stock")  - plots and residual analyses
# Tag("foldername","stock")  - residual analyses
# Erate("foldername","stock") - plots exploitation rate
# Rect("foldername","stock") - plots recruitment in millions
# LFzero("foldername","stock") - plots initial population structure
# Mature("foldername","stock") - plots maturation curve
# Moult("foldername","stock") - plots increment per moult
# Select("foldername","stock") - plots selectivity in each epoch by sex
# Bio("foldername","stock") - plots biomass
# SPBrect("foldername","stock") - plots recruited biomass vs surplus production
#
# MPD.All("foldername","stock") gives all of above
# MPD.Basic("foldername","stock") gives all of above except Catches, Moult, and SPBrect
#
# MCMC - plots and diagnostics for MCMC runs
#     Erate - plots Exploitation rate 
#     Rec - plots Recruitment
#     Select - plots Selectivity
#     Moult - plots Growth increment
#     LFzero - plots initial length structure
#     Biomass - plots biomass


#run <- "/home/darcy/Documents/CRA/2014/model/CRA1/"
#stock <- "CRA1"
run <- "examples/CRA3"
stock <- "CRA3"

  # Working
  LF(run, stock, LFOptions, PlotOptions)
  CPUE(run, stock, CPUEOptions, PlotOptions)
  Catches(run, stock, PlotOptions)
  Select(run, stock, PlotOptions)
  Tag(run, stock, TagOptions, PlotOptions)


  # Not done
  CR(run,stock) #no CR in these runs
# vh: comment out PRI plots cause crashes when no PRI output files
  POO(run,stock,CROptions,PlotOptions)
  Erate(run,stock)
  Rect(run,stock)
  LFzero(run,stock)
  Mature(run,stock)
  Moult(run,stock)
  Bio(run,stock)
  SPBrect(run,stock,PlotOptions)
  #DDgrowth(run,stock)
  sexRatio(run,stock,SEXROptions,PlotOptions)
