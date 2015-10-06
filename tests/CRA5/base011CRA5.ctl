# Control file for RL model Sept-Oct 2015
# run description (one word only)
CRA5MCMC011
# first yr; last yr (can be overwritten with -retro CL switch); last data yr; last projection year;
# last data year stays constant and last yr changes to run retrospectives
# for standard projections, last proj yr is last year + 4; for MPEs last yr plus 21
1945 2014 2014 2018
# year that seasons/yr changes (from 1 season/yr); #seasons/yr
1979 2 
#take berried females?  0==no, 1==yes; one value for each season specified in previous line
0 1
# first length (left side of bin); last length; bin increments; mean recr length, std of recr length
30 90 2 32 2 
# number of regions
1
# number of tagging areas (areas with different growth and associated tag data sets)
1
# number of tag data files for each tagging area (vector with number for each tagging area)
1
# first and last year that each tag data set applies to (order tag data sets so earliest years before later years)
1945 2018
# for each region, which area is used to calculatge growth transition matrices
1
# names of regions data files (all one line, max length is 80)
CRA5poo.dat
# names of tag data files (all one line, max length 80)
CRA5
# just a check number
12345
#  6 Likelihood switches - 0 or 1 for 
#  6 Likelihood types - Options: 1) normal_log 2)lognormal 3) Multinomial 4) robust_Fournier 5) robust_Coleraine 
#                   6) normal_BentleyVar 7) robust_normal 8)robust-lognormal  9) normal
#                  10) for LF data; separate multinomial Ns for sex-ratio; males, immatures, females  (N for sex-ratio is LF like-weight)
#  7 Likelihood weights (extra is penalty for exceeding max. ExRate )
# if you change a switch here CHECK PHASES BELOW!!
#      LFs   Tag       CPUE    PRI      CR   Poo       U
         1      1       1       0       1    1
        10      7       2       2       2    1
         4    1.0     2.6       1       4    0.3   1000000
# if LFs likelihood type is 10, then values below of effective Ns for males; immature females; females 
# Need this line with three dummy values when LF likelihood type is not 10 
0.98  0.27  0.87	
# CPUE process error; Adhoc CPUE process error, then year for Adhoc CPUE process error 
# Adhoc process error applies to that year and all later years; to turn off make the year > last_yr
0.25 0.25  2100
# CR rel. sigma; PRI rel. sigma
0.3  0.3
# 1 to activate RunManager
0
# fishing mortality dynamics switch
# 0 for finite fishing mortality dynamics; 1 for instantaneous fishing mortality dynamics using Newton-Raphson
# >=2 instantaneous with estimated fpars; 3 same but with high CV for Cnsl)
# recent assessments have used 3
1
# number of Newton/Raphson iterations when doing instantaneous F dynamics (as in projections, for
#  example)... if 0 this value is ignored.. can be overwritten with command line argument -iter n
3
# Poo lag for each region, lag between settlement and rect to model
# if negative, then model doesn't fit to poo for that region
# (can be overwritten with command line input... -PooLag n
1
#growth model type (one integer): 0 is Schnute-Francis; 1 is inverse logistic
0
# >0 to turn on density-dependent growth
#1  no longer use this... estimate dd growth by turning on Gdd parameter through phase
# 0 for no S-R function, 1 for B-H; then lag between spawning and recruitment
# both values required
0 2
# if 0 then movement not estimated (est if >0)
0
# number of movements: ie number of from's-to's
0
# size range of fish that move
45 60
# year range where movement allowed
1985 2010
# from to (for each movement)
# when number of movements is zero, no values here!!

# 
# Implicit value - normally leave set to 1 !!!
# value of zero gives the implicit result of the priors with no effect from data
1
# which M for each region
1
# which Rdevs for each region
1
# first and last years for estimating Rdev's (one set for each Rdev sequence specified in previous line)
# the last year should be greater than last data year when estimating poo (can be last poo year + poolag)
1945 2015
# which type of recruitment projection
1
# first and last year for re-sampling Rdev's for projections (same as above)
# recent practice has been the most recent ten years
2003 2012
# which sigmaR for each region
1 
# which steepness for each region
1 
# which qCPUE for each region
1 
# which qCR for each region
1 
# which qPRI for each region
1 
# which qPoo for each region
1 
# which CPUEpow for each region
1 
# which maturity curve for each region
1 
# which initial exploitation rate
1 
# For the following parameters only one set of phases, bounds, etc. (ie. not region specific)
# when turning pars off and on here, CHECK LL switches!!
# if you reduce the number of phases, check the maxfn vector in the code or use -maxfn command line
# 1 is normal; 2 is lognormal; 3 is robust normal; 4 is robust lognormal; 5 is log-uniform (assumes parameter is in log space)
#sigmat:   ph;  lb;  ub; PR type; mean; std/CV; init;
            -4   -10    4        0    0     0.2   .001
#lnR0:     ph;  lb;  ub; PR type; mean; std/CV; init;
            1    1   25        0     0       0    18
# init ER:  ph;  lb;  ub; PR type; mean; std/CV; init;
            -4    0   0.99      0     0       0    0
# M:       ph;  lb;  ub; PR type; mean; std/CV; init;
            4  .01  .35        2   .12      .4   .12
# Rdev:    ph;  lb;  ub; PR type; 
            2  -2.3  2.3       1
# sigmaR:  ph;  lb;  ub; PR type; mean; std/CV; init;
           -4  0.1   0.8       1   0.4     0.2   0.4 
#steepness:ph;  lb;  ub; PR type; mean; std/CV; init;
           -4   0.2    1       1   0.75    0.2   0.8 
# qCPUE:   ph;  lb;  ub; PR type; mean; std/CV; init;
            1  -25    0        0     0       0  -6.
# qCR:     ph;  lb;  ub; PR type; mean; std/CV; init; 
            1  -25    2        0     0       0  -3.
# qPRI:    ph;  lb;  ub; PR type; mean; std/CV; init;
           -1  -25    0        0     0       0   -17.
# qPoo:    ph;  lb;  ub; PR type; mean; std/CV; init;
            1   -25    0        0     0       0   -6.
# CPUEpow: ph;  lb;  ub; PR type; mean; std/CV; init;
           -1  .001   2        0     0       0    1.
# Mat_50:  ph;  lb;  ub; PR type; mean; std/CV; init;
            3   30   80        0     0       0    60
# Mat_95:  ph;  lb;  ub; PR type; mean; std/CV; init;
            -3    1   60        0     0       0   3.26
# growth parameters males
#Galpha:   ph;  lb;  ub; PR type; mean; std/CV; init;
            2    1   20        0     0       0	   3.5
#Gbeta=Galpha*Gdiff (so 0-1 bounds make sense)
#Gdiff:    ph;  lb;  ub; PR type; mean; std/CV; init;
            2   .001  1        0     0       0    .8
# growth parameters females (!!NOTE female lb, ub and phase values are ignored!!)
#Galpha:   ph;  lb;  ub; PR type; mean; std/CV; init;
            2    1   20        0     0       0     3.5
#Gbeta=Galpha*Gdiff (so 0-1 bounds make sense)
#Gdiff:    ph;  lb;  ub; PR type; mean; std/CV; init;
            2   .001  1        0     0       0    .5
# males
#Gshape:   ph;  lb;  ub; PR type; mean; std/CV; init;
            3    .1  15        1     4.81       0.38     4.8
#G_CV:     ph;  lb;  ub; PR type; mean; std/CV; init;
            5    .01   2        1     0.59    .0076   0.59
#females
#Gshape:   ph;  lb;  ub; PR type; mean; std/CV; init;
            3    .1  15        1     4.51       0.24     4.5
#G_CV:     ph;  lb;  ub; PR type; mean; std/CV; init;
            5   .01   2        1     0.82    .013   0.82
#both
#G_DD:     ph;  lb;  ub; PR type; mean; std/CV; init;
           -5    0    1        0     0       0    .0
#GminSD:   ph;  lb;  ub; PR type; mean; std/CV; init;
           -5   .01   5        0     0       0   0.0001
#Gobs_std:   ph;  lb;  ub; PR type; mean; std/CV; init;
           5  .00001 10       1     1.48       0.015   1.5
#end of growth pars
#Cnsl_scaler:ph;  lb;  ub; PR type; mean; std/CV; init;
           -3    0.01    10       0     0       0  1.0
#
# selectivity type: 1 is logistic (2 parameters); 2 is double norm (3 parameters)
2
# which selectivity for each region
1 
# number of epochs for each set of selectivities (ie. max of previous line)
# current max is 2; and 2nd epoch begins 1993
2
# if logistic: sel1 is 50%; sel2 is 95%diff; (only input 2 sets of control values)
# if Dnorm: sel1 is LVar; sel2 is Rvar; sel3: is length at SelMax
# two sets of values; one for each sex
#Sel1:     ph;  lb;  ub; PR type; mean; std/CV; init;
            4    1   50        0   4.1    0.82    4.1
            4    1   50        0   9.2    1.84    9.2
#Sel2:     ph;  lb;  ub; PR type; mean; std/CV; init;
            -3    1  250        0     0       0   200
            -3    1  250        0     0       0   200
#Sel3:     ph;  lb;  ub; PR type; mean; std/CV; init;
            5   30   90        0    55     11      55
            5   30   90        0    64     12.8    64
# icheck1
12345
# which vulnerability for each region
1
# by sex, which vul_par for each season (if value is 0, then fixed at 1)
#males
  0 1
# immature females
  2 3
# mature females
  4 3
# Vul_par bounds, phases, etc (need the correct # of them, of course)
#Vul1:     ph;  lb;  ub; PR type; mean; std/CV; init;
            3   .01   1        0    0      0    .8
#Vul2:     ph;  lb;  ub; PR type; mean; std/CV; init;
            3   .01   1        0    0      0    .8
#Vul3:     ph;  lb;  ub; PR type; mean; std/CV; init;
            3   .01   1        0    0      0    .8
#Vul4:     ph;  lb;  ub; PR type; mean; std/CV; init;
            3   .01   1        0    0      0    .8
#movement parameters
           -4    0   0.4      0      0      0     0.0
# which wt pars for each region
1 
# wt pars  (a by sex, then b by sex -repeat if more than one wt matrix)
4.160E-06	1.300E-05	1.300E-05													
2.9354	2.545		2.5452		
# handling mortality proportion
0.1																		
# minimum survival proportion
0.02
#icheck 2
12345

