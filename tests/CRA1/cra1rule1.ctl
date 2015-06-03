#Name of file with Reference points (MSY related)
stackedinformed.msy
#first and last mcmc record... code currently allows a maximum of 3000 records in ref. file 
1 1000
# print switch 
0
# non-commercial catches (these are also catches except for Recr)...by region: recreational, custommary, illegal
44.6 10 72
# slope and intercept of function for calculating proportion of SL catch by season (by region)
# uses predicted AW CPUE
0.0992    0.4752
# slope and intercept for converting mean CPUE (SS/AW) to offset year CPUE (by region)
0.9779198    0.038435
# control parameters
# 1: recruitment is changed by this proportion (ie. a value of 0.3 reduces recruitment by 30%)
# 2: increases the std. dev. of "observed" CPUE by this amount (ie. a value of 2 doubles the std. dev.)
# 3: q trend - qCPUE increases by this proportion each year 
# 4: proportional change to illegal catch
# 5: if > 0, then the recreational catch removed each year is equal to the recreational allowance
# 6: if not equal to -999, then the mean recr dev. is set to this value (overrides the value from the years specified in assessment control file)
#  NOTE: cntrl par(6) will screw you up, because the inclination will be to have a value of 0
# 7: if not equal to 0, then the std. dev. of recr. devs. is set to this value (overrides as above)
0    0    0    0    0    -999    0    0    0    0
# number of rules
20
# parameter values for each rule (10 pars for each region... so region 1 is first ten... then region 2, etc.)
1 1 0 0 0 0 0 0 0 0
1 10 0 0 0 0 0 0 0 0
1 30 0 0 0 0 0 0 0 0
1 50 0 0 0 0 0 0 0 0
1 70 0 0 0 0 0 0 0 0
1 90 0 0 0 0 0 0 0 0
1 110 0 0 0 0 0 0 0 0
1 140 0 0 0 0 0 0 0 0
1 170 0 0 0 0 0 0 0 0
1 200 0 0 0 0 0 0 0 0
1 240 0 0 0 0 0 0 0 0
1 280 0 0 0 0 0 0 0 0
1 320 0 0 0 0 0 0 0 0
1 360 0 0 0 0 0 0 0 0
1 400 0 0 0 0 0 0 0 0
1 450 0 0 0 0 0 0 0 0
1 500 0 0 0 0 0 0 0 0
1 550 0 0 0 0 0 0 0 0
1 600 0 0 0 0 0 0 0 0
1 700 0 0 0 0 0 0 0 0
#check number
12345
# first and last year for calculating average recr exploitation rate for projections
1979 2013
# Recreational catch:by region, by year (1979-last year) by period// used to calculate quasi exploitation rates
# input needs to be for 1979 to last year... independent of what years used to calc avg.
# CRA2 (1979 to 2013:)
4.270002    38.43002
5.033513    45.30162
5.885632    52.97069
4.647025    41.82323
4.797705    43.17935
4.23827    38.14443
4.662923    41.96631
4.402269    39.62042
4.20857    37.87713
4.468577    40.21719
4.227082    38.04374
3.810347    34.29312
3.18192    28.63728
2.667442    24.00698
2.99304    26.93736
4.311782    38.80604
5.590355    50.3132
4.693501    42.24151
4.671241    42.04117
4.171627    37.54464
2.952563    26.57307
4.12153    37.09377
4.623923    41.61531
3.278152    29.50337
2.888896    26.00006
4.330286    38.97257
3.789735    34.10762
5.337136    48.03422
5.888192    52.99373
4.481654    40.33489
5.095491    45.85942
4.587668    41.28901
4.2007    37.8063
5.745955    51.7136
4.460306    40.14275
# last offset year of CPUE data (begins 1980 to last year offset CPUE (annual, offset year),regions in columns
2013
0.8671
0.9199
0.9738
0.997
0.9486
0.8441
0.8313
0.755
0.7814
0.6234
0.68
0.6332
0.6619
0.5986
0.7539
0.9841
1.1107
1.0022
1.0246
0.9863
0.9735
1.2415
1.1777
1.0259
1.1948
1.3285
1.512
1.7618
1.7429
1.7825
1.549
1.6101
1.6827
1.5083
#checknumber (12s assure blank line at end)
12345
12
12
