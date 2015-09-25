RLPlots
=======

`RLPlots` is an R package for plotting rock lobster (RL) stock assessment
outputs and management proceedure evaluations (MPEs). Plotting functions are
available for MPD fits, Markov chain Monte Carlo (MCMC) and some other plots.


# Installation

RLPlots can be installed from R using

    install.packages("devtools")
    devtools::install_github("NZRLIC/RLPlots")

To install the devtools package requires Rtools, which is available from

	https://cran.r-project.org/bin/windows/Rtools/
	
# Using RLPlots

## MPDs

Plotting of MPDs can be done using, for example

    MPD.All(stock = "CRA1")


## MCMCs

Plotting of MCMCs can be done using

    MCMC.All(stock = "CRA1", source.dir = ".")

Plotting multiple MCMC chais on a single plot can be done using    

    Multichain.All(stock = "CRA1", source.dir = c("chain1/", "chain2/"))
    
The snail trail plot can be done using

    Snail(stock = "CRA1", source.dir = ".")


## MPEs

To plot MPE rules and results you must first read in the data before plotting

    dat <- read.sims.sum(source.dir = ".")
    # To plot all rules
    MPE_rules(control.pars = dat[,13:22], target.dir = ".")
    MPE_rules(control.pars = dat[irule,13:22], current.cpue = 1.48, target.dir = ".")
    mpeview(dat = dat, stock = "CRA1", point.col = "par5", target.dir = ".",
            pars = c("avI_med","AvComm_med","P<Bref","AAV_med"),
            axis.labels = list(x = "CPUE", y = c("Commercial catch (tonnes)","P<Bref","%AAV")))


# Developer commandments

-  Thou shalt use four-space indentation
-  Thou shalt place parentheses below function/loop/if statements
-  Thou shalt use spaces between equal signs
-  Thou shalt use a space after a comma
-  Thou shalt use comments within a function
-  Thou shalt fill out the doxygen header for each function
-  Thou shalt test thy code

For example

    #' Doxygen header function name
    #'
    #' Description
    #'
    #' @author Who
    #' @param var1 a variable
    #' @param var1 another variable
    #' @return some stuff
    #' @export
    #'
    func <- function(var1 = 1, var2 = 2)
    {
        # An example
        if (var1 == 1) do.something(var1)
        # Or
        if (var2 != 3)
        {
            do.something(var2)
        }
    }
