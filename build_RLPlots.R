
# vanilla session
rm(list=ls())

# update description file
VERSION <- "1.01"
DATE    <- Sys.Date()

DESCRIPTION <- readLines("DESCRIPTION")
DESCRIPTION[3] <- paste("Version:", VERSION)
DESCRIPTION[4] <- paste("Date:", DATE)
writeLines(DESCRIPTION, "DESCRIPTION")

<<<<<<< HEAD:build_lobview.R
# Write RLPlots.version()
=======
# Write lobview.version()
>>>>>>> 80774bd0b6d3423d40e5461db381d6b3cd8e0419:build_RLPlots.R
filename <- "R/RLPlots.version.R"
cat("#' Function to return version number\n", file = filename)
cat("#'\n", file = filename, append = TRUE)
cat("#' @export\n",file = filename, append = TRUE)
cat("#'\n", file = filename, append = TRUE)
cat("lobview.version <- function()\n", file = filename, append = TRUE)
cat("{\n", file = filename, append = TRUE)
cat(paste("    return(\"Version: ", VERSION, "\\n", "Compile date: ", DATE, "\\n\")\n", sep = ""), file = filename, append = TRUE)
cat("}\n", file = filename, append = TRUE)

# run roxygen
library(roxygen2)
<<<<<<< HEAD:build_lobview.R
roxygen2::roxygenize("../RLPlots/")

# build package
library(devtools)
build('../RLPlots', binary=TRUE)

# install package locally
pkg_name <- paste("RLPlots_",VERSION,".zip",sep="")
=======
roxygen2::roxygenize()

# build package
library(devtools)
devtools::build(binary=TRUE)

# install package locally
pkg_name <- paste("../RLPlots_",VERSION,".zip",sep="")
>>>>>>> 80774bd0b6d3423d40e5461db381d6b3cd8e0419:build_RLPlots.R
install.packages(pkg_name, repos = NULL)

# tidy up
file.remove(pkg_name)
