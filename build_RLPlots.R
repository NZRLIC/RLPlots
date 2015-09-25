
# vanilla session
rm(list=ls())

# update description file
VERSION <- "1.01"
DATE    <- Sys.Date()

DESCRIPTION <- readLines("DESCRIPTION")
DESCRIPTION[3] <- paste("Version:", VERSION)
DESCRIPTION[4] <- paste("Date:", DATE)
writeLines(DESCRIPTION, "DESCRIPTION")

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
roxygen2::roxygenize()

# build package
library(devtools)
build(binary=TRUE)

# install package locally
pkg_name <- paste("RLPlots_",VERSION,".zip",sep="")

# install package locally
pkg_name <- paste("../RLPlots_",VERSION,".zip",sep="")
install.packages(pkg_name, repos = NULL)

# tidy up
file.remove(pkg_name)
