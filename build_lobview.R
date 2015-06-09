
# vanilla session
rm(list=ls())

# update description file
VERSION <- "1.01"
DATE    <- Sys.Date()

DESCRIPTION <- readLines("lobview/DESCRIPTION")
DESCRIPTION[3] <- paste("Version:", VERSION)
DESCRIPTION[4] <- paste("Date:", DATE)
writeLines(DESCRIPTION, "lobview/DESCRIPTION")

# Write lobview.version()
filename <- "lobview/R/lobview.version.R"
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
roxygen2::roxygenize("lobview/")

# build package
library(devtools)
build('lobview', binary=TRUE)

# install package locally
pkg_name <- paste("lobview_",VERSION,".zip",sep="")
install.packages(pkg_name, repos = NULL)

# tidy up
file.remove(pkg_name)
