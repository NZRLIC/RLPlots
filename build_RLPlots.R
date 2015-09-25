
# vanilla session
rm(list = ls())

# Get Version and date
VERSION <- scan('DESCRIPTION',what = character(),skip = 2,nlines = 1)[2]
DATE    <- Sys.Date()
TIME    <- Sys.time()

# increment development version
VERSION <- paste(substr(VERSION, start = 1, stop = 6), as.numeric(substr(VERSION, start = 7, stop = 10)) + 1, sep = '')

# update DESCRIPTION
DESCRIPTION    <- readLines('DESCRIPTION')
DESCRIPTION[3] <- paste('Version:', VERSION)
DESCRIPTION[4] <- paste('Date:', DATE)
writeLines(DESCRIPTION, 'DESCRIPTION')
rm(DESCRIPTION)

# Write .onAttach
filename <- "R/zzz.R"
cat(".onAttach <- function(libname, pkgname)\n", file = filename)
cat("{\n", file = filename, append = TRUE)
cat(paste("    packageStartupMessage(\"RLPlots version ", VERSION, " (", TIME, ")\")\n", sep = ""), file = filename, append = TRUE)
cat("}\n", file = filename, append = TRUE)
rm(filename)

# run roxygen
roxygen2::roxygenize()

# build package
devtools::build(binary = TRUE)

# install package locally
pkg_name <- paste("../RLPlots_",VERSION,".zip",sep = "")
install.packages(pkg_name, repos = NULL)

# tidy up
file.copy(pkg_name, "C:/PROJECTS/CRA201201B - lobster/lobster")
file.remove(pkg_name)
