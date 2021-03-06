# Take a look at: https://gist.github.com/halpo/1374344

PKG_VERSION=$(shell grep -i ^version ./DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package ./DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard ./R/*.R)

ifdef ComSpec
	RM=del /F /Q
then
	RM=rm -rf
endif

linux: $(R_FILES)
	R --vanilla -e 'source("version_update.R"); roxygen2::roxygenize("../RLPlots/")'
	chmod 777 ../RLPlots/DESCRIPTION
	R CMD INSTALL --build ../RLPlots/

windows: $(R_FILES)
	R --vanilla -e 'source(\"version_update.R\"); roxygen2::roxygenize(\".\")'
	Rcmd INSTALL --build .

clean:
	$(RM) $(PKG_NAME)_*.tar.gz
