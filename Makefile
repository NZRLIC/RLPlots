# Take a look at: https://gist.github.com/halpo/1374344

PKG_VERSION=$(shell grep -i ^version ../RLPlots/DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package ../RLPlots/DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard ../CRACE/R/*.R)

ifdef ComSpec
	RCMD=Rcmd
	RM=del /F /Q
	ROXY=R --vanilla -e 'require(roxygen2);roxygen2::roxygenize("../RLPlots/")'
then
	ROXY=-e 'require(roxygen2);roxygen2::roxygenize("../RLPlots/")'
	CH=chmod 777 ../RLPlots/DESCRIPTION
	RCMD=R CMD INSTALL --build
	RM=rm -rf
endif

linux: $(R_FILES)
	R --vanilla -e 'require(roxygen2);roxygen2::roxygenize("../RLPlots/")'
	chmod 777 ../RLPlots/DESCRIPTION
	R CMD INSTALL --build ../RLPlots/

windows: $(R_FILES)
	R --vanilla -e 'source(\"version_update.R\"); roxygen2::roxygenize(\"../RLPlots/\")'
	Rcmd INSTALL --build ../RLPlots/

clean:
	$(RM) $(PKG_NAME)_*.tar.gz
