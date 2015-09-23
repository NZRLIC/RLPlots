# Take a look at: https://gist.github.com/halpo/1374344

PKG_VERSION=$(shell grep -i ^version ../RLPlots/DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package ../RLPlots/DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard ../RLPlots/R/*.R)

ifdef ComSpec
	RCMD=Rcmd
	RM=del /F /Q
then
	RCMD=R CMD
	RM=rm -rf
	ROXY=R --vanilla -e 'require(roxygen2);roxygen2::roxygenize("RLPlots/")'
endif

build: $(R_FILES)
	R --vanilla -e 'require(roxygen2);roxygen2::roxygenize("../RLPlots/")'
	chmod 777 ../RLPlots/DESCRIPTION
	R CMD INSTALL --build ../RLPlots/

clean:
	$(RM) $(PKG_NAME)_*.tar.gz
