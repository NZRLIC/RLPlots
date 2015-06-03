rem update description with version number and 
rem write version function
R --vanilla < update-description.R
rem run roxygen
R --vanilla < run-roxygen.R
rem compile and install package
Rcmd build --force lobview
Rcmd INSTALL --build lobview

