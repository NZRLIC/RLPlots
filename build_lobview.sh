echo
echo == Building helpfiles ==
R CMD BATCH update-description.R
R CMD BATCH run-roxygen.R


echo
echo == Building source ==
cd ..
R CMD build RLPlots


echo == Installing ==
R CMD INSTALL RLPlots_*.tar.gz


echo
echo == Building for windows ==
mkdir localRlib
rm -r RLPlots.zip
R CMD INSTALL -l localRlib lobview_*.tar.gz
cd localRlib
zip -r RLPlots RLPlots
mv RLPlots.zip ..
cd ..
rm -r localRlib/

cd RLPlots
chmod 777 DESCRIPTION
