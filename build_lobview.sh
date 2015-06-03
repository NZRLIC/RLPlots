echo
echo == Building helpfiles ==
R CMD BATCH update-description.R
R CMD BATCH run-roxygen.R


echo
echo == Building source ==
R CMD build lobview


echo == Installing ==
R CMD INSTALL lobview_*.tar.gz


echo
echo == Building for windows ==
mkdir localRlib
rm -r lobview.zip
R CMD INSTALL -l localRlib lobview_*.tar.gz
cd localRlib
zip -r lobview lobview
mv lobview.zip ..
cd ..
rm -r localRlib/

cd lobview
chmod 777 DESCRIPTION
