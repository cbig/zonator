#!/bin/sh

CURRENT_TAR=zonator_0.5.7.tar.gz

#echo "Building documentation and vignettes..."
#/usr/bin/R CMD BATCH document.R
echo "Building package..."
/usr/bin/R CMD build ../../
#echo "Checking package..."
#/usr/bin/R CMD check --as-cran $CURRENT_TAR
