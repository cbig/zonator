#!/bin/sh

CURRENT_TAR=zonator_0.5.5.9001.tar.gz

#/usr/bin/R CMD BATCH document.R
#/usr/bin/R CMD build ../../ --no-build-vignettes
/usr/bin/R CMD build ../../
/usr/bin/R CMD check --as-cran $CURRENT_TAR
#/usr/bin/R CMD INSTALL $CURRENT_TAR
#/usr/bin/R CMD BATCH document.R
