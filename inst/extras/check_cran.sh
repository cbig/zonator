#!/bin/sh

CURRENT_TAR=zonator_0.5.7.tar.gz

/usr/bin/R CMD BATCH document.R
/usr/bin/R CMD build ../../
/usr/bin/R CMD check --as-cran $CURRENT_TAR
