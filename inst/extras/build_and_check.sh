#!/bin/sh

# Version key/value should be on his own line
# From: https://gist.github.com/DarrenN/8c6a5b969481725a4413
PACKAGE_VERSION=$(cat ../../DESCRIPTION \
  | grep Version \
  | head -1 \
  | awk -F: '{ print $2 }' \
  | sed 's/[",]//g' \
  | tr -d '[[:space:]]')

PACKAGE_TAR="zonator_$PACKAGE_VERSION.tar.gz"

R CMD build ../../
mv $PACKAGE_TAR ../../dist
R CMD check --as-cran ../../dist/$PACKAGE_TAR
rm -R zonator.Rcheck
