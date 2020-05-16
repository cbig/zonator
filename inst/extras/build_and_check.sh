#!/bin/sh

# Version key/value should be on his own line
# From: https://gist.github.com/DarrenN/8c6a5b969481725a4413
PACKAGE_VERSION=$(cat ../../DESCRIPTION \
  | grep Version \
  | head -1 \
  | awk -F: '{ print $2 }' \
  | sed 's/[",]//g' \
  | tr -d '[[:space:]]')

DIST_DIR="../../dist/"

if [ ! -d $DIST_DIR ]; then
  mkdir -p $DIST_DIR;
fi

PACKAGE_TAR="zonator_$PACKAGE_VERSION.tar.gz"

R CMD build ../../
mv $PACKAGE_TAR $DIST_DIR
R CMD check --as-cran $DIST_DIR$PACKAGE_TAR
rm -R zonator.Rcheck
