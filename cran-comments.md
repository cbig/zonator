## Test environments
* local openSUSE Linux x64 install, R 3.3.2
* ubuntu 14.04 (on travis-ci), R 3.3.2
* Windows Server 2012 R2 x64 (on AppVeyor), R 3.3.2
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 2 NOTES.

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Joona Lehtomaki <joona.lehtomaki@gmail.com>’

New submission

This is my first submission to CRAN after assuming the maintainer role of the package.

* checking installed package size ... NOTE
  installed size is 14.3Mb
  sub-directories of 1Mb or more:
    doc       1.9Mb
    extdata  11.6Mb

Subdirectory extdata includes data relevant for testing and demonstration, but which are not required by the package. These will be moved into a separate data package. 

## Downstream dependencies

There are currently no downstream dependencies for this package.
