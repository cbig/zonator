## Submission

This release updates all tests to fix the breaking changes introduced in 
testthat release 2.0.

## Test environments

* local openSUSE Linux x64 install, R 3.4.3
* Ubuntu 14.04 x64 (on travis-ci), R 3.4.2
* Windows Server 2012 R2 32-bit (on AppVeyor), R 3.4.3
* win-builder (devel and release)

## R CMD check results

Status: 2 NOTES

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Joona Lehtomaki <joona.lehtomaki@gmail.com>’

Suggests or Enhances not in mainstream repositories:
  zdat
Availability using Additional_repositories specification:
  zdat   yes   https://jlehtoma.github.io/drat

-> zdat is a large data package (7.7 Mb) that is needed
to demonstrate full functionality of zonator and building 
the vignettes. Package can be installed from GitHub using drat:

install.packages('zdat', repos='https://jlehtoma.github.io/drat/', type='source')

* checking package dependencies ... NOTE
Package suggested but not available for checking: 'zdat'

-> See above.

## Downstream dependencies

There are currently no downstream dependencies for this package.
