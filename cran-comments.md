## Submission

This release is a re-submission to CRAN after the package was archived on CRAN
two years ago. The reason was that hard coded paths caused an error
with the new staged install functionality introduced in R 3.6.0. Hard coded 
paths are now properly managed (set within a function) and package passes
staged install. 

## Test environments

* local openSUSE Linux x64 install, R 4.0.0
* Ubuntu 16.04.6 LTS x64 (on Travis CI), R 3.6.3, R 4.0.0, R devel
* Windows Server 2012 R2 64-bit (on AppVeyor), R 4.0.0
* win-builder (oldrelease, release and devel)

## R CMD check results

Status: 2 NOTES

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Joona Lehtomaki <joona.lehtomaki@gmail.com>’

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Zonation (3:22, 13:5)

-> not mis-spelled.

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

-> see above.

## Downstream dependencies

There are currently no downstream dependencies for this package.
