## Submission

This is a bugfix following 0.5.7 realesed on
CRAN addressing two issues reported by Prof Brian Ripley.

* The startup message in 0.5.7 version was
plain wrong: zonator does not require zdat data 
package to operate, only to build the vignettes. The
startup message has now been removed.

* Code (and tests) were failing on Solaris as I have
no access to this platform. I've tried to address the
issue as suggested by Prof Ripley, but still cannot 
test the package on Solaris.

## Test environments

* local openSUSE Linux x64 install, R 3.4.1
* Ubuntu 14.04 (on travis-ci), R 3.4.0
* Windows Server 2012 R2 32-bit (on AppVeyor), R 3.4.0
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE.

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
