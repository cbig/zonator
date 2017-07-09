## Resubmission
This is a resubmission. In this version I have:

* Added an URL for the Zonation software in DESCRIPTION 
Description field.

* Added a prebuilt vignette index at zonator/build/index.rds.

## Test environments

* local openSUSE Linux x64 install, R 3.4.0
* Ubuntu 14.04 (on travis-ci), R 3.4.0
* Windows Server 2012 R2 32-bit (on AppVeyor), R 3.4.0
* win-builder (devel and release)
* Debian Linux, R-devel, GCC ASAN/UBSAN (on R-hub builder), R-devel

## R CMD check results

0 ERRORs | 0 WARNINGs | 2 NOTES.

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Joona Lehtomaki <joona.lehtomaki@gmail.com>’

New submission

-> This is my first submission to CRAN .

Possibly mis-spelled words in DESCRIPTION:
  Zonation (3:22, 13:5)

-> Not a mis-spelled word, but actual name of software.

Suggests or Enhances not in mainstream repositories:
  zdat

-> zdat is a large data package (7.7 Mb) that is needed
to demonstrate full functionality of zonator. Package
can be installed from GitHub using drat:

install.packages('zdat', repos='https://jlehtoma.github.io/drat/', type='source')

## Downstream dependencies

There are currently no downstream dependencies for this package.
