## CHANGES IN VERSION 0.3.0 (2013-12-05)

* Much of the internal functionality has been re-written
* Documentation is improving, but still needs a lot of work

### NEW FEATURES

* New classes `Zresults` and `Zcurves`. See docs for methods and structure.

* New methods for class `Zvariant`:
  - `groupnames<-`: assign human readable group names
  - `groupnames`: get assigned unique groupnames and codes
  - `has_results`: returns TRUE/FALSE depending on whether the variant has 
  results or not
  - `results`: returns a `Zresults` object specific to a `Zvariant` object

## CHANGES IN VERSION 0.2.2 (2013-11-28)

* Feature can be named for each variant and feature and group identities can
be joined and queried. 

### NEW FEATURES

* New methods for class `Zvariant`:
  - `featurenames<-`: assign human readable feature names
  - `featurenames`: get assigned unique featurenames
  - `sppdata`: return the whole spp data frame of a variant

## CHANGES IN VERSION 0.2.1 (2013-11-27)

* Add groups functionality to `Zvariant`. Groups are read in from the actual
Zonation input file and can be used (eventually) in many operations.

### NEW FEATURES

* New methods for class `Zvariant`:
  - `groups`: returns a numeric vector containing group codes for features
  - `groupnames<-`: assign human readable group names to groups
  - `groupnames`: get assigned unique groupnames with associated group codes 

* New utlity functions:
  - `read_groups`: read in Zonation groups file

## CHANGES IN VERSION 0.2.0 (2013-11-22)

* Refactor all function names to follow underscore "_" pattern. Assume all
old calls broken.

## CHANGES IN VERSION 0.1.3 (2013-11-15)

* Updated the tutorial data shipping with zonator to correspond with the updated
Zonation tutorial (basic)
* Started using knit/Rmd to create vignettes
* Started NEWS document

### NEW FEATURES

* New methods for class `Zpoject`:
  - `names`: returns a character vector of the names of variants in a project

* New methods for class `Zvariant`:
  - `has_results`: boolean indicating whether the variant already has associated
    results

* New utlity functions:
  - `read.spp`: read in spp files