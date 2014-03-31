## CHANGES IN VERSION 0.3.9 (2014-03-31)

### BUG FIXES

* Assigning group names now works correctly with generic default values and
  whenever groups (ids) are changed. (~~[issue29](https://github.com/cbig/zonator/issues/29)~~)
  
* Generic group naming now works, i.e. if group ids are changed group names
  get a generic value "group1", "group2" etc. (~~[issue22](https://github.com/cbig/zonator/issues/22)~~)

### NEW FEATURES

* Assigning group ids with `groups()<-` now recalculates group specific 
  stats (min, mean, max, weighted mean) for defined groups. In other words,
  it is now possible to define groups other than those defined in Zonation
  groups file and have the group curves data calculated for the new groups.

* `cross_jaccard()` now returns a dataframe with correct RasterLayer names
  colnames and rownames.
  
* `cross_jaccard()` now accpets a numeric vector of threshold values for
  comparison.
  
* `jaccard()` now has additional parameters for controlling the thresholds for
  rasters `x` and `y` being compared. Arguments `xmin`, `xmax`, `ymin`, and 
  `ymax` can be used to control which ranges of the raster values are compared.

* New methods for class `Zproject`:
  - Method `rank_rasters()` accepts a new argument `variants` which can be
  either a character of numeric vector of variant names / IDs defining which
  rank rasters are included in the RasterStack that is returned. Default is 
  to return all.
  
* New methods for class `Zvariant`:
  - Methods `weights()` returns a numeric vector of weights assigned to a 
  variant.

## CHANGES IN VERSION 0.3.8 (2014-03-17)

### BUG FIXES

### NEW FEATURES

* Creating new Zproject object based on an existing Zonation project involves
  reading in a lot of files and it can be useful to know the sequence of reading
  in case something goes wront. `create_zproject()` now accepts a new argument
  `debug=TRUE` which enables logging of file reading sequence. (~~[issue28](https://github.com/cbig/zonator/issues/28)~~)

* New methods for class `Zvariant`:
  - Generic replacement method `groups()<-`
  - Method `nfeatures()` returns the number of features in a variant


## CHANGES IN VERSION 0.3.7 (2014-02-05)

### BUG FIXES

* Fix bugs in tests
* `check_zonation()` fixed on Windows (~~[issue26](https://github.com/cbig/zonator/issues/26)~~)

### NEW FEATURES

* New methods for class `ZCurvesDataFrame`:
  - `featurenames()`

## CHANGES IN VERSION 0.3.6 (2014-01-24)

### BUG FIXES

* Fix initial feature naming (~~[issue24](https://github.com/cbig/zonator/issues/24)~~)
* `check_path()` works on Windows (~~[issue23](https://github.com/cbig/zonator/issues/23)~~)
* Fix several tests on Windows 

## CHANGES IN VERSION 0.3.5 (2014-01-20)

### NEW FEATURES

* Results vignette has bee updated.

* Post-processign (PPA) LSM results are now handled by `Zresults`. Results,
if present, are read in from the output folder. More specifically:
  - Class `Zresults` now defines a new slot `ppa.lsm` tha holds the content
  of PPA LSM results

* `zlegend()` can be used to fetch Zonation rank raster map legend schemes. So
  far only one scheme ("spectral") is implemented.

* New methods for class `Zproject`:
  - `rank_rasters()`: returns a `RasterStack` of all priority rank rasters
  of all variants within a project

* New methods for class `Zvariant`:
  - Implented `curves()`

* New methods for class `Zvariant` and `Zresults`:
  - `rank_raster()`: returns `RasterLayer` of priority rank raster of a given
  variant or its results

## CHANGES IN VERSION 0.3.4 (2014-01-14)

### NEW FEATURES

* New methods for class `Zproject`:
  - `variants()`: returns a list containing all the variants within a project

* New methods for class `Zvariant` and `Zresults`:
  - `outdir()`: returns the path to location of output dirctory
  
## CHANGES IN VERSION 0.3.3 (2013-12-20)

Maintance update, R CMD check and TravisCI test passes after few minor fixes.

## CHANGES IN VERSION 0.3.2 (2013-12-19)

* Class `Zcurves` has been refactored to 2 classes: `ZCurvesDataFrame` and 
`ZGroupCurvesDataFrame`. Change was introduced mostly to handle performance
curves plotting more sensibly.

### NEW FEATURES

* Generic `groupnames()` now returns a character vector instead of a named 
character vector (with names being the original group numbers)

* `plot()` now works for `ZCurvesDataFrame` and `ZGroupCurvesDataFrame` objecs.

* New methods for class `Zresults`:
  - `groupnames()`
  
## NEW DEPENDENCIES

* RColorBrewer 

## CHANGES IN VERSION 0.3.1 (2013-12-11)

### NEW FEATURES

* bat-files are read recursively when creating a project (~~[issue20](https://github.com/cbig/zonator/issues/20)~~)
* `check_paths()` now deals with relative path components 
* `has_results()` now returns a list of logicals instead of a single logical

* New methods for class `Zresults`:
  - `has_results()`: returns a list of TRUE/FALSE depending on whether the 
  particular results items (curves, grp.curves, rank, wrscr, prop) are available
  or not

## CHANGES IN VERSION 0.3.0 (2013-12-05)

* Much of the internal functionality has been re-written
* Documentation is improving, but still needs a lot of work

### NEW FEATURES

* New classes `Zresults` and `Zcurves`. See docs for methods and structure.

* New methods for class `Zvariant`:
  - `groupnames()<-`: assign human readable group names
  - `groupnames()`: get assigned unique groupnames and codes
  - `has_results()`: returns TRUE/FALSE depending on whether the variant has 
  results or not
  - `results()`: returns a `Zresults` object specific to a `Zvariant` object

## CHANGES IN VERSION 0.2.2 (2013-11-28)

* Feature can be named for each variant and feature and group identities can
be joined and queried. 

### NEW FEATURES

* New methods for class `Zvariant`:
  - `featurenames()<-`: assign human readable feature names
  - `featurenames()`: get assigned unique featurenames
  - `sppdata()`: return the whole spp data frame of a variant

## CHANGES IN VERSION 0.2.1 (2013-11-27)

* Add groups functionality to `Zvariant`. Groups are read in from the actual
Zonation input file and can be used (eventually) in many operations.

### NEW FEATURES

* New methods for class `Zvariant`:
  - `groups()`: returns a numeric vector containing group codes for features
  - `groupnames()<-`: assign human readable group names to groups
  - `groupnames()`: get assigned unique groupnames with associated group codes 

* New utlity functions:
  - `read_groups()`: read in Zonation groups file

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
  - `names()`: returns a character vector of the names of variants in a project

* New methods for class `Zvariant`:
  - `has_results()`: boolean indicating whether the variant already has associated
    results

* New utlity functions:
  - `read.spp()`: read in spp files
