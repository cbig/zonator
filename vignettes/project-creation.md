<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Create zonator project based on existing data}
-->

### Create `zonator` project based on existing data

The following examples will use 
[the basic Zonation tutorial data](https://github.com/cbig/zonation-tutorial/tree/master/basic)
that is distributed with zonator package. These data, however, do not include 
the results data. The first time you are trying these examples, you will 
therefore have to run the actual Zonation runs. Since it is recommended to run 
all the runs to create a more realistic example of a Zonation project, do the 
following:


```r
library(zonator)

setup.dir <- system.file("extdata/tutorial/basic", package = "zonator")

# Get all the bat-files
bat.files <- list.files(setup.dir, "\\.bat$", full.names = TRUE)
## 
## # Run all the runs
## for (bat.file in bat.files) {
##     run_bat(bat.file)
## }
## 
```


Now that the the actual runs have been executed, you can create a new zonator 
project based on existing results by doing the following:


```r
test.project <- create_zproject(setup.dir)
```


Individual variants (i.e. runs) can be extracted form the project:


```r
variant.1 <- getvariant(test.project, 1)
```


Using an index number such as `1` is one option, but you can also use the name
of the variant. By default, zonator will assign the name of bat-file used to run
the run as a name, without the ".bat" extensions of course. You can inspect all
the names in your project:


```r
names(test.project)
```

```
## [1] "do_01_core_area_zonation"        "do_02_additive_benefit_function"
## [3] "do_03_boundary_length_penalty"   "do_04_distribution_smoothing"   
## [5] "do_05_hierarchical_removal_mask"
```

