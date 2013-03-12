# zonator

`zonator` is a utility R package for dealing with various tasks related to
[Zonation](http://www.helsinki.fi/bioscience/consplan/software/Zonation/index.html) 
conservation prioritization software. The package started out as a collection
of helper scripts which have now been packages for anybody interested in using
a familiar tool (R) in setting up Zonation runs and managing Zonation results.

**NOTE that the scripts are strictly alpha quality. Do not expect any sort 
of stability at least before 0.5 release.**

## Objectives

Functions and classes found in `zonator` can be useful for following stages
of running a Zonation analysis.

### 1. Pre-processing
### 2. Running Zonation
### 3. Post-processing

#### Results comparison

#### Plotting

## Installing

1. Make sure you have package `devtools` installed:  
```r
install.packages("devtools")
```
2. Then install `zonator` directly from GitHub:
```r
install_github("zonator", "jlehtoma")
```

## Developers

* Joona Lehtomäki <>