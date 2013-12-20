[![Build Status](https://travis-ci.org/cbig/zonator.png)](https://travis-ci.org/cbig/zonator)
[![Stories in Ready](https://badge.waffle.io/cbig/zonator.png?label=ready)](http://waffle.io/cbig/zonator)

# zonator

`zonator` is a utility R package for dealing with various tasks related to
[Zonation](http://www.helsinki.fi/bioscience/consplan/software/Zonation/index.html) 
conservation prioritization software. The package started out as a collection
of helper scripts which have now been packages for anybody interested in using
a familiar tool (R) in setting up Zonation runs and managing Zonation results.

**NOTE that the scripts are strictly alpha quality. Do not expect any sort 
of stability at least before 0.5 release.**

Functions and classes found in `zonator` can be useful for following stages
of running a Zonation analysis.

1. Setting up a Zonation project and pre-processing files.
1. Running Zonation.
1. Post-processing (task such as results analysis and comparison, plotting etc).

## Installation

1. Make sure you have package `devtools` installed and loaded in R:  
```
# Open R prompt or RStudio and type the following on the command line
install.packages("devtools")
```  

2. Then install `zonator` directly from GitHub by running the following line in R:  
```
install_github("zonator", "cbig")
```  

## Examples

Some examples are provided by the package vignettes. You can view these after installing the package by:

```
vignette("zonator-project")
vignette("zonator-results")
```

Content of these vignettes is also avaible in [zonator-wiki](https://github.com/cbig/zonator/wiki).

## Developers

* Joona Lehtomäki <joona.lehtomaki@gmail.com>
