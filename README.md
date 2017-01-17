[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/cbig/zonator.png)](https://travis-ci.org/cbig/zonator)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/zonator)](http://www.r-pkg.org/pkg/zonator)

# zonator

`zonator` is a utility R package for dealing with various tasks related to
[Zonation](http://www.helsinki.fi/bioscience/consplan/software/Zonation/index.html) 
conservation prioritization software. The package started out as a collection
of helper scripts which have now been packages for anybody interested in using
a familiar tool (R) in setting up Zonation runs and managing Zonation results.

Functions and classes found in `zonator` can be useful for following stages
of running a Zonation analysis.

1. Setting up a Zonation project and pre-processing files ([examples](http://cbig.github.io/zonator/vignettes/zonator-project.html)).
1. Running Zonation.
1. Post-processing, results analysis and comparison, and plotting ([examples](http://cbig.github.io/zonator/vignettes/zonator-results.html)).

## Installation

1. Make sure you have package `devtools` installed and loaded in R:  
```
# Open R prompt or RStudio and type the following on the command line
install.packages("devtools")
```  

2. Then install `zonator` directly from GitHub by running the following line in R:  
```
install_github("cbig/zonator")
```  

## Examples

Some examples are provided by the package vignettes. You can view these after installing the package by:

```
vignette("zonator-project")
vignette("zonator-results")
```

## Developers

* Joona Lehtomäki <joona.lehtomaki@gmail.com>
* Aija Kukkala <aija.kukkala@helsinki.fi>
