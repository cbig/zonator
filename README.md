[![](https://www.r-pkg.org/badges/version/zonator)](https://CRAN.R-project.org/package=zonator)
[![](http://cranlogs.r-pkg.org/badges/zonator)](https://CRAN.R-project.org/package=zonator)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![GitHub version](https://badge.fury.io/gh/cbig%2Fzonator.svg)](https://badge.fury.io/gh/cbig%2Fzonator)
[![Build Status](https://travis-ci.org/cbig/zonator.png)](https://travis-ci.org/cbig/zonator)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/cbig/zonator?branch=master&svg=true)](https://ci.appveyor.com/project/cbig/zonator)
[![codecov](https://codecov.io/gh/cbig/zonator/branch/master/graph/badge.svg)](https://codecov.io/gh/cbig/zonator)



# zonator

`zonator` is a utility R package for dealing with various tasks related to
Zonation conservation prioritization software. The package started out as a collection
of helper scripts which have now been packaged for anybody interested in using
a familiar tool (R) in setting up Zonation runs and managing Zonation results.

Functions and classes found in `zonator` can be useful for the following stages
of running a Zonation analysis.

1. Setting up a Zonation project and pre-processing files ([examples](https://cbig.github.io/zonator/articles/zonator-project.html)).
1. Running Zonation.
1. Post-processing, results analysis and comparison, and plotting ([examples](https://cbig.github.io/zonator/articles/zonator-results.html)).

## Installation

### Release version

**NOTE: As of 2019-03-15 `zonator` isn't on CRAN. Hopefully it will get back there soon, but in the meantime you can install it directly from Github:**

You can install the latest release version from CRAN:

```
install.packages("remotes")
remotes::install_github("cbig/zonator")
```



### Development version

Alternatively, you can install the latest (potentially unstable) development version from Github.

1. Make sure you have package `remotes` installed and loaded in R:  

```
# Open R prompt or RStudio and type the following on the command line
install.packages("remotes")
```  

2. Then install `zonator` directly from GitHub by running the following line in R:  

```
remotes::install_github("cbig/zonator", ref = "develop", build_vignettes = TRUE)
```  

## Examples

Some examples are provided by the package vignettes. You can view these after installing the package by:

```
vignette("zonator-project")
```

and 

```
vignette("zonator-results")
```

## Developers

* Joona Lehtomäki <joona.lehtomaki@gmail.com>
* Aija Kukkala <aija.kukkala@helsinki.fi>
