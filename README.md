[![Build Status](https://travis-ci.org/jlehtoma/zonator.png)](https://travis-ci.org/jlehtoma/zonator)

# zonator

`zonator` is a utility R package for dealing with various tasks related to
[Zonation](http://www.helsinki.fi/bioscience/consplan/software/Zonation/index.html) 
conservation prioritization software. The package started out as a collection
of helper scripts which have now been packages for anybody interested in using
a familiar tool (R) in setting up Zonation runs and managing Zonation results.

**NOTE that the scripts are strictly alpha quality. Do not expect any sort 
of stability at least before 0.2 release.**

Functions and classes found in `zonator` can be useful for following stages
of running a Zonation analysis.

1. Setting up a Zonation project and pre-processing files.
1. Running Zonation.
1. Post-processing (task such as results analysis and comparison, plotting etc).

For more detailed examples, see the [wiki](https://github.com/jlehtoma/zonator/wiki).

## Installing

1. Make sure you have package `devtools` installed:  
```
install.packages("devtools")
```  

2. Then install `zonator` directly from GitHub:  
```
install_github("zonator", "jlehtoma")
```  

## Developers

* Joona Lehtomäki <joona.lehtomaki@gmail.com>
