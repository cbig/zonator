---
layout: post
title:  "Project creation"
date:   2013-12-05 18:53:45
---


## Create `zonator` project based on existing data

The following examples will use 
[the basic Zonation tutorial data](https://github.com/cbig/zonation-tutorial/tree/master/basic)
that is distributed with zonator package. This includes also the results of the
tutorial runs so there is no need to run the variants in order to inspect the
results. In case you *do* want to rerun the variants and you have Zonation
installed in your system do so by running the following code. Otherwise just
skip this code section.


{% highlight r %}
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
{% endhighlight %}


You can create a new zonator project based on existing results by doing the 
following:


{% highlight r %}
tutorial.project <- create_zproject(setup.dir)
{% endhighlight %}


zonator also includes a utility function `opendir()` which takes a zproject
object as an argument and opens the file system folder containing the setup
files:


{% highlight r %}
opendir(tutorial.project)
{% endhighlight %}


## Working with variants

Individual variants (i.e. runs) can be extracted form the project using an 
index number. `nvariants()` will tell how many variants are included in the 
project.


{% highlight r %}
nvariants(tutorial.project)
{% endhighlight %}



{% highlight text %}
## [1] 5
{% endhighlight %}



{% highlight r %}
variant.1 <- get_variant(tutorial.project, 1)
{% endhighlight %}


Using an index number such as `1` is one option, but you can also use the name
of the variant. By default, zonator will assign the name of bat-file used to run
the run as a name, without the ".bat" extensions of course. `names()` will print 
the names of all the variants. Name can also be used to extract a variant.


{% highlight r %}
names(tutorial.project)
{% endhighlight %}



{% highlight text %}
## [1] "01_core_area_zonation"        "02_additive_benefit_function" "03_boundary_length_penalty"  
## [4] "04_distribution_smoothing"    "05_hierarchical_removal_mask"
{% endhighlight %}



{% highlight r %}
variant.caz <- get_variant(tutorial.project, "01_core_area_zonation")
{% endhighlight %}


Each variant object is an instance of class `Zvariant` and have a suite of 
useful methods for dealing with data parsed from various Zonation input files.

### spp data

Zonation spp-file is one the mandatory input files that always needs to be in
place and thus all variants have one. When a new `Zvariant` instance is created
the associated spp file is automatically parsed into it. All the spp data 
(with group code column if available) can be retrieved using `sppdata()`:


{% highlight r %}
sppdata(variant.caz)
{% endhighlight %}



{% highlight text %}
##   weight alpha bqp bqp_p cellrem             filepath     name group
## 1      1  1.00   1     1       1 ../data/species1.tif species1     1
## 2      1  0.50   1     1       1 ../data/species2.tif species2     2
## 3      1  0.25   1     1       1 ../data/species3.tif species3     2
## 4      1  0.75   1     1       1 ../data/species4.tif species4     1
## 5      1  0.50   1     1       1 ../data/species5.tif species5     2
## 6      1  1.50   1     1       1 ../data/species6.tif species6     1
## 7      1  1.00   1     1       1 ../data/species7.tif species7     1
{% endhighlight %}


Feature names from the spp file/data can be accessed directly by using method
`featurenames()`.


{% highlight r %}
featurenames(variant.caz)
{% endhighlight %}



{% highlight text %}
## [1] "species1" "species2" "species3" "species4" "species5" "species6" "species7"
{% endhighlight %}


The generated names are not necessarily very informative and can be changed to
new values. Rememeber that the names need to be valid data frame column names
(zontor will try to fix these even if you don't).


{% highlight r %}
featurenames(variant.caz) <- c("Koala", "Masked.owl", "Powerful.owl", "Tiger.quoll", "Sooty.owl", "Squirrel.glider", "Yellow-bellied.glider")
featurenames(variant.caz)
{% endhighlight %}



{% highlight text %}
## [1] "Koala"                 "Masked.owl"            "Powerful.owl"          "Tiger.quoll"          
## [5] "Sooty.owl"             "Squirrel.glider"       "Yellow-bellied.glider"
{% endhighlight %}



{% highlight r %}
# Or all the spp data
sppdata(variant.caz)
{% endhighlight %}



{% highlight text %}
##   weight alpha bqp bqp_p cellrem             filepath                  name group
## 1      1  1.00   1     1       1 ../data/species1.tif                 Koala     1
## 2      1  0.50   1     1       1 ../data/species2.tif            Masked.owl     2
## 3      1  0.25   1     1       1 ../data/species3.tif          Powerful.owl     2
## 4      1  0.75   1     1       1 ../data/species4.tif           Tiger.quoll     1
## 5      1  0.50   1     1       1 ../data/species5.tif             Sooty.owl     2
## 6      1  1.50   1     1       1 ../data/species6.tif       Squirrel.glider     1
## 7      1  1.00   1     1       1 ../data/species7.tif Yellow-bellied.glider     1
{% endhighlight %}


### Groups

Notice that data frames returned by `sppdata()` in previous examples alreadt had
a column called "group". This is because all tutorial variants have groups 
enabled by deafault. If a variant doesn't use groups, then this column will be
missing.

Group identities in Zonation input file are coded with integer values. Method
`groups()` will return just this integer vector:


{% highlight r %}
groups(variant.caz)
{% endhighlight %}



{% highlight text %}
## [1] 1 2 2 1 2 1 1
{% endhighlight %}


Groups can also have more informative names attached to them by using method
`groupnames()`. The format for setting (mapping) group names is stricts and 
involves a named character vector in which (column) names correspond to integer
group codes and character elements the group names to be assigned:


{% highlight r %}
# By default, there are no group names
groupnames(variant.caz)
{% endhighlight %}



{% highlight text %}
## [1] NA
{% endhighlight %}



{% highlight r %}

# Contruct a group name mapping using a named character vector
labels <- c("mammals", "owls")
names(labels) <- c(1, 2)
groupnames(variant.caz) <- labels
groupnames(variant.caz)
{% endhighlight %}



{% highlight text %}
##         1         2 
## "mammals"    "owls"
{% endhighlight %}


Now group 1 is labeled "mammals" and group 2 is labeled "owls". Note that 
`sppdata()` has an optional argument `group.names` that can be set to `TRUE`
if group names are preferable to group codes (names have to be set first,
though).


{% highlight r %}
sppdata(variant.caz, group.names = TRUE)
{% endhighlight %}



{% highlight text %}
##   weight alpha bqp bqp_p cellrem             filepath                  name group.name
## 1      1  1.00   1     1       1 ../data/species1.tif                 Koala    mammals
## 2      1  0.50   1     1       1 ../data/species2.tif            Masked.owl       owls
## 3      1  0.25   1     1       1 ../data/species3.tif          Powerful.owl       owls
## 4      1  0.75   1     1       1 ../data/species4.tif           Tiger.quoll    mammals
## 5      1  0.50   1     1       1 ../data/species5.tif             Sooty.owl       owls
## 6      1  1.50   1     1       1 ../data/species6.tif       Squirrel.glider    mammals
## 7      1  1.00   1     1       1 ../data/species7.tif Yellow-bellied.glider    mammals
{% endhighlight %}

