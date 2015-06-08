## ----setup, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------
options(width=160)

## ----setup-objects, message=FALSE-----------------------------------------------------------------------------------------------------------------------------
library(zonator)

# Start by creating a project using the tutorial data
setup.dir <- system.file("extdata/tutorial/basic", package="zonator") 
tutorial.project <- create_zproject(setup.dir)

# Get a specific variant
variant.caz <- get_variant(tutorial.project, "01_core_area_zonation")

# Let's also rename the features and groups while we're at it
featurenames(variant.caz) <- c("Koala", "Masked.owl", "Powerful.owl", 
                             "Tiger.quoll", "Sooty.owl", "Squirrel.glider",
                             "Yellow-bellied.glider")

groupnames(variant.caz) <- c("1"="mammals", "2"="owls")

## ----has-results, message=FALSE-------------------------------------------------------------------------------------------------------------------------------
has_results(variant.caz)

## ----get-results----------------------------------------------------------------------------------------------------------------------------------------------
results.caz <- results(variant.caz)

## ----curves---------------------------------------------------------------------------------------------------------------------------------------------------
# Get feature-specific curves
feature.curves.caz <- curves(results.caz)
head(feature.curves.caz)

# Note! Using curves() variant.caz will work as well
feature.curves.caz <- curves(variant.caz)
head(feature.curves.caz)

# Get just 2 species
duo.curves.caz <- curves(results.caz, cols=c("Koala", "Tiger.quoll"))
head(duo.curves.caz)

## ----grp-curves-----------------------------------------------------------------------------------------------------------------------------------------------
# Get group-specific curves
feature.grp.curves.caz <- curves(results.caz, groups=TRUE)
head(feature.grp.curves.caz)

# Get all the aggregate stats for owls
owls.grp.curves.caz <- curves(results.caz, groups=TRUE, 
                              cols=c("min.owls", "mean.owls", "max.owls",
                                     "w.mean.owls", "ext2.owls"))
head(owls.grp.curves.caz)

## ----curves-range---------------------------------------------------------------------------------------------------------------------------------------------

# Get performance values for 2 species for a range between 60% and 75% of 
# landscape lost 
duo.curves.caz.range <- curves(results.caz, cols=c("Koala", "Tiger.quoll"),
                               lost.lower=0.6, lost.upper=0.75)
head(duo.curves.caz.range)
tail(duo.curves.caz.range)

## ----perfomance-----------------------------------------------------------------------------------------------------------------------------------------------
# Define the levels of landscape lost
lost.levels <- c(0.25, 0.50, 0.75)
# Notice that performance operates with Zresults-object
performance(results.caz, lost.levels)

# Same can be done for groups
performance(results.caz, lost.levels, groups=TRUE)

## ----perfomance-melted----------------------------------------------------------------------------------------------------------------------------------------
# Define the levels of landscape lost
lost.levels <- c(0.25, 0.50, 0.75)
# Notice that performance operates with Zresults-object
performance(results.caz, lost.levels, melted=TRUE)

## ----mask-variant---------------------------------------------------------------------------------------------------------------------------------------------
variant.mask <- get_variant(tutorial.project, "05_hierarchical_removal_mask")

featurenames(variant.mask) <- c("Koala", "Masked.owl", "Powerful.owl", 
                             "Tiger.quoll", "Sooty.owl", "Squirrel.glider",
                             "Yellow-bellied.glider")
labels <- c("mammals", "owls")
names(labels) <- c(1, 2)
groupnames(variant.mask) <- labels

results.mask <- results(variant.mask)
feature.curves.mask <- curves(results.mask)
feature.grp.curves.mask <- curves(results.mask, groups=TRUE)

## ----curves-plotting-1, fig.width=10--------------------------------------------------------------------------------------------------------------------------
plot(feature.curves.caz)

## ----curves-plotting-2, fig.width=10--------------------------------------------------------------------------------------------------------------------------
plot(feature.curves.caz, invert.x=TRUE, main="All features")

## ----curves-plotting-3, fig.width=10--------------------------------------------------------------------------------------------------------------------------
plot(feature.curves.caz, invert.x=TRUE, main="Just few features", 
     subs=c('Yellow-bellied.glider', 'Sooty.owl'), mean=TRUE)

## ----curves-plotting-4, fig.width=10--------------------------------------------------------------------------------------------------------------------------
plot(feature.grp.curves.caz, invert.x=TRUE, main="All groups")

## ----curves-plotting-5, fig.width=10--------------------------------------------------------------------------------------------------------------------------
plot(feature.grp.curves.caz, invert.x=TRUE, main="All groups", subs=c('owls'))

## ----curves-plotting-6, fig.width=10--------------------------------------------------------------------------------------------------------------------------
plot(feature.grp.curves.caz, invert.x=TRUE, main="All groups", monochrome=TRUE)

## ----curves-plotting-7, fig.width=10--------------------------------------------------------------------------------------------------------------------------
plot(feature.grp.curves.mask, invert.x=TRUE, main="All groups", mean=TRUE, 
     min=TRUE, max=TRUE)

## ----perfomance-hist, fig.width=10----------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
# NOTE: we're using melted=TRUE here
occr.remaining <- performance(results.caz, c(0.9, 0.95, 0.98), melted=TRUE)
ggplot(occr.remaining, aes(x = factor(feature), y = perf.levels,
                           fill=factor(pr.lost))) + 
  geom_bar(position="dodge", stat = "identity")


## ----rank-raster-1, fig.width=10------------------------------------------------------------------------------------------------------------------------------
library(raster)
rank.caz <- rank_raster(results.caz)
# Let's use a spectral color scheme designed for Zonation rank maps
leg <- zlegend("spectral")
plot(rank.caz, breaks=leg$values, col=leg$colors)

## ----rank-raster-2, fig.height=20-----------------------------------------------------------------------------------------------------------------------------
# RasterStack is returned
all.ranks <- rank_rasters(tutorial.project)
plot(all.ranks, breaks=leg$values, col=leg$colors, nc=1)

## ----rank-raster-3, fig.width=10------------------------------------------------------------------------------------------------------------------------------
pairs(stack(all.ranks[[1:3]]), method="kendall")

## ----rank-raster-4, fig.width=10------------------------------------------------------------------------------------------------------------------------------
pas.file <- system.file("extdata/tutorial/data", "mask_rs.tif", 
                        package="zonator")
pas <- raster(pas.file)
# Re-class zeros as NA
pas[pas == 0] <- NA
# Plot the histogram of rank.caz within mask raster
plot_hist(x=rank.caz, pas, add.median=TRUE)

## ----rank-rasterVis-1, fig.width=10, message=FALSE------------------------------------------------------------------------------------------------------------
library(rasterVis)
# Note that breaks in the following are different from the previous maps
levelplot(rank.caz, FUN.margin=mean, 
          par.settings=rasterTheme(region=leg$colors))

