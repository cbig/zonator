## ----setup, echo=FALSE------------------------------------------------------------------------------------------------
options(width = 120)

## ----create-new-init, message=FALSE,warning=FALSE---------------------------------------------------------------------
library(zonator)

# Create a (temporary) path to a new project. Basename component of this path
# will also be the name of the project.
project_path <- file.path(tempdir(), "test_project")

# Define the names of the variants within the project
variant_names <- c("01_variant", "02_variant", "03_variant")

## ----create-new-01, message=FALSE,warning=FALSE,eval=FALSE------------------------------------------------------------
#  # Create a new project from scratch. Note that since we do not provide any
#  # template file paths (for the .spp and .dat files), the templates shipped
#  # with the tutorial data will be used.
#  new_project <- create_zproject(root = project_path, variants = variant_names)

## ----create-new-02, message=FALSE,warning=FALSE,eval=FALSE------------------------------------------------------------
#  # Directory containing the input raster files
#  input_raster_dir <- system.file("extdata/tutorial/data", package="zonator")
#  new_project <- create_zproject(root = project_path, variants = variant_names,
#                                 spp_template_dir = input_raster_dir,
#                                 spp_file_pattern = "^species[0-9].tif$")

## ----create-new-03, message=FALSE,warning=FALSE,eval=FALSE------------------------------------------------------------
#  new_project <- create_zproject(root = project_path, variants = variant_names,
#                                 spp_template_dir = input_raster_dir,
#                                 spp_file_pattern = "^species[0-9].tif$",
#                                 weight = c(1, 1, 1, 2, 3, 2, 1))

## ----run-variant-1, eval=(1:6), message=FALSE-------------------------------------------------------------------------
library(zonator)

setup.dir <- system.file("extdata/tutorial/basic", package="zonator")

# Get all the bat-files
bat.files <- list.files(setup.dir, "\\.bat$", full.names=TRUE)

# Run all the runs
#for (bat.file in bat.files) {
#  run_bat(bat.file)
#}


## ----creat-zproject, message=FALSE------------------------------------------------------------------------------------
tutorial.project <- create_zproject(setup.dir)

## ----opendir, eval=FALSE----------------------------------------------------------------------------------------------
#  opendir(tutorial.project)

## ----extract-variant--------------------------------------------------------------------------------------------------
nvariants(tutorial.project)
variant.1 <- get_variant(tutorial.project, 1)

## ----variants-by-name-------------------------------------------------------------------------------------------------
names(tutorial.project)
variant.caz <- get_variant(tutorial.project, "01_core_area_zonation")

## ----sppdata----------------------------------------------------------------------------------------------------------
sppdata(variant.caz)

## ----nfeatures--------------------------------------------------------------------------------------------------------
nfeatures(variant.caz)

## ----sppdata-weights--------------------------------------------------------------------------------------------------
# Note that all biodiversity features (species) have an equal weight of 1
sppweights(variant.caz)

## ----sppdata-names-2--------------------------------------------------------------------------------------------------
featurenames(variant.caz)

## ----sppdata-names-1--------------------------------------------------------------------------------------------------
featurenames(variant.caz) <- c("Koala", "Masked.owl", "Powerful.owl", 
                             "Tiger.quoll", "Sooty.owl", "Squirrel.glider",
                             "Yellow-bellied.glider")
featurenames(variant.caz)
# Or all the spp data
sppdata(variant.caz)

## ----groups-----------------------------------------------------------------------------------------------------------
groups(variant.caz)

## ----group-names-1----------------------------------------------------------------------------------------------------
# By default, generic group names are used
groupnames(variant.caz)

## ----group-names-2----------------------------------------------------------------------------------------------------
# Construct a group name mapping using a named character vector
groupnames(variant.caz) <- c("1"="mammals", "2"="owls")
groupnames(variant.caz)

## ----sppdata-group-names----------------------------------------------------------------------------------------------
sppdata(variant.caz, group.names=TRUE)

## ----changing-groups-1------------------------------------------------------------------------------------------------
sppdata(variant.caz)

## ----changing-groups-2------------------------------------------------------------------------------------------------
groups(variant.caz) <- c(1, 2, 3, 1, 2, 1, 1)
# Check the group ID codes
groups(variant.caz)

## ----changing-groups-3------------------------------------------------------------------------------------------------
# Using generic group names after the group IDs have been changed
groupnames(variant.caz)
# Construct a new group names mapping
groupnames(variant.caz) <- c("1"="mammals", "2"="big.owls", "3"="small.owls")
groupnames(variant.caz)

## ----changing-groups-4------------------------------------------------------------------------------------------------
sppdata(variant.caz, group.names=TRUE)

