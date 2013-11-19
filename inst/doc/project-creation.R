
## ----run-variant-1, eval=(1:6), message=FALSE----------------------------
library(zonator)

setup.dir <- system.file("extdata/tutorial/basic", package="zonator")

# Get all the bat-files
bat.files <- list.files(setup.dir, "\\.bat$", full.names=TRUE)

# Run all the runs
for (bat.file in bat.files) {
  run_bat(bat.file)
}



## ----creat-zproject------------------------------------------------------
test.project <- create_zproject(setup.dir)


## ----extract-variant-----------------------------------------------------
nvariants(test.project)
variant.1 <- getvariant(test.project, 1)


## ----variants-by-name----------------------------------------------------
names(test.project)
variant.1 <- getvariant(test.project, "01_core_area_zonation")


