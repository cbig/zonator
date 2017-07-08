library(devtools)

build_dir <- "dist"

devtools::document()
devtools::build_vignettes()

if (!file.exists(build_dir)) {
  dir.create(build_dir)
}

build_file <- devtools::build(path = build_dir, vignettes = FALSE, manual = TRUE)

system(paste("R CMD check --as-cran ", build_file))
