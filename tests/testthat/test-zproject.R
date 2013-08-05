context("Zproject creation")

tutorial.dir <- system.file("extdata", "zonation-tutorial", package="zonator")
bat.file <- system.file("extdata/zonation-tutorial", "do_abf.bat", 
                        package="zonator")

test_that("Zonation tutorial data is available", {
  
  expect_true(file.exists(bat.file), 
              "Tutorial directory not found in the root of the package.")
  
})