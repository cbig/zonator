context("Zproject creation")

# FIXME: there must a smarter way than then the relative path below...
tutorial.dir <- file.path("../../", get.tutorialdir())

# Using tutorial data, must be present!
test_that("Zonation tutorial data is available", {
  
  expect_true(file.exists(tutorial.dir), 
              "Tutorial directory not found in the root of the package.")
  
})