context("Zresults creation")

test_that("Zresults is created correctly", {
  # We need a valid path to existing results
  results.path <- file.path(.options$output.dir, "01_core_area_zonation")
  invalid.results.path <- file.path(.options$output.dir, "xxx")
  
  # Create a new Zresults object
  test.results <- new("Zresults", root=results.path)
  
  expect_warning(new("Zresults", root=invalid.results.path))
  
  # Modified
  expect_true(any(class(test.results@modified) == "POSIXct"),
              "Test results object's slot 'modified' is not POSIXct")
  
  # Run info
  run.info.file <- file.path(results.path, "01_core_area_zonation.run_info.txt")
  expect_equal(run.info.file, test.results@run.info,
               "Test results object's run info file path incorrect")
  
  # Curves
  curves.file <- file.path(results.path, "01_core_area_zonation.curves.txt")
  correct.curves <- read_curves(curves.file)
  expect_identical(correct.curves, test.results@curves,
                   "Test results object's curves data incorrect")  
  
  # Group curves
  grp.curves.file <- file.path(results.path, "01_core_area_zonation.grp_curves.txt")
  correct.grp.curves <- read_grp_curves(grp.curves.file)
  expect_identical(correct.grp.curves, test.results@grp.curves,
                   "Test results object's group curves data incorrect") 
})