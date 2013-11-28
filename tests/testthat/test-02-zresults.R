context("Zresults creation")

test_that("Zvariant with results is created correctly", {
  # Let's create an empty Zvariant with no results and attach results to 
  # that later. This avoids creating Zresults as parts of Zvariants
  no.results.bat.file <- file.path(.options$setup.dir, 
                                   "06_dummy_for_testing.batx")
  no.results.variant <- new("Zvariant", bat.file=no.results.bat.file) 
}