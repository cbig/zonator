if (!requireNamespace("zdat", quietly = TRUE)) {
  stop("zdat is needed for running tests. Please install it.",
       call. = FALSE)
}

context("Reading raster results for Zresults works")

options <- get_options()

test_that("Retrieving results rank raster works", {
  results.path <- system.file("extdata", "basic/01/01_out",
                              package = "zdat")
  correct.rank.raster <- raster(file.path(results.path,
                                          "01.rank.compressed.tif"))
  test.results <- new("Zresults", root = results.path)

  expect_identical(rank_raster(test.results), correct.rank.raster,
                   "Correct rank raster is not returned for Zresults")

  no.results.path <- file.path(options$setup.dir, "06/06_out")
  suppressWarnings(test.no.results <- new("Zresults", root = no.results.path))

  expect_warning(rank_raster(test.no.results))

})

context("Reading raster results for Zvariant works")

test_that("Retrieving variant rank raster works", {
  results.path <- system.file("extdata", "basic/01/01_out",
                              package = "zdat")
  correct.rank.raster <- raster(file.path(results.path,
                                          "01.rank.compressed.tif"))

  bat.file <- system.file("extdata", "basic", "01.bat",
                          package = "zdat")
  test.variant <- new("Zvariant", bat.file = bat.file)

  expect_identical(rank_raster(test.variant), correct.rank.raster,
                   "Correct rank raster is not returned for Zvariant")

  # Test with a variant with no results
  no.results.bat.file <- file.path(options$setup.dir,
                                   "06.batx")
  suppressWarnings(no.results.variant <- new("Zvariant",
                                             bat.file = no.results.bat.file))
  expect_warning(rank_raster(no.results.variant))

})


test_that("Jaccard coefficient is calculated correctly for 2 variants", {

  bat.file <- system.file("extdata", "basic", "01.bat",
                          package = "zdat")
  bat.file.ds <- system.file("extdata", "basic", "04.bat",
                             package = "zdat")

  # Variants 1 and 4
  results.variant <- new("Zvariant", bat.file = bat.file)
  results.variant.ds <- new("Zvariant", bat.file = bat.file.ds)

  # Get the rank rasters
  rank.1 <- rank_raster(results.variant)
  rank.4 <- rank_raster(results.variant.ds)

  # Range of values in x is [0, 1] so these should generate errors
  expect_error((j <- jaccard(rank.1, rank.4, x.min = -0.5)),
                info = "Wrong x.min value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, x.max = 1.1)),
               info = "Wrong x.max value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, x.min = 0.6, x.max = 0.5)),
               info = "x.min being smaller than x.max does not generate an error")

  # Range of values in y is [0, 1] so these should generate errors
  expect_error((j <- jaccard(rank.1, rank.4, y.min = -0.5)),
               info = "Wrong x.min value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, y.max = 1.1)),
               info = "Wrong x.max value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, y.min = 0.6, y.max = 0.5)),
               info = "x.min being smaller than x.max does not generate an error")

  # Check the Jaccard index values

  # No overlap at all
  expect_equal(jaccard(rank.1, rank.1, x.max = 0.1, y.min = 0.9), 0,
               info = "Non-overlapping regions have Jaccard index != 0")
  # Completet overlap
  expect_equal(jaccard(rank.1, rank.1, x.min = 0.8, y.min = 0.8), 1,
               info = "Completely overlapping regions have Jaccard index != 1")

  # Use pre-computed and hard-coded values
  expect_equal(jaccard(rank.1, rank.4, x.min = 0.9, y.min = 0.9), 0.1976978,
               tolerance = 0.0001,
               info = "Jaccard index value not what expected")

})
