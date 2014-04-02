context("Comparing resulting rank rasters")

test_that("Jaccard coefficient is calculated correctly for 2 variants", {
 
  bat.file <- .options$bat.file
  bat.file.ds <- .options$bat.file.ds
  
  # Variants 1 and 4
  results.variant <- new("Zvariant", bat.file=bat.file)
  results.variant.ds <- new("Zvariant", bat.file=bat.file.ds)
  
  # Get the rank rasters
  rank.1 <- rank_raster(results.variant)
  rank.4 <- rank_raster(results.variant.ds)

  # Range of values in x is [0, 1] so these should generate errors
  expect_error((j <- jaccard(rank.1, rank.4, x.min=-0.5)),
                info="Wrong x.min value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, x.max=1.1)),
               info="Wrong x.max value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, x.min=0.6, x.max=0.5)),
               info="x.min being smaller than x.max does not generate an error")
  
  # Range of values in y is [0, 1] so these should generate errors
  expect_error((j <- jaccard(rank.1, rank.4, y.min=-0.5)),
               info="Wrong x.min value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, y.max=1.1)),
               info="Wrong x.max value does not generate an error")
  expect_error((j <- jaccard(rank.1, rank.4, y.min=0.6, y.max=0.5)),
               info="x.min being smaller than x.max does not generate an error")
  
  # Check the Jaccard index values
  
  # No overlap at all
  expect_equal(jaccard(rank.1, rank.1, x.max=0.1, y.min=0.9), 0,
               info="Non-overlapping regions have Jaccard index != 0")
  # Completet overlap
  expect_equal(jaccard(rank.1, rank.1, x.min=0.8, y.min=0.8), 1,
               info="Completely overlapping regions have Jaccard index != 1")
  
  # Use pre-computed and hard-coded values
  expect_equal(jaccard(rank.1, rank.4, x.min=0.9, y.min=0.9), 0.1976978, 
               tolerance=0.0001,
               info="Jaccard index value not what expected")
  
})
