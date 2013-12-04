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
  grp.curves.file <- file.path(results.path, 
                               "01_core_area_zonation.grp_curves.txt")
  correct.grp.curves <- read_grp_curves(grp.curves.file)
  expect_identical(correct.grp.curves, test.results@grp.curves,
                   "Test results object's group curves data incorrect") 
})

context("Zresults methods")

test_that("getting curves for individual features works", {

  results.path <- file.path(.options$output.dir, "01_core_area_zonation")
  test.results <- new("Zresults", root=results.path)
  
  curves.file <- file.path(results.path, "01_core_area_zonation.curves.txt")
  
  correct.curves <- read_curves(curves.file)
  expect_identical(correct.curves, curves(test.results),
                   "Method curves doesn't get he right curves data")
  
  # Only selected curves. These are cost, ave_pr, and feats 1, 3 and 4
  correct.selected.curves <- correct.curves[,c(1, 2, 4, 8, 10, 11)]
  # Test with col names
  #browser()
  expect_identical(correct.selected.curves, 
                   curves(test.results, cols=c("cost", "ave_pr", "f1",
                                               "f3", "f4")),
                   "Method curves doesn't return right data with col names") 
  # Test with col indexes
  expect_identical(correct.selected.curves, 
                   curves(test.results, cols=c(2, 4, 8, 10, 11)),
                   "Method curves doesn't return right data with col indexes")
  
  # Test for faulty col names
  
  expect_warning(curves(test.results, cols=c("cost", "ave_pr", "f1",
                                             "f3", "f4", "XXX")))
  suppressWarnings(expect_identical(correct.selected.curves, 
                   curves(test.results, cols=c("cost", "ave_pr", "f1",
                                               "f3", "f4", "XXX")),
                   "Method curves doesn't return right data with col names"))
  # Test for faulty indexes
  expect_warning(curves(test.results, cols=c(2, 4, 8, 10, 11, 200)))
  suppressWarnings(expect_identical(correct.selected.curves, 
                                    curves(test.results, 
                                           cols=c(2, 4, 8, 10, 11, 200)),
                  "Method curves doesn't return right data with indexes"))
  
})

test_that("getting curves for groups works", {
  
  results.path <- file.path(.options$output.dir, "01_core_area_zonation")
  test.results <- new("Zresults", root=results.path)
  
  grp.curves.file <- file.path(results.path, 
                               "01_core_area_zonation.grp_curves.txt")
  
  correct.grp.curves <- read_grp_curves(grp.curves.file)
  expect_identical(correct.grp.curves, curves(test.results, groups=TRUE),
                   "Method curves doesn't get he right group curves data")
  
  # Only selected curves. These are cost, ave_pr, and feats 1, 3 and 4
  correct.selected.grp.curves <- correct.grp.curves[,c(1, 2, 4, 8, 10, 11)]
  # Test with col names
  expect_identical(correct.selected.grp.curves, 
                   curves(test.results, groups=TRUE,
                          cols=c("cost", "mean.g1",  "min.g2",
                                 "max.g2", "w.mean.g2")),
                   "Method curves doesn't return right groups data with col names") 
  
  # Test with col indexes
  expect_identical(correct.selected.grp.curves, 
                   curves(test.results, groups=TRUE, cols=c(2, 4, 8, 10, 11)),
                   "Method curves doesn't return right groups data with col indexes")
  
  # Test for faulty col names
  expect_warning(curves(test.results, groups=TRUE, cols=c("cost", "mean.g1",  
                                                          "min.g2", "max.g2", 
                                                          "w.mean.g2", "XXX")))
  suppressWarnings(expect_identical(correct.selected.grp.curves, 
                                    curves(test.results, groups=TRUE,
                                           cols=c("cost", "mean.g1",  "min.g2",
                                                  "max.g2", "w.mean.g2", "XXX")),
                                    "Method curves doesn't return right groups data with col names"))
  # Test for faulty indexes
  expect_warning(curves(test.results, groups=TRUE, cols=c(2, 4, 8, 10, 11, 200)))
  suppressWarnings(expect_identical(correct.selected.grp.curves, 
                                    curves(test.results, groups=TRUE,
                                           cols=c(2, 4, 8, 10, 11, 200)),
                                    "Method curves doesn't return right groups data with indexes"))
  
})

test_that("performance levels are reported right", {
  
  results.path <- file.path(.options$output.dir, "01_core_area_zonation")
  test.results <- new("Zresults", root=results.path)
  
  curves.file <- file.path(results.path, "01_core_area_zonation.curves.txt")
  
  correct.curves <- read_curves(curves.file)
  
  # Test for individual feature for several thresholds
  # Get the right rows
  breaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  row.ids <- sapply(breaks, function(x) {which(correct.curves$pr_lost >= x)[1]})
  
  levels.all <- correct.curves[row.ids, c(1, 8:ncol(correct.curves))]
  row.names(levels.all) <- 1:nrow(levels.all)
  
  # Don't allow values < 0 or > 1 for pr.lost
  expect_error(performance(test.results, pr.lost=-0.1))
  expect_error(performance(test.results, pr.lost=1.1))
  
  # Get all features
  expect_identical(levels.all, performance(test.results, pr.lost=breaks),
                   "Method performance doesn't return levels right")
  
  # Get levels for a specific feature/features
  levels.feat1 <- data.frame(pr_lost=levels.all$pr_lost, f1=levels.all$f1)
  expect_identical(levels.feat1, performance(test.results, pr.lost=breaks,
                                             features="f1"),
                   "Method performance doesn't return level for 1 feature right")
  levels.feats1.5 <- data.frame(pr_lost=levels.all$pr_lost, f1=levels.all$f1,
                                f5=levels.all$f5)
  expect_identical(levels.feats1.5, performance(test.results, pr.lost=breaks,
                                                features=c("f1", "f5")),
                   "Method performance doesn't return levels for several features right")
  # Test for invalid feature name
  expect_warning(performance(test.results, pr.lost=breaks, features="fX1"))
  suppressWarnings(expect_true(is.na(performance(test.results, pr.lost=breaks, 
                                                 features="fX1"))))
})

test_that("performance levels are reported right", {
  
  results.path <- file.path(.options$output.dir, "01_core_area_zonation")
  test.results <- new("Zresults", root=results.path)
  
  curves.file <- file.path(results.path, "01_core_area_zonation.curves.txt")
  
  correct.curves <- read_curves(curves.file)
  
  # Test for individual feature for several thresholds
  # Get the right rows
  breaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  row.ids <- sapply(breaks, function(x) {which(correct.curves$pr_lost >= x)[1]})
  
  levels.all <- correct.curves[row.ids, c(1, 8:ncol(correct.curves))]
  row.names(levels.all) <- 1:nrow(levels.all)
  
  # Don't allow values < 0 or > 1 for pr.lost
  expect_error(performance(test.results, pr.lost=-0.1))
  expect_error(performance(test.results, pr.lost=1.1))
  
  # Get all features
  expect_identical(levels.all, performance(test.results, pr.lost=breaks),
                   "Method performance doesn't return levels right")
  
  # Get levels for a specific feature/features
  levels.feat1 <- data.frame(pr_lost=levels.all$pr_lost, f1=levels.all$f1)
  expect_identical(levels.feat1, performance(test.results, pr.lost=breaks,
                                             features="f1"),
                   "Method performance doesn't return level for 1 feature right")
  levels.feats1.5 <- data.frame(pr_lost=levels.all$pr_lost, f1=levels.all$f1,
                                f5=levels.all$f5)
  expect_identical(levels.feats1.5, performance(test.results, pr.lost=breaks,
                                                features=c("f1", "f5")),
                   "Method performance doesn't return levels for several features right")
  # Test for invalid feature name
  expect_warning(performance(test.results, pr.lost=breaks, features="fX1"))
  suppressWarnings(expect_true(is.na(performance(test.results, pr.lost=breaks, 
                                                 features="fX1"))))
})

