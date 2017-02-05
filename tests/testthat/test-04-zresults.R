context("Zresults creation")

test_that("Zresults is created correctly", {
  # We need a valid path to existing results
  results.path <- .options$results.dir
  invalid.results.path <- file.path(.options$setup.dir, "xxx")

  # Create a new Zresults object
  test.results <- new("Zresults", root=results.path)

  expect_warning(new("Zresults", root=invalid.results.path))

  # Modified
  expect_true(any(class(test.results@modified) == "POSIXct"),
              "Test results object's slot 'modified' is not POSIXct")

  # Run info
  run.info.file <- file.path(results.path, "01.run_info.txt")
  expect_identical(run.info.file, test.results@run.info,
                   "Test results object's run info file path incorrect")

  # Curves
  curves.file <- file.path(results.path, "01.curves.txt")
  correct.curves <- read_curves(curves.file)
  expect_identical(correct.curves, test.results@curves,
                   "Test results object's curves data incorrect")

  # Group curves
  grp.curves.file <- file.path(results.path,
                               "01.grp_curves.txt")
  correct.grp.curves <- read_grp_curves(grp.curves.file)
  expect_identical(correct.grp.curves, test.results@grp.curves,
                   "Test results object's group curves data incorrect")
})

test_that("Zresults is created correctly when there are no results", {

  no.results.path <- file.path(.options$output.dir, "06")
  suppressWarnings(test.no.results <- new("Zresults", root = no.results.path))

})

context("Zresults methods")

test_that("getting curves for individual features works", {

  results.path <- .options$results.dir
  test.results <- new("Zresults", root=results.path)

  curves.file <- file.path(results.path, "01.curves.txt")

  correct.curves <- read_curves(curves.file)
  expect_identical(correct.curves, curves(test.results),
                   "Method curves doesn't get he right curves data")

  # Only selected curves. These are cost, ave_pr, and feats 1, 3 and 4
  correct.selected.curves <- correct.curves[,c(1, 2, 4, 8, 10, 11)]
  correct.selected.curves <- new("ZCurvesDataFrame", correct.selected.curves,
                                 is.feature=c(FALSE, FALSE, FALSE,
                                              TRUE, TRUE, TRUE))
  # Test with col names
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

  # Test for a given range
  correct.selected.curves <- correct.curves[which(correct.curves$pr_lost <= 0.1),]
  correct.selected.curves <- new("ZCurvesDataFrame", correct.selected.curves,
                                 is.feature=c(rep(FALSE, 7), rep(TRUE, ncol(correct.curves) - 7)))
  expect_identical(correct.selected.curves,
                   curves(test.results, lost.upper=0.1),
                   "Method curves doesn't return right data with pr_lost <= 0.1")

  # Test for a given range
  correct.selected.curves <- correct.curves[which(correct.curves$pr_lost >= 0.1),]
  correct.selected.curves <- new("ZCurvesDataFrame", correct.selected.curves,
                                 is.feature=c(rep(FALSE, 7), rep(TRUE, ncol(correct.curves) - 7)))

  expect_identical(correct.selected.curves,
                   curves(test.results, lost.lower=0.1),
                   "Method curves doesn't return right data with pr_lost >= 0.1")

  correct.selected.curves <- correct.curves[which(correct.curves$pr_lost >= 0.1 & correct.curves$pr_lost <= 0.4),]
  correct.selected.curves <- new("ZCurvesDataFrame", correct.selected.curves,
                                 is.feature=c(rep(FALSE, 7), rep(TRUE, ncol(correct.curves) - 7)))

  expect_identical(correct.selected.curves,
                   curves(test.results, lost.lower=0.1, lost.upper=0.4),
                   "Method curves doesn't return right data with 0.1 <= pr_lost <= 0.4")

  expect_error(curves(test.results, lost.lower=0.4, lost.upper=0.1),
               info="curves() should throw error on invalid range")
  expect_error(curves(test.results, lost.lower=-0.1),
               info="curves() should throw error on invalid lower range")
  expect_error(curves(test.results, lost.lower=1.0),
               info="curves() should throw error on invalid lower range")
  expect_error(curves(test.results, lost.upper=1.1),
               info="curves() should throw error on invalid upper range")
  expect_error(curves(test.results, lost.upper=0.0),
               info="curves() should throw error on invalid upper range")
})

test_that("getting curves for groups works", {

  results.path <- .options$results.dir
  test.results <- new("Zresults", root=results.path)

  grp.curves.file <- file.path(results.path, "01.grp_curves.txt")

  correct.grp.curves <- read_grp_curves(grp.curves.file)
  expect_identical(correct.grp.curves, curves(test.results, groups=TRUE),
                   "Method curves doesn't get he right group curves data")

  # Only selected curves. These are cost, ave_pr, and feats 1, 3 and 4
  inds <- c(1, 2, 4, 8, 10, 11)
  correct.selected.grp.curves <- correct.grp.curves[,inds]
  correct.selected.grp.curves <- new("ZGroupCurvesDataFrame",
                                     correct.selected.grp.curves,
                                     is.group=c(FALSE, FALSE, TRUE,
                                                  TRUE, TRUE, TRUE))
  # Test with col names
  expect_identical(correct.selected.grp.curves,
                   curves(test.results, groups=TRUE,
                          cols=c("cost", "mean.group1",  "min.group2",
                                 "max.group2", "w.mean.group2")),
                   "Method curves doesn't return right groups data with col names")

  # Test with col indexes
  expect_identical(correct.selected.grp.curves,
                   curves(test.results, groups=TRUE, cols=c(2, 4, 8, 10, 11)),
                   "Method curves doesn't return right groups data with col indexes")

  # Test for faulty col names
  expect_warning(curves(test.results, groups=TRUE, cols=c("cost", "mean.group1",
                                                          "min.group2", "max.group2",
                                                          "w.mean.group2", "XXX")))
  suppressWarnings(expect_identical(correct.selected.grp.curves,
                                    curves(test.results, groups=TRUE,
                                           cols=c("cost", "mean.group1",  "min.group2",
                                                  "max.group2", "w.mean.group2", "XXX")),
                                    "Method curves doesn't return right groups data with col names"))
  # Test for faulty indexes
  expect_warning(curves(test.results, groups=TRUE, cols=c(2, 4, 8, 10, 11, 200)))
  suppressWarnings(expect_identical(correct.selected.grp.curves,
                                    curves(test.results, groups=TRUE,
                                           cols=c(2, 4, 8, 10, 11, 200)),
                                    "Method curves doesn't return right groups data with indexes"))

})

test_that("performance levels are reported right for individual features", {

  results.path <- .options$results.dir
  test.results <- new("Zresults", root=results.path)

  curves.file <- file.path(results.path, "01.curves.txt")

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
  expect_equivalent(levels.all, performance(test.results, pr.lost=breaks),
                   "Method performance doesn't return levels right")
  # Get levels for a specific feature/features
  levels.feat1 <- data.frame(pr_lost=levels.all$pr_lost, f1=levels.all$f1)
  expect_equivalent(levels.feat1, performance(test.results, pr.lost=breaks,
                                             features="f1"),
                   "Method performance doesn't return level for 1 feature right")

  levels.feats1.5 <- data.frame(pr_lost=levels.all$pr_lost, f1=levels.all$f1,
                                f5=levels.all$f5)
  expect_equivalent(levels.feats1.5, performance(test.results, pr.lost=breaks,
                                                features=c("f1", "f5")),
                   "Method performance doesn't return levels for several features right")

  # Test for invalid feature name
  expect_warning(performance(test.results, pr.lost=breaks, features="fX1"))
  suppressWarnings(expect_true(is.na(performance(test.results, pr.lost=breaks,
                                                 features="fX1"))))
})

test_that("performance levels are reported right for groups", {

  results.path <- .options$results.dir
  test.results <- new("Zresults", root=results.path)

  curves.file <- file.path(results.path, "01.grp_curves.txt")

  correct.curves <- read_grp_curves(curves.file)

  # Test for individual feature for several thresholds
  # Get the right rows
  breaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  row.ids <- sapply(breaks, function(x) {which(correct.curves$pr_lost >= x)[1]})

  levels.all <- correct.curves[row.ids, c(1, 3:ncol(correct.curves))]
  row.names(levels.all) <- 1:nrow(levels.all)

  # Get all features
  expect_equivalent(levels.all, performance(test.results, pr.lost=breaks,
                                           groups=TRUE),
                   "Method performance doesn't return group levels right")

  # Get levels for a specific feature/features
  levels.grp1 <- data.frame(pr_lost=levels.all$pr_lost,
                            mean.g1=levels.all$mean.group1)
  expect_equivalent(levels.grp1, performance(test.results, pr.lost=breaks,
                                             features="group1", groups=TRUE),
                   "Method performance doesn't return level for 1 group right")
  levels.grps1.2 <- data.frame(pr_lost=levels.all$pr_lost,
                               mean.g1=levels.all$mean.group1,
                               mean.g2=levels.all$mean.group2)
  expect_equivalent(levels.grps1.2, performance(test.results, pr.lost=breaks,
                                               features=c("group1", "group2"),
                                               groups=TRUE),
                   "Method performance doesn't return levels for several groups right")
})

test_that("featurenanmes are reported right", {

  results.path <- .options$results.dir
  test.results <- new("Zresults", root=results.path)

  correct.names <- c("f1", "f2", "f3", "f4", "f5", "f6", "f7")

  expect_identical(correct.names, featurenames(test.results),
                   "Method featurenames doesn't return the right feature names")

})

test_that("Retrieving results output directory works", {
  results.path <- .options$results.dir
  test.results <- new("Zresults", root=results.path)

  expect_identical(outdir(test.results), results.path,
                   "outdir() does not return the correct path for Zresults")

})

test_that("Reading feature info works", {
  results.path <- .options$results.dir

  correct.col.names <- c("Weight", "DistSum", "IGRetained",
                         "TviolationFractRem", "DistrMeanX", "DistMeanY",
                         "MapFileName")
  correct.features.info <- read.table(file.path(results.path,
                                                "01.features_info.txt"),
                                      sep = "\t", skip = 2, as.is = TRUE,
                                      col.names = correct.col.names)

  test.results <- new("Zresults", root = results.path)

  expect_identical(features_info(test.results), correct.features.info,
                   "Correct features info is not returned for Zresults")

  no.results.path <- file.path(.options$setup.dir, "06/06_out")
  suppressWarnings(test.no.results <- new("Zresults", root = no.results.path))

  expect_warning(features_info(test.no.results))

})

test_that("Reading cost data works", {
  results.path <- .options$results.dir
  curves.file <- file.path(results.path, "01.curves.txt")

  correct.cost_data <- read.table(curves.file, skip = 1, as.is = TRUE)[,1:2]
  names(correct.cost_data) <- c("pr_lost", "cost")

  test.results <- new("Zresults", root = results.path)

  expect_identical(cost(test.results), correct.cost_data,
                   "Correct cost data is not returned for Zresults")

  no.results.path <- file.path(.options$setup.dir, "06/06_out")
  suppressWarnings(test.no.results <- new("Zresults", root = no.results.path))

  expect_warning(cost(test.no.results))

})
