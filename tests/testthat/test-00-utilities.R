context("Utilities")

options <- get_options()

test_that("name checking works", {

  valid.names <- c("foo", "bar", "spam", "eggs")
  # Pass
  expect_identical(valid.names, check_names(valid.names),
                   "Truly valid names not accepted by check_names()")

  invalid.names <- c("foo", "foo", "spam", "eggs")
  # Give warning
  expect_warning(check_names(invalid.names),
                 info="check_names() does not throw a warning when there are duplicate names")

  valid.names <- c(11, 12, 13, 16)
  # Pass
  expect_identical(as.character(valid.names), check_names(valid.names))

  invalid.names <- c(11, 12, 13, 13, 12)
  # Give warning
  expect_warning(check_names(invalid.names),
                 info="check_names() does not throw a warning when there are duplicate names")

  valid.names <- c(11, 12, 13, 16)
  # Pass
  expect_identical(as.character(valid.names), check_names(valid.names))

  invalid.names <- c(11, 12, 13, 13, 12)
  # Fail
  expect_warning(check_names(invalid.names),
               info="check_names() does not throw a warning when there are duplicate names")

  invalid.names <- c("foo", "bar", "", "eggs")
  # Fail
  expect_error(check_names(invalid.names),
               info="check_names() does not throw an error when there are empty names")

  valid.names <- c("foo.bar", "bar", "spam", "eggs")
  invalid.names <- c("foo bar", "bar", "spam", "eggs")
  # Pass
  expect_warning(check_names(invalid.names),
                 info="check_names() should throw a warning when items have whitespaces")
  suppressWarnings(expect_identical(valid.names, check_names(invalid.names),
                                    "check_names() does not deal with whitespace in correct way"))

})

test_that("index mapping works", {
  library(ggplot2)
  data(diamonds)

  correct.inds <- c(1, 3, 4, 6)
  expect_identical(map_indexes(c("carat", "color", "clarity", "table"),
                               names(diamonds)),
               correct.inds,
               "map_indexes() does not return the right index values with names")

  expect_warning(map_indexes(c("carat", "color", "clarity", "table", "XXX"),
                             names(diamonds)),
                 info="map_indexes() does not warn about missing header name")

  suppressWarnings(expect_true(is.null(map_indexes(c("XXX"), names(diamonds))),
              "map_indexes() does not return NULL when no names are found"))

  expect_identical(map_indexes(correct.inds,
                           1:ncol(diamonds)),
                   correct.inds,
                   "map_indexes() does not return the right index values with indexes")
  })

test_that("path checking works", {
  # This file will actually exist
  path1 <- tempfile(fileext = ".txt")
  path2 <- tempfile(fileext = ".txt")
  # Needed for Windows compatability
  path1 <- gsub("\\\\", "/", path1)
  path2 <- gsub("\\\\", "/", path2)
  file.name1 <- basename(path1)
  file.name2 <- basename(path2)
  dir.name1 <- dirname(path1)
  dir.name2 <- paste0(dirname(path2), "fhasjkldfhlru")
  # Create only the first file path
  file.create(path1)

  expect_identical(path1, check_path(path1),
                   "check_path() does not return a valid path with full path")
  expect_identical(path1, check_path(file.name1, dir.name1, require.file=TRUE),
                   "check_path() does not return a valid path with valid file and dir names")
  expect_identical(dir.name1, check_path(file.name2, dir.name1),
                   "check_path() does not return only valid parent dir")

  expect_error(check_path(file.name1),
               info="check_path() should throw error if only file name is provided")
  expect_error(check_path(file.name2, dir.name2),
               info="check_path() should throw error if missing file and dir names are provided")

  # Test relative path normalization
  # This dir doesn't actually exist
  deep.dir <- file.path(dir.name1, "kfsjlkdfjl", "adfhjklf")
  rel.file <- file.path("..", "..", file.name1)

  expect_identical(path1, check_path(rel.file, deep.dir),
                   "check_path() does not normalize relative path correctly")

  deep.dir2 <- file.path(dir.name1, "kfsjlkdfjl")
  rel.file2 <- file.path("..", file.name1)

  expect_identical(path1, check_path(rel.file2, deep.dir2),
                   "check_path() does not normalize relative path correctly")

  unlink(path1)
  unlink(path2)
})

test_that("getting color schemes works", {

  z_colors_spectral <- list(values=c(0.0, 0.2, 0.5, 0.75, 0.9, 0.95, 0.98, 1.0),
                            labels=c("0.00-0.20", "0.20-0.50", "0.50-0.75",
                                     "0.75-0.90", "0.90-0.95", "0.95-0.98",
                                     "0.98-1.00"),
                            colors=c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf",
                                     "#fdc980", "#f07c4a", "#d7191c"))
  expect_identical(zlegend("spectral"), z_colors_spectral,
                   "Spectral color scheme not returned correctly")
})

test_that("Calculating group curves stats works", {

  # Get test data from the tutorial
  group.ids <- read_groups(options$groups.file)[,1]
  weights <- read_spp(options$results.spp.file)[,1]
  curves.data <- read_curves(options$results.curves)
  grp.curves.data <- read_grp_curves(options$results.grp.curves)

  # First test that regroup_curves() produces the same results as in the
  # original group curves file. ext2 must be removed as it can't be calculated
  # by regroup_curves().
  grp.curves.data <- grp.curves.data[,-grep("ext2", names(grp.curves.data))]
  # Having a wrong number of group.ids should produce an error
  too.many.group.ids <- rep(group.ids, 2)
  expect_error(new.grp.curves.data <- regroup_curves(curves.data,
                                                     weights,
                                                     too.many.group.ids),
               info="Providing too many group.ids to regroup_curves() should cause an error")
  # Having a wrong number of weights should produce an error
  too.many.weights <- rep(weights, 2)
  expect_error(new.grp.curves.data <- regroup_curves(curves.data,
                                                     too.many.weights,
                                                     group.ids),
               info="Providing too many weights to regroup_curves() should cause an error")

  # Calculate the new groups
  new.grp.curves.data <- regroup_curves(curves.data, weights, group.ids)
  new.grp.curves.data <- new.grp.curves.data[,-grep("ext2", names(new.grp.curves.data))]

  expect_identical(round(grp.curves.data$min.group1),
                   round(new.grp.curves.data$min.group1),
                   "regroup_curves not returning correct min values for group 1")
  #expect_identical(round(grp.curves.data$min.group2, 5),
  #                 round(new.grp.curves.data$min.group2, 5),
  #                 "regroup_curves not returning correct min values for group 2")
})

test_that("leaf_tags() works", {

  list_data <- list()
  list_data[["Settings"]] <- list()
  list_data[["Settings"]][["removal rule"]] <- "1"
  list_data[["Settings"]][["warp factor"]] <- "100"
  list_data[["Settings"]][["edge removal"]] <- "1"
  list_data[["Settings"]][["annotate name"]] <- "0"
  list_data[["Settings"]][["use groups"]] <- "1"
  list_data[["Settings"]][["groups file"]] <- "01/01_groups.txt"
  list_data[["Info-gap settings"]] <- list()
  list_data[["Info-gap settings"]][["Info-gap proportional"]] <- "0"
  list_data[["Info-gap settings"]][["use info-gap weights"]] <- "1"
  list_data[["Info-gap settings"]][["Info-gap weights file"]] <- ""

  correct_section_tags <- c("Settings.removal rule", "Settings.warp factor",
                         "Settings.edge removal", "Settings.annotate name",
                         "Settings.use groups", "Settings.groups file",
                         "Info-gap settings.Info-gap proportional",
                         "Info-gap settings.use info-gap weights",
                         "Info-gap settings.Info-gap weights file")

  expect_error(leaf_tags("foo"),
               regexp = "Function only accepts lists",
               info = "Using something else than list should cause error.")
  expect_equal(names(leaf_tags(list_data)), correct_section_tags,
               info = "Correct tags not found")
  expect_equal(leaf_tags(list_data)[["Settings.removal rule"]], "1",
               info = "Incorrect tag/value pair received.")

  # Test without section prefixes
  correct_tags <- c("removal rule", "warp factor", "edge removal",
                    "annotate name", "use groups", "groups file",
                    "Info-gap proportional", "use info-gap weights",
                    "Info-gap weights file")
  expect_equal(names(leaf_tags(list_data, omit_sections = TRUE)),
               correct_tags,
               info = "Correct tags not found")

})
