context("Utilities")

test_that("name checking works", {
  valid.names <- c("foo", "bar", "spam", "eggs")
  # Pass
  expect_equal(valid.names, check_names(valid.names),
               "Truly valid names not accepted by check_names()")
  
  invalid.names <- c("foo", "foo", "spam", "eggs")
  # Fail
  expect_error(check_names(invalid.names),
               info="check_names() does not throw an error when there are duplicate names")
  
  valid.names <- c(11, 12, 13, 16)
  # Pass
  expect_equal(as.character(valid.names), check_names(valid.names))
  
  invalid.names <- c(11, 12, 13, 13, 12)
  # Fail
  expect_error(check_names(invalid.names),
               info="check_names() does not throw an error when there are duplicate names")
  
  valid.names <- c(11, 12, 13, 16)
  # Pass
  expect_equal(as.character(valid.names), check_names(valid.names))
  
  invalid.names <- c(11, 12, 13, 13, 12)
  # Fail
  expect_error(check_names(invalid.names),
               info="check_names() does not throw an error when there are duplicate names")
  
  invalid.names <- c("foo", "bar", "", "eggs")
  # Fail
  expect_error(check_names(invalid.names),
               info="check_names() does not throw an error when there are empty names")
  
  valid.names <- c("foo.bar", "bar", "spam", "eggs")
  invalid.names <- c("foo bar", "bar", "spam", "eggs")
  # Pass
  expect_warning(check_names(invalid.names),
                 info="check_names() should throw a warning when items have whitespaces")
  suppressWarnings(expect_equal(valid.names, check_names(invalid.names),
                                "check_names() does not deal with whitespace in correct way"))
  
})

test_that("index mapping works", {
  
  data(diamonds)
  
  correct.inds <- c(1, 3, 4, 6)
  expect_equal(map_indexes(c("carat", "color", "clarity", "table"),
                           names(diamonds)),
               correct.inds,
               "map_indexes() does not return the right index values with names")
  
  expect_warning(map_indexes(c("carat", "color", "clarity", "table", "XXX"),
                             names(diamonds)),
                 infor="map_indexes() does not warn about missing header name")
  
  suppressWarnings(expect_true(is.null(map_indexes(c("XXX"), names(diamonds))),
              "map_indexes() does not return NULL when no names are found"))
  
  expect_equal(map_indexes(correct.inds,
                           1:ncol(diamonds)),
               correct.inds,
               "map_indexes() does not return the right index values with indexes")
  })

test_that("path checking works", {
  # This file will actually exist
  path1 <- tempfile(fileext = ".txt")
  path2 <- tempfile(fileext = ".txt")
  file.name1 <- basename(path1)
  file.name2 <- basename(path2)
  dir.name1 <- dirname(path1)
  dir.name2 <- paste0(dirname(path2), "fhasjkldfhlru")
  # Create only the first file path
  file.create(path1)
  
  expect_equal(path1, check_path(path1),
               "check_path() does not return a valid path with full path")
  expect_equal(path1, check_path(file.name1, dir.name1),
               "check_path() does not return a valid path with valid file and dir names")
  expect_equal(dir.name1, check_path(file.name2, dir.name1),
               "check_path() does not return only valid parent dir")
  
  expect_error(check_path(file.name1),
               info="check_path() should throw error if only file name is provided")
  expect_error(check_path(file.name2, dir.name2),
               info="check_path() should throw error if missing file and dir names are provided")
  
  # Test relative path normalization
  # This dir doesn't actually exist
  deep.dir <- file.path(dir.name1, "kfsjlkdfjl", "adfhjklf")
  rel.file <- file.path("..", "..", file.name1)
  
  expect_equal(path1, check_path(rel.file, deep.dir),
               "check_path() does not normalize relative path correctly")
  
  deep.dir2 <- file.path(dir.name1, "kfsjlkdfjl")
  rel.file2 <- file.path("..", file.name1)
  
  expect_equal(path1, check_path(rel.file2, deep.dir2),
               "check_path() does not normalize relative path correctly")
  
  unlink(path1)
  unlink(path2)
})