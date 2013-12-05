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
