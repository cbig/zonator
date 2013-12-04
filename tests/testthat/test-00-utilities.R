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
