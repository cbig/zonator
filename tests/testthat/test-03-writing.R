context("Writing Zonation input files")

test_that("Writing a dat file works", {

  dat_file <- system.file("extdata", "template.dat", package = "zonator")

  org_dat_data <- read_dat(dat_file)

  # Change removal rule to 2 (ABF)
  org_dat_data$Settings$`removal rule` <- "2"

  # Don't expect anything else but a list
  expect_error(write_dat(x = "foo", filename = dat_file_path),
               info = "Providing non-list data should cause an error.")

  # Trying to write lists with more than 1 level of nestedness should cause
  # an error.
  deep_dat_data <- org_dat_data
  deep_dat_data$Settings[["dummy"]] <- list("level2" = 1)
  expect_error(write_dat(x = deep_dat_data, filename = dat_file_path),
               info = "No error emitted when writing more one level of data to dat file.")

  # Write file, expect message informing about the write operation
  dat_file_path <- tempfile(fileext = ".dat")
  write_dat(x = org_dat_data, filename = dat_file_path)

  # Read in the file again
  new_dat_data <- read_dat(dat_file_path)
  # Check that the data is actually the same
  expect_equal(org_dat_data, new_dat_data,
               info = "Data not the same after writing and re-reading.")

  # Overwriting should fail if not defined explicitly
  expect_error(write_dat(x = org_dat_data, filename = dat_file_path),
               info = "No error emitted when overwriting implicitly.")
})
