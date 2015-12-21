context("Writing Zonation input files")

test_that("Writing a dat file works", {
  
  dat_file <- system.file("extdata/tutorial/basic/01", "01.dat",
                          package="zonator")
  
  org_dat_data <- read_dat(dat_file)
  
  # Change removal rule to 2 (ABF)
  org_dat_data$Settings$removal_rule <- 2
  # Write file, expect message informing about the write operation
  #dat_file_path <- tempfile(fileext = ".dat")
  #expect_message(write_dat(x = org_dat_data, file = dat_file_path),
  #               paste("Wrote dat-file", dat_file_path),
  #               info = "Correct message not emitted from writing a dat file.")
})