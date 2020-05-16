context("Controlling Zonation")

options <- get_options()

test_that("Zonation executable checking works", {
  # Version not found
  expect_warning(check_zonation("zig5"),
                 info="Exe-not-found error not reported right")
})

context("Creating Zonation input files")

test_that("Creating a spp file based on a directory of input rasters works", {

  # This is what the spp file is supposed to look like
  correct_spp_file <- options$spp.file
  correct_spp_content <- read_spp(correct_spp_file)
  row.names(correct_spp_content) <- NULL

  # Generate a temporary spp file
  temp_spp_file <- tempfile(fileext = ".spp")

  create_spp(filename = temp_spp_file, weight = correct_spp_content$weight,
             alpha = correct_spp_content$alpha, bqp = correct_spp_content$bqp,
             bqp_p = correct_spp_content$bqp_p,
             cellrem = correct_spp_content$cellrem,
             spp_file_dir = options$data.dir,
             spp_file_pattern = "(species)[1-9]{1}\\.(tif)",
             override_path = "../data")

 #  Compare the content of files
  expect_equal(correct_spp_content, read_spp(temp_spp_file),
               info = "Generated spp file content not the same as template")
})
