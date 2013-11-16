context("Controlling Zonation")

bat.file <- system.file("extdata/tutorial/basic", "do_01_core_area_zonation.bat",
                        package="zonator")

faulty.bat.file <- file.path(dirname(bat.file), "wrong.bat")

test_that("Zonation executable checking works", {
  # Version not found
  expect_warning(check_zonation("zig5"),
              "Zonation executable zig5 not found in the system.")
})

test_that("Parsing a bat file works", {
  
  if (.Platform$OS.type == "unix") {
    correct.sequence <- c("zig3", "-r", 
                          "01_core_area_zonation/01_core_area_zonation.dat", 
                          "01_core_area_zonation/01_core_area_zonation.spp", 
                          "basic_output/01_core_area_zonation/01_core_area_zonation.txt", 
                          "0.0", "0", "1.0", "0", 
                          "--grid-output-formats=compressed-tif",
                          "--image-output-formats=png")
  } else {
    correct.sequence <- c("call", "zig3.exe", "-r", 
                          "01_core_area_zonation/01_core_area_zonation.dat", 
                          "01_core_area_zonation/01_core_area_zonation.spp", 
                          "basic_output/01_core_area_zonation/01_core_area_zonation.txt", 
                          "0.0", "0", "1.0", "0", 
                          "--grid-output-formats=compressed-tif", 
                          "--image-output-formats=png")
  }
  # Test the default command sequence
  expect_equal(parse_bat(bat.file), paste(correct.sequence, collapse=" "))
  
  # Change to a different executable version and test again
  if (.Platform$OS.type == "unix") {
    correct.sequence[1] <- "zig4"
  } else {
    correct.sequence[2] <- "zig4.exe"
  }
  expect_equal(parse_bat(bat.file, "zig4"), 
               paste(correct.sequence, collapse=" "))
  
  
})

test_that("Running Zonation works", {
  
  #expect_true(run_bat(bat.file),
  #            paste("Running bat file", bat.file, "failed."))
  expect_error(run_bat(faulty.bat.file),
               paste("Bat file", faulty.bat.file, "does not exist."))
  
})