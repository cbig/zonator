context("Controlling Zonation")

bat.file <- system.file("extdata/zonation-tutorial", "do_abf.bat",
                        package="zonator")

faulty.bat.file <- file.path(dirname(bat.file), "wrong.bat")

test_that("Zonation executable checking works", {
  # Zig3
  expect_true(check_zonation(), 
              "Zonation v3 (zig3) not found in the system.")
  # Zig4
  expect_true(check_zonation("zig4"), 
              "Zonation v4 (zig4) not found in the system.")
  # Version not found
  expect_warning(check_zonation("zig5"),
              "Zonation executable zig5 not found in the system.")
})

test_that("Parsing a bat file works", {
  
  if (.Platform$OS.type == "unix") {
    correct.sequence <- c("zig3", "-r", 
                          "tutorial_input/set_abf.dat", 
                          "tutorial_input/splist_abf.spp", 
                          "tutorial_output/output_abf.txt", 
                          "0.0", "0", "1.0", "0", 
                          "--grid-output-formats=compressed-tif",
                          "--image-output-formats=png")
  } else {
    correct.sequence <- c("call", "zig3.exe", "-r", 
                          "tutorial_input/set_abf.dat", 
                          "tutorial_input/splist_abf.spp", 
                          "tutorial_output/output_abf.txt", 
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