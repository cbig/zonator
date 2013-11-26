context("Parsing Zonation input files")

test_that("Parsing a bat file works", {
  
  bat.file <- system.file("extdata/tutorial/basic", "01_core_area_zonation.bat",
                          package="zonator")
  
  faulty.bat.file <- file.path(dirname(bat.file), "wrong.bat")
  
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

test_that("Parsing an unpopulated spp file works", {
  
  group.file <- system.file("extdata/tutorial/basic", "groups.txt",
                          package="zonator")
  
})

test_that("Parsing an unpopulated spp file works", {
  
  # Test with an unpopulated spp file (i.e. no rows)
  spp.file <- system.file("extdata", 
                          "template.spp",
                          package="zonator")
  
  spp.data <- read_spp(spp.file)
  expect_true(is.data.frame(spp.data),
              "Spp data read in not a data frame")
  # Should return an empty data.frame
  expect_true(all(dim(spp.file) == c(0, 0)),
              "Spp data should have dim c(0,0)")
  
})

test_that("Parsing a populated spp file works", {
  
  # Test with a populated spp file (i.e. has rows)
  
  spp.file <- system.file("extdata/tutorial/basic/01_core_area_zonation", 
                          "01_core_area_zonation.spp",
                          package="zonator")
  
  # Construct a spp data frame corresponding to 01_core_area_zonation.spp
  correct.data <- data.frame(weight=1,
                             alpha=c(1.0, 0.5, 0.25, 0.75, 0.50, 1.50, 1.0),
                             bqp=1,
                             bqp_p=1,
                             cellrem=1,
                             filepath=paste0("../data/species", 1:7, ".tif"),
                             stringsAsFactors=FALSE)
  
  spp.data <- read_spp(spp.file)
  expect_true(is.data.frame(spp.data),
              "Spp data read in not a data frame")
  # Check names
  expect_true(all(names(spp.data) == names(correct.data)),
              paste("Spp data names no what expected:", 
                    paste(names(spp.data), collapse=" ")))
  # Check that the values match
  expect_true(all(spp.data == correct.data),
              "Spp data does not correspond to expectations")
  
  })