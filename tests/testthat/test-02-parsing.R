context("Parsing Zonation input files")

test_that("Parsing a bat file works", {

  bat.file <- .options$bat.file

  faulty.bat.file <- file.path(dirname(bat.file), "wrong.bat")

  if (.Platform$OS.type == "unix") {
    correct.sequence <- c("zig4", "-r",
                          "01/01.dat",
                          "01/01.spp",
                          "01/01_out/01.txt",
                          "0.0", "0", "1.0", "0",
                          "--grid-output-formats=compressed-tif",
                          "--image-output-formats=png")
  } else {
    correct.sequence <- c("call", "zig4.exe", "-r",
                          "01/01.dat",
                          "01/01.spp",
                          "01/01_out/01.txt",
                          "0.0", "0", "1.0", "0",
                          "--grid-output-formats=compressed-tif",
                          "--image-output-formats=png")
  }
  # Test the default command sequence
  expect_identical(parse_bat(bat.file), paste(correct.sequence, collapse = " "))

  # Change to a different executable version and test again
  if (.Platform$OS.type == "unix") {
    correct.sequence[1] <- "zig4"
    expect_identical(parse_bat(bat.file, exe = "zig4"),
                 paste(correct.sequence, collapse = " "))
  } else {
    correct.sequence[2] <- "zig4.exe"
    expect_identical(parse_bat(bat.file, exe = "zig4.exe"),
                     paste(correct.sequence, collapse = " "))
  }

})

test_that("Parsing an ini file (dat) works", {

  dat.file <- .options$dat.file

  faulty.dat.file <- file.path(dirname(dat.file), "wrong.bat")

  # Test that error is reported if a dat file can't be found
  suppressWarnings(expect_error(read_dat(faulty.dat.file),
                                "(dat-file).+(not found!)",
                                info = "Error raised should name the dat file"))

  # Test that error is reported if the file doesn't seem to be a dat-file
  bat.file <- .options$bat.file
  suppressWarnings(expect_error(read_dat(bat.file),
                                "(dat-file).+(doesn't seem to have any section headers.).+",
                                info = "Error raised should name the dat file"))

  correct.data <- list()
  correct.data[["Settings"]] <- list()
  correct.data[["Settings"]][["removal rule"]] <- "1"
  correct.data[["Settings"]][["warp factor"]] <- "100"
  correct.data[["Settings"]][["edge removal"]] <- "1"
  correct.data[["Settings"]][["annotate name"]] <- "0"
  correct.data[["Settings"]][["use groups"]] <- "1"
  correct.data[["Settings"]][["groups file"]] <- "01/01_groups.txt"

  expect_equal(read_dat(dat.file), correct.data,
               info = "Data parsed from dat file is not correct")

})

test_that("Parsing a valid groups file works", {

  groups.file <- .options$groups.file

  groups.data <- read_groups(groups.file)

  expect_true(is.data.frame(groups.data),
              "Groups data read in not a data frame")

  # Construct a groups data frame corresponding to groups.txt
  correct.data <- data.frame(output.group = c(1, 2, 2, 1, 2, 1, 1),
                             condition.group = -1,
                             retention.group = -1,
                             retention.mode = 1,
                             local.edge.correct.group = -1)

  # Check names
  expect_true(all(names(groups.data) == names(correct.data)),
              paste("Groups data names no what expected:",
                    paste(names(groups.data), collapse = " ")))
  # Check that the values match
  expect_true(all(groups.data == correct.data),
              "Groups data does not correspond to expectations")

})

test_that("Parsing an invalid groups file works", {

  groups.file <- system.file("extdata/tutorial/basic", "invalid_groups.txt",
                             package = "zonator")

  expect_error(groups.data <- read_groups(groups.file),
               info = "Invalid groups file should throw an error")

})

test_that("Parsing an unpopulated spp file works", {

  # Test with an unpopulated spp file (i.e. no rows)
  spp.file <- system.file("extdata", "template.spp",
                          package = "zonator")

  suppressWarnings(spp.data <- read_spp(spp.file))
  expect_true(is.data.frame(spp.data),
              "Spp data read in not a data frame")
  # Should return an empty data.frame
  expect_true(all(dim(spp.file) == c(0, 0)),
              "Spp data should have dim c(0,0)")

})

test_that("Parsing a populated spp file works", {

  # Test with a populated spp file (i.e. has rows)

  spp.file <- .options$spp.file

  # Construct a spp data frame corresponding to 01_core_area_zonation.spp
  correct.data <- data.frame(weight = 1,
                             alpha = c(1.0, 0.5, 0.25, 0.75, 0.50, 1.50, 1.0),
                             bqp = 1,
                             bqp_p = 1,
                             cellrem = 1,
                             filepath = paste0("../data/species", 1:7, ".tif"),
                             stringsAsFactors = FALSE)

  spp.data <- read_spp(spp.file)
  expect_true(is.data.frame(spp.data),
              "Spp data read in not a data frame")
  # Check column names
  expect_true(all(names(spp.data) == names(correct.data)),
              paste("Spp data names no what expected:",
                    paste(names(spp.data), collapse = " ")))
  # Check that the values match
  expect_true(all(spp.data == correct.data),
              "Spp data does not correspond to expectations")

  })
