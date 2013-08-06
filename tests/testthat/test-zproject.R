context("Tutorial data existence")

# Tutorial directories
tutorial.dir <- system.file("extdata", "zonation-tutorial", package="zonator")
input.dir <- file.path(tutorial.dir, "tutorial_input")
output.dir <- file.path(tutorial.dir, "tutorial_output")

# Tutorial run and configuration files
bat.file <- file.path(tutorial.dir, "do_abf.bat")
dat.file <- file.path(input.dir, "set_abf.dat")
spp.file <- file.path(input.dir, "splist_abf.spp")
species.files <- paste0(input.dir, "/species", 1:7, ".tif")

test_that("Zonation tutorial data is available", {
  
  expect_true(file.exists(tutorial.dir), 
              "Tutorial directory not found in root of the package.")
  expect_true(file.exists(input.dir),
              "Input directory not found in the tutorial folder.")
  expect_true(file.exists(output.dir),
              "Output directory not found in the tutorial folder.")
  expect_true(file.exists(bat.file), 
              "Tutorial batch file not found in the tutorial folder.")
  expect_true(file.exists(dat.file), 
              "Tutorial dat file not found in the tutorial input folder.")
  expect_true(file.exists(spp.file), 
              "Tutorial spp file not found in the tutorial input folder.")
  
  for (species in species.files) {
    expect_true(file.exists(species), 
                paste("Tutorial species raster", species, 
                      "not found in the tutorial input folder."))
  }
})

context("Zproject creation")

test_that("Zproject is created correctly based on existing project", {    
    test.project <- create_zproject(tutorial.dir)
    
    # Test slots
    expect_that(test.project, is_a("Zproject"),
                paste("Test project is not an instance of class 'Zproject':",
                      class(test.project)))
    expect_that(test.project@root, equals(tutorial.dir),
                paste("Test project object's slot 'root' does not point to tutorial directory:",
                      test.project@root))
    # Test that there are variants
    expect_true(length(test.project@variants) >= 1, 
                "Test project has no variants.")
    
    for (variant in test.project@variants) {
      expect_that(variant, is_a("Zvariant"),
                  paste("Test project object's slot 'variants' contains an object",
                        "not an instance of class 'Zvariant:",
                        class(variant)))
    
    # Test for a new project
    temp.dir <- tempdir()
    test.project <- create_zproject(temp.dir, variants=c("var1", "var2"))
  }
})

test_that("Zproject is created correctly as a new project", {
  temp.dir <- file.path(tempdir(), "test_zproject")
  test.project <- create_zproject(temp.dir, variants=c("variant1", "variant2"))
  
  # Test slots
  expect_that(test.project, is_a("Zproject"),
              paste("Test project is not an instance of class 'Zproject':",
                    class(test.project)))
  expect_that(test.project@root, equals(temp.dir),
              paste("Test project object's slot 'root' does not point to tutorial directory:",
                    test.project@root))
  # Test that there are variants
  expect_true(length(test.project@variants) >= 1, 
              "Test project has no variants.")
  
  for (variant in test.project@variants) {
    expect_that(variant, is_a("Zvariant"),
                paste("Test project object's slot 'variants' contains an object",
                      "not an instance of class 'Zvariant:",
                      class(variant)))
  }
  file.remove(temp.dir)
})

context("Zvariant creation")

test_that("Zvariant is created correctly", {

  test.variant <- new("Zvariant", bat.file=bat.file)
  
  # Test slots
  expect_that(test.variant@name, equals("do_abf"),
              paste("Test variant object's slot 'name' is not 'do_abf':",
                    test.variant@name))
  expect_that(test.variant@bat.file, equals(bat.file),
              paste("Test variant object's slot 'bat.file' does not point to the real bat.file:",
                    test.variant@bat.file))
  
  batch.file.content <- scan(file=bat.file, "character", sep=" ", quiet=TRUE)
  
  
})