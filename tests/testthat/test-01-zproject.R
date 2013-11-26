context("Tutorial data existence")

# Tutorial directories
tutorial.dir <- system.file("extdata", "tutorial", package="zonator")
setup.dir <- file.path(tutorial.dir, "basic")
data.dir <- file.path(tutorial.dir, "data")
output.dir <- file.path(setup.dir, "basic_output")

# Tutorial run and configuration files
bat.file <- file.path(setup.dir, "01_core_area_zonation.bat")
dat.file <- file.path(setup.dir, "01_core_area_zonation/01_core_area_zonation.dat")
spp.file <- file.path(setup.dir, "01_core_area_zonation/01_core_area_zonation.spp")
species.files <- paste0(data.dir, "/species", 1:7, ".tif")
groups.file <- file.path(setup.dir, "groups.txt")

test_that("Zonation tutorial data is available", {
  
  expect_true(file.exists(tutorial.dir), 
              "Tutorial directory not found in root of the package.")
  expect_true(file.exists(setup.dir),
              "Input setup directory not found in the tutorial folder.")
  expect_true(file.exists(data.dir),
              "Input data directory not found in the tutorial folder.")
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
  
  expect_true(file.exists(groups.file), 
              "Tutorial groups file not found in the tutorial input folder.")
  
})

context("Zproject creation")

test_that("Zproject is created correctly based on existing project", {    

    test.project <- create_zproject(setup.dir)
    
    # Test slots
    expect_that(test.project, is_a("Zproject"),
                paste("Test project is not an instance of class 'Zproject':",
                      class(test.project)))
    expect_that(test.project@root, equals(setup.dir),
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
})

test_that("Zproject is created correctly as a new project", {
  temp.dir <- file.path(tempdir(), "test_zproject")
  
  variant.names <- c("GPAN_01_abf",
                     "GPAN_02_caz",
                     "GPAN_03_abf_w",
                     "GPAN_04_caz_w", 
                     "GPAN_05_abf_w_ecor_w10",
                     "GPAN_06_caz_w_ecor_w10",
                     "GPAN_07_abf_w_ecor_w40",
                     "GPAN_08_caz_w_ecor_w40")
  
  dat.template <- system.file("extdata", "template_GPAN.dat", package="zonator")
  
  test.project <- create_zproject(root=temp.dir, variants=variant.names,
                                  dat.from=dat.template)
  
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
})

context("Zvariant creation")

test_that("Zvariant is created correctly", {

  # Variant with no results, no results
  no.results.bat.file <- file.path(setup.dir, "06_dummy_for_testing.batx")
  no.results.variant <- new("Zvariant", bat.file=no.results.bat.file)
  
  # Variant with results
  
  results.variant <- new("Zvariant", bat.file=bat.file)
  
  # Test slots
  expected.name <- gsub(".bat", "", basename(bat.file))
  expect_that(results.variant@name, equals(expected.name),
              paste0("Test variant object's slot 'name' is not '",
                     expected.name, "' :", results.variant@name))
  expect_that(results.variant@bat.file, equals(bat.file),
              paste("Test variant object's slot 'bat.file' does not point to the real bat.file:",
                    results.variant@bat.file))
  
  # FIXME: Variant call.params is not tested for here because the class
  # initializer already has a check function. Should it rather be here?
  
  # This variant should have the results as well (ship with the package)
  expect_true(has_results(results.variant), 
              "Test variant doesn't have results although it should")
  
  
})