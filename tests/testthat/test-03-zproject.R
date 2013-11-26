context("Zproject creation")

test_that("Zproject is created correctly based on existing project", {    
  setup.dir <- .options$setup.dir
  
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