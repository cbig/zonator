context("Zproject creation")

options <- get_options()

test_that("Zproject is created correctly based on existing project", {
  setup.dir <- options$setup.dir

  test.project <- load_zproject(setup.dir)

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

context("Zproject methods")

test_that("Retrieving variants works", {
  setup.dir <- options$setup.dir
  test.project <- load_zproject(setup.dir)

  bat.file <- options$bat.file
  spp.file <- options$spp.file
  test.variant <- new("Zvariant", bat.file = bat.file)

  # Number of variants
  expect_equivalent(nvariants(test.project), 6,
                    info = paste("Number of variants reported incorrectly"))

  # Single variant retrieval based on index
  expect_identical(get_variant(test.project, 1),
                   test.variant,
                   "Variant isn't returned correctly based on index")

  # Single variant retrieval based on name
  expect_identical(get_variant(test.project, "01"),
                   test.variant,
                   "Variant isn't returned correctly based on name")

  all.variants <- test.project@variants
  expect_equivalent(variants(test.project), all.variants,
                    "All variants are not returned correctly")
})

# test_that("Zproject is created correctly as a new project", {
#   temp.dir <- file.path(tempdir(), "test_zproject")
#
#   variant.names <- c("GPAN_01_abf",
#                      "GPAN_02_caz",
#                      "GPAN_03_abf_w",
#                      "GPAN_04_caz_w",
#                      "GPAN_05_abf_w_ecor_w10",
#                      "GPAN_06_caz_w_ecor_w10",
#                      "GPAN_07_abf_w_ecor_w40",
#                      "GPAN_08_caz_w_ecor_w40")
#
#   dat.template <- system.file("extdata", "template_GPAN.dat", package="zonator")
#
#   test.project <- create_zproject(root=temp.dir, variants=variant.names,
#                                   dat.from=dat.template)
#
#   # Test slots
#   expect_that(test.project, is_a("Zproject"),
#               paste("Test project is not an instance of class 'Zproject':",
#                     class(test.project)))
#   expect_that(test.project@root, equals(temp.dir),
#               paste("Test project object's slot 'root' does not point to tutorial directory:",
#                     test.project@root))
#   # Test that there are variants
#   expect_true(length(test.project@variants) >= 1,
#               "Test project has no variants.")
#
#   for (variant in test.project@variants) {
#     expect_that(variant, is_a("Zvariant"),
#                 paste("Test project object's slot 'variants' contains an object",
#                       "not an instance of class 'Zvariant:",
#                       class(variant)))
#   }
# })
