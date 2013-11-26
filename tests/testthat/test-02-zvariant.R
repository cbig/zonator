context("Zvariant creation")

test_that("Zvariant with results is created correctly", {
  bat.file <- .options$bat.file
  spp.file <- .options$spp.file
  results.variant <- new("Zvariant", bat.file=bat.file)
  
  # Name
  expected.name <- gsub(".bat", "", basename(bat.file))
  expect_true(.hasSlot(results.variant, "name"),
              "Test variant object doesn't have a slot 'name'")
  expect_that(results.variant@name, equals(expected.name),
              paste0("Test variant object's slot 'name' is not '",
                     expected.name, "' :", results.variant@name))
  
  # bat-file
  expect_true(.hasSlot(results.variant, "bat.file"),
              "Test variant object doesn't have a slot 'bat.file'")
  expect_that(results.variant@bat.file, equals(bat.file),
              paste("Test variant object's slot 'bat.file' does not point to the real bat.file:",
                    results.variant@bat.file))
  
  # Results
  expect_true(.hasSlot(results.variant, "results"),
              "Test variant object doesn't have a slot 'results'")
  expect_true(has_results(results.variant), 
              "Test variant doesn't have results although it should")
  
  # spp-data
  expect_true(.hasSlot(results.variant, "spp.data"),
              "Test variant object doesn't have a slot 'spp.data'")
  correct.spp.data <- read_spp(spp.file)
  # Check that the values match
  expect_true(all(correct.spp.data == results.variant@spp.data),
              paste("Test variant objects 'spp.data' slot does not correspond",
                    "to expectations"))
  
  # Groups
  expect_true(.hasSlot(results.variant, "groups"),
              "Test variant object doesn't have a slot 'groups'")
  
  correct.grp.levels <- factor(c(1, 2, 2, 1, 2, 1, 1))
  variant.grp.levels <- groups(results.variant)
  expect_identical(variant.grp.levels, correct.grp.levels, 
              paste("Test variant group information wrong"))
  
})

test_that("Zvariant without results is created correctly", {
  
  # Variant with no results, no results
  no.results.bat.file <- file.path(.options$setup.dir, 
                                   "06_dummy_for_testing.batx")
  no.results.variant <- new("Zvariant", bat.file=no.results.bat.file)
  
  expect_false(has_results(no.results.variant),
               "Test variant should not have results")
  expect_true(is.na(groups(no.results.variant)),
               "Test variant should not have groups")
}
)