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
  expect_true(all(unlist(has_results(results.variant))), 
              "Test variant doesn't have all results although it should")
  
  # spp-data
  expect_true(.hasSlot(results.variant, "spp.data"),
              "Test variant object doesn't have a slot 'spp.data'")
  correct.feature.data <- read_spp(spp.file)
  
  # Generate the feature names that Zvariant should generate as well
  correct.feature.data$name <- basename(tools::file_path_sans_ext(correct.feature.data$filepath))
  expect_identical(featurenames(results.variant), correct.feature.data$name,
                   "Test variant object's feature names not what expected")
})

test_that("Zvariant without results is created correctly", {
  
  # Variant with no results, no results
  no.results.bat.file <- file.path(.options$setup.dir, 
                                   "06_dummy_for_testing.batx")
  expect_warning(no.results.variant <- new("Zvariant", 
                                           bat.file=no.results.bat.file))
  
  # Groups
  res <-unlist(has_results(no.results.variant))
  expect_false(all(res),
               "Test variant should not have results")
  expect_true(is.na(groups(no.results.variant)),
               "Test variant should not have groups")
  
  correct.grp.names <- c("mammals", "owls")
  names(correct.grp.names) <- c(1, 2)
  expect_error((groupnames(no.results.variant) <- correct.grp.names))
}
)

context("Zvariant methods")

test_that("Assigning and fetching feature names works", {
  bat.file <- .options$bat.file
  spp.file <- .options$spp.file
  results.variant <- new("Zvariant", bat.file=bat.file)
  
  correct.feature.data <- read_spp(spp.file)
  
  # Test assigning feature names
  correct.feature.names <- c("Koala", "Masked.owl", "Powerful.owl", 
                             "Tiger.quoll", "Sooty.owl", "Squirrel.glider",
                             "Yellow-bellied.glider")
  featurenames(results.variant) <- correct.feature.names
  expect_identical(correct.feature.names, featurenames(results.variant),
                   "Test variant feature names not what they're supposed to")
  
  # Check for valid names
  invalid.feature.names <- correct.feature.names
  # Inroduce a duplicate name
  invalid.feature.names[2] <- "Koala"
  expect_error(featurenames(results.variant) <- invalid.feature.names)
  
  # Check that the values match, first patch the spp data with the names
  correct.feature.data$name <- correct.feature.names
  expect_true(all(correct.feature.data == results.variant@spp.data),
              paste("Test variant objects 'spp.data' slot does not correspond",
                    "to expectations"))
  
  # Names should be reflected in results curve headers as well
  variant.results <- results(results.variant)
  curves.names <- names(curves(variant.results))
  curves.names <- curves.names[8:length(curves.names)]
  expect_identical(correct.feature.data$name, curves.names,
                   paste("Test variant object's result curves header does not",
                         "contain the correct feature names"))
})

test_that("Assigning and fetching group names and identities works", {
  bat.file <- .options$bat.file
  spp.file <- .options$spp.file
  results.variant <- new("Zvariant", bat.file=bat.file)

  correct.feature.data <- read_spp(spp.file)
  correct.feature.data$name <- basename(tools::file_path_sans_ext(correct.feature.data$filepath))
  
  # Groups
  expect_true(.hasSlot(results.variant, "groups"),
              "Test variant object doesn't have a slot 'groups'")
  
  correct.grp.codes <- c(1, 2, 2, 1, 2, 1, 1)
  variant.grp.codes <- groups(results.variant)
  expect_identical(variant.grp.codes, correct.grp.codes, 
                   paste("Test variant group information wrong"))
  
  # The group names haven't been set, so there should be none
  expect_true(is.na(groupnames(results.variant)),
              "Test variant group names not NA although they haven't been set")
  
  # Test assigning correct group names and codes
  correct.grp.names <- c("mammals", "owls")
  names(correct.grp.names) <- c(1, 2)
  groupnames(results.variant) <- correct.grp.names
  expect_identical(correct.grp.names, groupnames(results.variant),
                   "Test variant group names not what they're supposed to")
  
  # Test the sppdata method. NOTE that sppdata will cbind group code 
  # column to the end! 
  extended.feature.data <- cbind(correct.feature.data, correct.grp.codes)
  names(extended.feature.data) <- c(names(correct.feature.data), "group")
  expect_true(all(extended.feature.data == sppdata(results.variant)),
              paste("Method sppdata doesn't return what it's supposed to"))
  # Test also using group names
  extended.feature.data <- cbind(correct.feature.data, results.variant@groups$name)
  names(extended.feature.data) <- c(names(correct.feature.data), "group.name")
  
  expect_true(all(extended.feature.data == sppdata(results.variant, group.names=TRUE)),
              paste("Method sppdata doesn't return what it's supposed to"))
  
  # Test assigning wrong group codes
  incorrect.grp.names <- c("foo", "bar")
  names(incorrect.grp.names) <- c(4, 5)
  expect_error(groupnames(results.variant) <- incorrect.grp.names)
  
  # Test for spp data and groups and spp names functionality
})
