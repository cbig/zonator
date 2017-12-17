context("Zvariant creation")

test_that("Zvariant with results is created correctly", {
  bat.file <- .options$bat.file
  spp.file <- .options$spp.file
  results.variant <- new("Zvariant", bat.file = bat.file)

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

  # [fixme] - ppa.lsm should be included in the tutorial runs, otherwise this
  # test will fail as well

  # Results
  #expect_true(all(unlist(has_results(results.variant))),
  #            "Test variant doesn't have all results although it should")

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

  # Variant with no groups, no results
  no.results.bat.file <- file.path(.options$setup.dir,
                                   "06.batx")
  suppressWarnings(no.results.variant <- new("Zvariant",
                                             bat.file = no.results.bat.file))

  # Groups
  res <- unlist(has_results(no.results.variant))
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
#
# test_that("Print method works", {
#   bat.file <- .options$bat.file
#   spp.file <- .options$spp.file
#   results.variant <- new("Zvariant", bat.file = bat.file)
#
#   text_output <- paste0(c("name       : ", "01_core_area_zonation", "\n",
#                           "bat-file   : ", bat.file, "\n",
#                           "features   : ", "7 [species1, species2, species3, species4, species5, ...]", "\n",
#                           "groups     : ", "2 [group1, group2]", "\n",
#                           "has results: ", "yes (created: 2014-08-13)"),
#                         collapse = "")
#
#   expect_output(show(results.variant), text_output,
#                 info = "Output from show() not correct")
#   expect_output(print(results.variant), text_output,
#                 info = "Output from print() not correct")
# })

test_that("Getting the number of features works", {
  bat.file <- .options$bat.file
  spp.file <- .options$spp.file
  results.variant <- new("Zvariant", bat.file = bat.file)

  expect_that(nfeatures(results.variant), equals(7),
              info = "Test variant number of features not correct")
})

test_that("Getting weights works", {
  # Use variant 4 (distribution smoothing) because it has weights
  bat.file <- .options$bat.file.ds
  spp.file <- .options$spp.file.ds
  results.variant <- new("Zvariant", bat.file = bat.file)

  correct.feature.data <- read_spp(spp.file)
  correct.weights <- correct.feature.data$weight
  expect_identical(correct.weights, sppweights(results.variant),
                   "Test variant weights not what they're supposed to")

})

test_that("Setting weights works", {
  # Use variant 4 (distribution smoothing) because it has weights
  bat.file <- .options$bat.file.ds
  results.variant <- new("Zvariant", bat.file = bat.file)

  # Generate correct and incorrect number of weights
  correct.weights <- rep(1, 7)
  incorrect.weights <- rep("n", 7)
  incorrect.n.weights <- rep(1, 5)

  sppweights(results.variant) <- correct.weights
  expect_identical(correct.weights, sppweights(results.variant),
                   "Test variant weights not what they're supposed to")

  # This should error: wrong number of weights
  expect_error(sppweights(results.variant) <- incorrect.n.weights,
               info = "Setting wrong number of weights should error")

  # This should error: wrong type of weights
  expect_error(sppweights(results.variant) <- incorrect.weights,
               info = "Setting wrong type of weights should error")

})

test_that("Assigning and fetching feature names works", {
  bat.file <- .options$bat.file
  spp.file <- .options$spp.file
  results.variant <- new("Zvariant", bat.file = bat.file)

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
  copy.results.variant <- results.variant
  # Inroduce a duplicate name
  invalid.feature.names[2] <- "Koala"
  expect_warning(featurenames(copy.results.variant) <- invalid.feature.names)

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
  results.variant <- new("Zvariant", bat.file = bat.file)

  correct.feature.data <- read_spp(spp.file)
  correct.feature.data$name <- basename(tools::file_path_sans_ext(correct.feature.data$filepath))

  # Groups
  expect_true(.hasSlot(results.variant, "groups"),
              "Test variant object doesn't have a slot 'groups'")

  correct.grp.codes <- c(1, 2, 2, 1, 2, 1, 1)
  variant.grp.codes <- groups(results.variant)
  expect_identical(variant.grp.codes, correct.grp.codes,
                   paste("Test variant group codes incorrect"))

  # Assigning groups
  new.grp.codes <- c(3, 4, 4, 3, 4, 3, 4)
  groups(results.variant) <- new.grp.codes
  expect_identical(groups(results.variant), new.grp.codes,
                   paste("Test variant newly assigned group codes incorrect"))
  # Expect error if the assigned vector is wrong length
  expect_error((groups(results.variant) <- rep(c(1, 2), 2)),
               info = "Trying to assign too few group IDs did not generate an error")
  expect_error((groups(results.variant) <- rep(c(1, 2), 10)),
               info = "Trying to assign too many group IDs did not generate an error")
  # Expect error if any of the groups is NA
  new.grp.codes[3] <- NA
  expect_error((groups(results.variant) <- new.grp.codes),
               info = "Trying to assign NA as a group ID did not generate an error")
  # Set the correct codes back
  groups(results.variant) <- correct.grp.codes

  # Variant with no groups
  no.grps.bat.file <- file.path(.options$setup.dir,
                                "03.bat")
  no.grps.results.variant <- new("Zvariant", bat.file = no.grps.bat.file)

  # Variant doesn't have groups, so there should be no group names either
  expect_true(is.na(groupnames(no.grps.results.variant)),
              "Test variant group names not NA although they haven't been set")

  # Test that setting groups when there were initially none works
  groups(no.grps.results.variant) <- correct.grp.codes
  expect_identical(groups(no.grps.results.variant), correct.grp.codes,
                   paste("Test variant group codes incorrect"))

  # Test assigning correct group names and codes
  correct.grp.names <- c("mammals", "owls")
  names(correct.grp.names) <- c(1, 2)
  groupnames(results.variant) <- correct.grp.names
  expect_identical(as.vector(correct.grp.names), groupnames(results.variant),
                   "Test variant group names not what they're supposed to")

  # NOTE that sppdata will cbind group code column to the end!
  extended.feature.data <- cbind(correct.feature.data, correct.grp.codes)
  names(extended.feature.data) <- c(names(correct.feature.data), "group")
  expect_true(all(extended.feature.data == sppdata(results.variant)),
              paste("Method sppdata doesn't return what it's supposed to"))
  # Test also using group names
  extended.feature.data <- cbind(correct.feature.data, results.variant@groups$name)
  names(extended.feature.data) <- c(names(correct.feature.data), "group.name")

  expect_true(all(extended.feature.data == sppdata(results.variant,
                                                   group.names = TRUE)),
              paste("Method sppdata doesn't return what it's supposed to"))

  # Test changing group codes with different levels of groups
  new.grp.codes <- c(1, 3, 3, 1, 2, 1, 1)
  groups(results.variant) <- new.grp.codes
  # Now create new group names
  expanded.grp.names <- c("mammals", "big.owls", "small.owls")
  names(expanded.grp.names) <- c(1, 2, 3)
  groupnames(results.variant) <- expanded.grp.names
  # Names should be reflected in results group curve headers as well
  expect_identical(sort(as.vector(expanded.grp.names)),
                   sort(groupnames(results.variant)),
                   paste("Test variant object's results does not return",
                         "the correct group names"))
  # Set the correct codes back
  groups(results.variant) <- correct.grp.codes

  # Test assigning wrong group codes
  incorrect.grp.names <- c("foo", "bar")
  names(incorrect.grp.names) <- c(4, 5)
  expect_error(groupnames(results.variant) <- incorrect.grp.names)

  # Test that method implementations for Zvariant and Zresults return the same
  # group names
  variant.results <- results(results.variant)
  expect_identical(groupnames(results.variant), groupnames(variant.results),
                   paste("Generic method groupnames does not return the same",
                         "values for group names"))
})

test_that("Getting and setting spp data works", {
  bat_file_nogrps <- .options$bat.file.no.grps
  spp_file_nogrps <- .options$spp.file.no.grps
  results_variant_nogrps <- new("Zvariant", bat.file = bat_file_nogrps)

  bat_file <- .options$bat.file
  spp_file <- .options$spp.file
  results_variant <- new("Zvariant", bat.file = bat_file)

  # Test with a variant with no results
  bat_file_noresults <- .options$bat.file.no.results
  spp_file_noresults <- .options$spp.file.no.restults
  suppressWarnings(results_variant_noresults <- new("Zvariant",
                                                    bat.file = bat_file_noresults))

  # NO GROUPS

  # Read the spp file directly for comparison
  spp_data <- read.table(spp_file_nogrps, as.is = TRUE)
  names(spp_data) <- c("weight", "alpha", "bqp", "bqp_p", "cellrem", "filepath")
  # Create featurename column
  spp_data$name <- gsub("\\.tif$", "", basename(spp_data$filepath))
  expect_equal(sppdata(results_variant_nogrps), spp_data,
               info = "Spp data without groups not returned correctly")

  # GROUPS

  # Create groups column
  spp_data_groups <- spp_data
  groups <- c(1, 2, 2, 1, 2, 1, 1)
  spp_data_groups$group <- groups
  expect_equal(sppdata(results_variant), spp_data_groups,
               info = "Spp data with groups not returned correctly")

  # Assigning spp data with incorrect number of columns should fail. NOTE that
  # assigning groups is not handled by sppdata().
  incorrect_spp_data <- data.frame(colA = 1:10, colB = 11:20)
  expect_error(sppdata(results_variant) <- incorrect_spp_data,
               "Incorrect number of columns in assigning spp data.",
               info = "Assigning wrong number of columns should cause an error.")
  # Assigning spp data with incorrect column names should fail.
  incorrect_spp_data <- spp_data
  names(incorrect_spp_data) <- paste0("foo", 1:ncol(spp_data))
  expect_error(sppdata(results_variant) <- incorrect_spp_data,
               paste0("Incorrect column names in assigning spp data. Permitted names are: ",
                      paste(names(results_variant@spp.data), collapse = ", ")),
               info = "Assigning wrong column names should cause an error.")

  # Duplicate the spp data stack, including groups
  spp_data <- rbind(spp_data, spp_data)
  # Assign the new spp data (this should work)
  suppressWarnings(sppdata(results_variant) <- spp_data)
  # However, the group information should have been defaulted
  nrows <- nrow(spp_data)
  default_group_data <-  data.frame(output.group = rep(1, nrows),
                                    condition.group = rep(-1, nrows),
                                    retention.group = rep(-1, nrows),
                                    retention.mode = rep(1, nrows),
                                    local.edge.correct.group = rep(-1, nrows),
                                    name = rep("group1", nrows))
  expect_equal(results_variant@groups, default_group_data,
               info = "Assinging spp data does not work correctly.")

  # Slot results_dirty should be TRUE.
  expect_equal(results_variant@results_dirty, TRUE,
                 info = "Changing variant data when results present should make object dirty.")


})

test_that("Retrieving variant output directory works", {
  bat.file <- .options$bat.file
  spp.file <- .options$spp.file
  test.variant <- new("Zvariant", bat.file = bat.file)

  correct.output.dir <- .options$results.dir

  expect_identical(outdir(test.variant), correct.output.dir,
                   "outdir() does not return the correct path for Zvariant")

})

test_that("Retrieving feature info works", {
  results.path <- .options$results.dir

  correct.col.names <- c("Weight", "DistSum", "IGRetained",
                         "TviolationFractRem", "DistrMeanX", "DistMeanY",
                         "MapFileName")
  correct.features.info <- read.table(file.path(results.path,
                                                "01.features_info.txt"),
                                      sep = "\t", skip = 2, as.is = TRUE,
                                      col.names = correct.col.names)

  bat.file <- .options$bat.file
  test.variant <- new("Zvariant", bat.file = bat.file)

  expect_identical(features_info(test.variant), correct.features.info,
                   "Correct features info is not returned for Zvariant")

  no.results.bat.file <- file.path(.options$setup.dir,
                                   "06.batx")
  suppressWarnings(test.no.results <- new("Zvariant", bat.file = no.results.bat.file))

  expect_warning(features_info(test.no.results))

})

test_that("Reading cost data works", {
  results.path <- .options$results.dir
  curves.file <- file.path(results.path, "01.curves.txt")

  correct.cost_data <- read.table(curves.file, skip = 1, as.is = TRUE)[,1:2]
  names(correct.cost_data) <- c("pr_lost", "cost")

  bat.file <- .options$results.bat.file
  test.variant <- new("Zvariant", bat.file = bat.file)

  expect_identical(cost(test.variant), correct.cost_data,
                   "Correct cost data is not returned for Zresults")

  no.results.bat.file <- file.path(.options$setup.dir,
                                   "06.batx")
  suppressWarnings(test.no.results <- new("Zvariant", bat.file = no.results.bat.file))

  expect_warning(cost(test.no.results))

})

test_that("Getting and setting dat data works", {
  bat_file <- .options$bat.file
  spp_file <- .options$spp.file
  test_variant <- new("Zvariant", bat.file = bat_file)

  # Get individual dat parameter values in different sections
  expect_equal(get_dat_param(test_variant, "removal rule"), "1",
               info = "Dat-file parameter value not fetched correctly.")
  expect_equal(get_dat_param(test_variant, "warp factor"), "100",
               info = "Dat-file parameter value not fetched correctly.")
  # Non-existing, but valid parameter value should cause a warning
  expect_warning(get_dat_param(test_variant, "ADMU layer file"),
                 info = "Not set, but valid parameter value should cause a warning.")
  suppressWarnings(expect_equal(get_dat_param(test_variant, "ADMU layer file"),
                                NA,
                   info = "Valid but not set parameter should return NA."))
  expect_error(get_dat_param(test_variant, "foo bar"),
               "Requested parameter not valid Zonation parameter.",
               info = "Invalid parameter value should cause an error.")

  # Set individual dat parameter values in different sections
  test_variant <- set_dat_param(test_variant, parameter = "removal rule",
                                value = "2")
  expect_equal(get_dat_param(test_variant, "removal rule"), "2",
               info = "Dat-file parameter value not set correctly.")
  # If results are present, object result state should be dirty.
  expect_equal(test_variant@results_dirty, TRUE,
               info = "Changing dat parameters should make object dirty.")
  # Also test integer value
  test_variant <- set_dat_param(test_variant, parameter = "removal rule",
                                value = 2)
  expect_equal(get_dat_param(test_variant, "removal rule"), "2",
               info = "Dat-file parameter value not set correctly.")
  # Non-existing, but valid parameter value should be ok
  condition_file <- "01/01_condition.txt"
  test_variant <- set_dat_param(test_variant, "condition file", condition_file)
  expect_equal(get_dat_param(test_variant, "condition file"), condition_file,
               info = "Not-set dat-file parameter value not set correctly.")
  # Setting invalid parameter should cause an error
  expect_error(set_dat_param(test_variant, "foo bar", "spam eggs"),
               info = "Setting invalid parameter value should cause an error.")

})

test_that("Saving Zvariant works", {
  bat_file <- .options$bat.file
  spp_file <- .options$spp.file
  test_variant <- new("Zvariant", bat.file = bat_file)

  # FIRST: change the run configuration parameter values

  # Set individual dat parameter values in different sections
  test_variant <- set_dat_param(test_variant, parameter = "removal rule",
                                value = "2")

  # SECOND: change the content of the spp file
  spp_data <- sppdata(test_variant)
  spp_data <- rbind(spp_data, spp_data)
  # Drop group column
  spp_data <- spp_data[,-ncol(spp_data)]
  suppressWarnings(sppdata(test_variant) <- spp_data)

  # Overwrite off should fail
  expect_error(save_zvariant(test_variant),
               "Cannot save: at least some of the variant files exist and overwrite is off.",
               info = "Overwriting should fail if not specified.")
  # Dir doesn't exist
  expect_error(save_zvariant(test_variant, dir = "dahjdkjsh/sajdhalsd",
                             overwrite = TRUE),
               "dir doesn't exist.",
               info = "Overwriting generate message.")
  # Successful write
  temp_variant_dir <- tempdir()
  temp_bat_file <- file.path(temp_variant_dir, basename(test_variant@bat.file))
  expect_message(save_zvariant(test_variant, dir = temp_variant_dir,
                 overwrite = TRUE),
                 info = "Overwriting generate message.")

})

context("Zvariant copying")

test_that("Zvariant with new name is copied correctly", {
  bat_file <- .options$bat.file.cond
  spp_file <- .options$spp.file.cond
  test_variant <- new("Zvariant", bat.file = bat_file)

  tmp_variant_dir <- tempdir()
  new_variant_name <- "08"

  expect_message(copied_variant <- copy_zvariant(test_variant,
                                                 name = new_variant_name,
                                                 dir = tmp_variant_dir),
                 info = "Success message not generated correctly")
})


context("Zvariant special cases")

test_that("Zvariant with condition layers is created correctly", {
  bat_file <- .options$bat.file.cond
  spp_file <- .options$spp.file.cond
  test_variant <- new("Zvariant", bat.file = bat_file)

  correct_condition_layers <- data.frame(group = c(1, 2),
                                         raster = c("../data/condition1.tif",
                                                    "../data/condition2.tif"))
  expect_equal(test_variant@condition.layers, correct_condition_layers,
               info = "Condition layers not assigned correctly.")

})

