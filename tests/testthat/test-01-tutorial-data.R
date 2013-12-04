context("Tutorial data existence")

test_that("Zonation tutorial data is available", {
  expect_true(file.exists(.options$tutorial.dir), 
              "Tutorial directory not found in root of the package.")
  expect_true(file.exists(.options$setup.dir),
              "Input setup directory not found in the tutorial folder.")
  expect_true(file.exists(.options$data.dir),
              "Input data directory not found in the tutorial folder.")
  expect_true(file.exists(.options$output.dir),
              "Output directory not found in the tutorial folder.")
  expect_true(file.exists(.options$bat.file), 
              "Tutorial batch file not found in the tutorial folder.")
  expect_true(file.exists(.options$dat.file), 
              "Tutorial dat file not found in the tutorial input folder.")
  expect_true(file.exists(.options$spp.file), 
              "Tutorial spp file not found in the tutorial input folder.")
  
  for (species in .options$species.files) {
    expect_true(file.exists(species), 
                paste("Tutorial species raster", species, 
                      "not found in the tutorial input folder."))
  }
  
  expect_true(file.exists(.options$groups.file), 
              "Tutorial groups file not found in the tutorial input folder.")
  
})
