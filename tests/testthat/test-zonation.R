context("Controlling Zonation")

test_that("Zonation executable checking works", {
  # Version not found
  expect_warning(check_zonation("zig5"),
              "Zonation executable zig5 not found in the system.")
})

