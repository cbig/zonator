context("Controlling Zonation")

test_that("Zonation executable checking works", {
  # Zig3
  expect_true(check_zonation(), 
              "Zonation v3 (zig3) not found in the system.")
  # Zig4
  expect_true(check_zonation("zig4"), 
              "Zonation v4 (zig4) not found in the system.")
  # Version not found
  expect_warning(check_zonation("zig5"),
              "Zonation executable zig5 not found in the system.")
})