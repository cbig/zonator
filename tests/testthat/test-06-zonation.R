context("Controlling Zonation")

test_that("Zonation executable checking works", {
  # Version not found
  expect_warning(check_zonation("zig5"),
                 info="Exe-not-found error not reported right")
})
