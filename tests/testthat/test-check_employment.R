test_that("Unemployed works", {
  result <- convert(values = c("9321"), from="isco_88", to = "egp", unemployed = "9321")$to
  expect_equal(result, "12")

  })


test_that("self-employed works", {
  result <- convert(values = c("9321"), from="isco_88", to = "egp", selfEmployed = "9321")$to
  expect_equal(result, "6")
})
