test_that("Faster mean get data", {
  expect_equal(faster_mean(c(1, 2, 3)), 2)
  expect_error(faster_mean("hello"))
})
