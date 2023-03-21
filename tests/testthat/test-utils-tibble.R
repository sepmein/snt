test_that("Get odd or even rows from a tibble", {
  # create a tibble
  df <- tibble::tibble(
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  # get even rows
  result <- snt::odd_or_even(df, odd = FALSE)
  result_expected <- tibble::tibble(
    x = c(2, 4, 6, 8, 10),
    y = c(2, 4, 6, 8, 10)
  )
  expect_equal(result, result_expected)
})
