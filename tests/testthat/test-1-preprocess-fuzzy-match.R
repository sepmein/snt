test_that("fuzzy match", {
  df <- data.frame(col = c("a", "b", "c", "aa"))
  result <- fuzzy_match(df, "col")
  expect_equal(result, tibble::tibble(
    col = c("a", "b", "c", "aa"),
    match = c("b", "a", "a", "a")
  ))
})
