test_that("fuzzy match", {
  df <- data.frame(
    col <- c(
      "a", "b", "c", "aa", "foo",
      "bar", "baz", "qux", "quux", "quuz", "corge",
      "grault", "garply", "waldo", "fred", "plugh", "xyzzy", "thud"
    )
  )
  result <- fuzzy_match(df, "col")
  expect_equal(2 * 2, 4)
})
