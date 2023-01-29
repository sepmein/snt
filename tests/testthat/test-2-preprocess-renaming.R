test_that("test the renaming of the dataframe", {
  # create a dataframe with mock Uppercase headers
  df <- tibble::tibble(
    A = 1:3,
    B = 4:6,
    C = 7:9
  )
  # rename with snt::to_lower
  df_rename <- snt::to_lower(df)
  # check if the headers are all lowercase
  expect_equal(
    colnames(df_rename),
    c("a", "b", "c")
  )
})
