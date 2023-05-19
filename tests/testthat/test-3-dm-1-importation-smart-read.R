test_that("get_files function handles patterns correctly", {
  # Positive tests: valid input
  path <- "/data/*yyyy*/file.txt"
  result <- get_files(path)
  expect_equal(length(result$files), length(result$dates))

  path <- "/data/*yyyy*/*mm*/file.txt"
  result <- get_files(path)
  expect_equal(length(result$files), length(result$dates))

  path <- "/data/*yyyy*/*mm*/*dd*/file.txt"
  result <- get_files(path)
  expect_equal(length(result$files), length(result$dates))

  # Negative tests: invalid input
  path <- "/data/yyyy/file.txt"
  expect_error(get_files(path), "Reading path do not contain pattern, please modify the year into *yyyy* and month into *mm*")

  path <- "/data/*yyyy*/*dd*/file.txt"
  expect_error(get_files(path), "The path contains day pattern (*dd*), but does not contain year pattern (*yyyy*) and month pattern (*mm*)")

  path <- "/data/*mm*/*dd*/file.txt"
  expect_error(get_files(path), "The path contains month pattern (*mm*), but does not contain year pattern (*yyyy*)")
})
