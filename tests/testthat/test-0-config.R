test_that("use_relative_or_absolute", {
  root <- "/Users/sepmein/x/snt"
  relative_dir <- "R"
  relative_file <- "README.md"
  non_exist_file <- "foobar.blabla"
  expect_equal(
    use_relative_or_absolute(root, relative_dir), file.path(root, relative_dir)
  )
  expect_equal(
    use_relative_or_absolute(
      root, relative_file
    ), file.path(root, relative_file)
  )
  expect_equal(
    use_relative_or_absolute(
      root, non_exist_file
    ), FALSE
  )
})

test_that("generate configuration", {
  # generate configuration
  generate_default_config(country = "nigeria")
  expect_equal(config::get("country"), "nigeria")
  # delete config.yml file
  file.remove("config.yml")
})