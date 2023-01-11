test_that("Util Sting Slice to end", {
  target <- tibble::tibble(
    hospital = c(
      "General Hospital Awa Onna",
      "B General Hospital Awa Onna",
      "C   General Hospital Awa Onna",
      "Awa Onna"
      ))
  pattern <- "General Hospital"
  result <-
    tibble::tibble(hospital = c(
      "Awa Onna General Hospital",
      "B Awa Onna General Hospital",
      "C Awa Onna General Hospital",
      "Awa Onna"
      ))
  expect_equal(target |> str_slice_to_end(hospital, pattern), result)
})

