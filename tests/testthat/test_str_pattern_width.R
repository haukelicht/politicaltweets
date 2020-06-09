test_that("str_pattern_width() return object has expected structure and result", {
  test <- c(
    paste0(letters, LETTERS, collapse = "")
    , paste(letters, collapse = "")
    , paste(LETTERS, collapse = "")
  )

  res <- politicaltweets:::str_pattern_width(test, "[[:upper:]]")
  expect_type(res, "integer")
  expect_length(res, 3L)
  expect_identical(res, c(26L, 0L, 26L))
})
