test_that("return object has expected structure" , {

  test <- c(
    "Hi 🤪"
    , "Bye 🤪 ♥️"
    , "what up"
    , "well ümlaut"
    , "12 pm"
  )

  res <- politicaltweets:::str_emoji_stats(test)
  expect_type(res, "list")
  expect_length(res, 5)
  expect_true(all(lengths(res) == 3))
  expect_true(all(lengths(res) == 3))
  expect_true(all(vapply(res, function(x) identical(names(x), c("ratio", "count", "emojis")), FALSE)))
  expect_identical(purrr::map_int(res, "count"), c(1L, 2L, 0L, 0L, 0L))
})
