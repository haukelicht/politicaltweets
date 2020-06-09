test_that("str_unicode_count() return object has expected structure and result" , {

  test <- c(
    "Hi 🤪"
    , "Bye 🤪 ♥️"
    , "what up"
    , "well ümlaut"
    , "12 pm"
  )

  res <- politicaltweets:::str_unicode_count(test)
  expect_type(res, "integer")
  expect_length(res, 5)
  expect_identical(res, c(1L, 3L, 0L, 1L, 0L))
})

test_that("str_unicode_char_ratio() return object has expected structure and result" , {

  test <- c(
    "Hi 🤪"
    , "Bye 🤪 ♥️"
    , "what up"
    , "well ümlau"
    , "12 pm"
  )

  res <- politicaltweets:::str_unicode_char_ratio(test)
  expect_type(res, "double")
  expect_length(res, 5)
  expect_identical(res, c(1/4, 3/8, 0, 1/10, 0))
})
