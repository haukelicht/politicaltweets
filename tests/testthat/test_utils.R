test_that("check_column_class()'s return structure and values are correct", {

  test <- tibble(foo = "foo")
  res <- politicaltweets:::check_column_class(.x = test, .map = required.tweets.df.cols)
  expect_type(res, "logical")
  expect_named(res, names(test))
  expect_length(res, ncol(test))
  expect_true(is.na(res))

  test$status_id <- TRUE
  res <- politicaltweets:::check_column_class(.x = test, .map = required.tweets.df.cols)
  expect_type(res, "logical")
  expect_named(res, names(test))
  expect_length(res, ncol(test))
  expect_equivalent(res, c(NA, FALSE))

  test$status_id <- "12345"
  res <- politicaltweets:::check_column_class(.x = test, .map = required.tweets.df.cols)
  expect_equivalent(res, c(NA, TRUE))

  test$foo <- NULL
  test$hashtags <- list(NA_character_)
  res <- politicaltweets:::check_column_class(.x = test, .map = required.tweets.df.cols)
  expect_equivalent(res, c(TRUE, TRUE))

  res <- politicaltweets:::check_column_class(.x = tweets.df.prototype, .map = required.tweets.df.cols)
  expect_true(all(res))
})
