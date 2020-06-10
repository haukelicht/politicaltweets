
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

test_that("compute_tweet_features()'s return structure and values are correct", {

  res <- politicaltweets:::compute_tweet_features(x = tweets.df.prototype)
  expect_true(inherits(res, "data.table"))
  expect_true(inherits(res, "data.frame"))
  expect_true(all(names(tweets.df.prototype) %in% names(res)))

  res <- politicaltweets:::compute_tweet_features(x = tweets.df.prototype, .as.data.table = FALSE)
  expect_true(inherits(res, "tbl_df"))
  expect_true(inherits(res, "data.frame"))

  expect_true(all(vapply(res[grep("^(has|is)_", names(res))], is.logical, FALSE)))
  expect_true(all(vapply(res[grep("^n_|_count$", names(res))], is.integer, FALSE)))
  expect_true(all(vapply(res[grep("^nchar($|_.+)", names(res))], is.integer, FALSE)))
  expect_true(all(vapply(res[grep("^char_ratio", names(res))], is.double, FALSE)))
})

test_that("create_tweet_features() fails with erreneous input", {

  expect_error(create_tweet_features(x = list()), "needs to be a/inherit from data.frame object")

  test <- tibble::tibble(
    status_id = "12345678"
    , text = "Hallo Welt, das ist ein Fake-tweet."
    , created_at = lubridate::now()
  )
  expect_error(politicaltweets:::create_tweet_features(x = test), "missing")
  expect_error(
    politicaltweets:::create_tweet_features(
      x = test
      , .req.columns.mapping = tibble::tibble(colname = "status_id", accepted_classes = "logical")
    )
    , "wrong classes"
  )

})
