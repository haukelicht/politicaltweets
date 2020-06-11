
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
  expect_true(all(vapply(res[grep("^nchar($|_.+)", names(res))], typeof, NA_character_) == "integer"))
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
      , .req.columns.mapping = tibble::tibble(colname = "status_id", accepted_classes = list("logical"))
    )
    , "wrong classes"
  )

})
