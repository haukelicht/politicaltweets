test_that("create_tweet_text_representations() fails if 'laserize' is not installed", {
  with_mock(
    is_installed = function(...) FALSE, .env = "rlang"
    , expect_error(create_tweet_text_representations(tweets.df.prototype), "not installed and setup")
  )
})

test_that("create_tweet_text_representations() fails with erreneous input", {

  expect_error(create_tweet_text_representations(x = list()), "needs to be a/inherit from data.frame object")

  test <- tibble::tibble(
    status_id = "12345678"
    , text = "Hallo Welt, das ist ein Fake-tweet."
  )
  expect_error(create_tweet_text_representations(x = test), "missing")
  expect_error(
    create_tweet_text_representations(
      x = test
      , .req.columns.mapping = tibble::tibble(colname = "status_id", accepted_types = list("logical"))
    )
    , "wrong classes"
  )
})

test_that("create_tweet_text_representations() returns expected object with mocked laserize()", {

  local_mock(laserize = function(...) matrix(rnorm(1024), nrow = 1, dimnames = list("test", sprintf("e%04d", 1:1024))), .env = "laserize")
  res <- create_tweet_text_representations(
      x = tweets.df.prototype
      , .keep.embeddings = FALSE
      , .compute.pcs = FALSE
      , .compute.ics = FALSE
    )
  expect_type(res, "list")
  expect_length(res, 4)
  expect_named(res, c("embeddings", "pcs", "ics", "removed"))
  expect_named(res, c("embeddings", "pcs", "ics", "removed"))
  expect_true(all(lengths(res) == 0))

  res <- create_tweet_text_representations(
      x = tweets.df.prototype
      , .keep.embeddings = TRUE
      , .compute.pcs = FALSE
      , .compute.ics = FALSE
    )
  expect_type(res, "list")
  expect_length(res, 4)
  expect_true(inherits(res$embeddings, "matrix"))
  expect_type(res$embeddings, "double")
  expect_identical(dim(res$embeddings), c(1L, 1024L))
  expect_identical(rownames(res$embeddings), "test")
  expect_identical(colnames(res$embeddings), sprintf("e%04d", 1:1024))

  res <- create_tweet_text_representations(
      x = tweets.df.prototype
      , .keep.embeddings = FALSE
      , .compute.pcs = TRUE
      , .compute.ics = TRUE
    )
  expect_type(res, "list")
  expect_length(res, 4)
  expect_true(inherits(res$pcs, "matrix"))
  expect_type(res$pcs, "double")
  expect_identical(dim(res$pcs), c(1L, 300L))
  expect_identical(rownames(res$pcs), "test")
  expect_identical(colnames(res$pcs), paste0("pc", 1:300))
  expect_true(inherits(res$ics, "matrix"))
  expect_type(res$ics, "double")
  expect_identical(dim(res$ics), c(1L, 300L))
  expect_identical(rownames(res$ics), "test")
  expect_identical(colnames(res$ics), paste0("ic", 1:300))

  test <- rbind(tweets.df.prototype, tweets.df.prototype)
  test$text[2] <- NA_character_
  res <- suppressWarnings(
    create_tweet_text_representations(
      x = test
      , .keep.embeddings = TRUE
      , .compute.pcs = TRUE
      , .compute.ics = TRUE
    )
  )
  expect_type(res, "list")
  expect_length(res, 4)
  expect_identical(dim(res$embeddings), c(1L, 1024L))
  expect_identical(dim(res$pcs), c(1L, 300L))
  expect_identical(dim(res$ics), c(1L, 300L))
  expect_length(res$removed, 1)
  expect_equal(res$removed, 2)
})

test_that("create_tweet_text_representations() returns expected object", {

  if (!rlang::is_installed("laserize")) {
    skip("laserize not installed. Run `remotes::install_github('haukelicht/laserize')` and then `laserize::setup_laser()`")
  } else {
    require(laserize)
    res <- create_tweet_text_representations(
      x = tweets.df.prototype
      , .keep.embeddings = TRUE
      , .compute.pcs = FALSE
      , .compute.ics = FALSE
      , check.languages = FALSE
    )
    expect_type(res, "list")
    expect_length(res, 4)
    expect_named(res, c("embeddings", "pcs", "ics", "removed"))
    expect_named(res, c("embeddings", "pcs", "ics", "removed"))
    expect_true(all(lengths(res[-1]) == 0))
    expect_true(inherits(res$embeddings, "matrix"))
    expect_type(res$embeddings, "double")
    expect_identical(dim(res$embeddings), c(1L, 1024L))
    # expect_identical(rownames(res$embeddings), "test")
    expect_identical(colnames(res$embeddings), sprintf("e%04d", 1:1024))
  }
})
