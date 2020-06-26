test_that("validate_train_ctrl_sum_fun", {

  res <- validate_train_ctrl_sum_fun(caret::twoClassSummary, c("yes", "no"), "A")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("result", "message"))
  expect_equivalent(lengths(res), c(1L, 1L))
  expect_false(res$result)

  res <- validate_train_ctrl_sum_fun(caret::twoClassSummary, c("yes", "no"), "ROC")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("result", "message"))
  expect_equivalent(lengths(res), c(1L, 0L))
  expect_true(res$result)

})

test_that("classify_tweets_debug_warning() raises warning", {
  expect_warning(
    classify_tweets_debug_warning(
      .x = data.frame(x = 1)
      , .predvars = training.features$colname
    )
    , "is missing the following predictors"
  )

})


test_that("classify_tweets_predict() fails with warning if predictor variables missing", {

  expect_error(
    suppressWarnings(
      classify_tweets_predict(
        model = ensemble.model
        , x = data.frame(x = 1)
        , na.rm = TRUE
        , threshold = .5
        , .predict.type = "prob"
        , .add = FALSE
        , .debug = FALSE
        , .verbose = FALSE
      )
    )
    , "Required predictor variables missing."
  )
  wrn <- capture_warning(
    classify_tweets_predict(
      model = ensemble.model
      , x = data.frame(x = 1)
      , na.rm = TRUE
      , threshold = .5
      , .predict.type = "prob"
      , .add = FALSE
      , .debug = FALSE
      , .verbose = FALSE
    )
  )
  expect_match(wrn$message, "missing some predictor variables.+again but with `.debug = TRUE`")

  # with .debug = TRUE
  expect_error(
    suppressWarnings(
      classify_tweets_predict(
        model = ensemble.model
        , x = data.frame(x = 1)
        , na.rm = TRUE
        , threshold = .5
        , .predict.type = "prob"
        , .add = FALSE
        , .debug = TRUE # !!!
        , .verbose = FALSE
      )
    )
    , "Required predictor variables missing."
  )
  wrn <- capture_warning(
    classify_tweets_predict(
      model = ensemble.model
      , x = data.frame(x = 1)
      , na.rm = TRUE
      , threshold = .5
      , .predict.type = "prob"
      , .add = FALSE
      , .debug = TRUE # !!!
      , .verbose = FALSE
    )
  )
  expect_match(wrn$message, "missing the following predictors")

})


test_that("classify_tweets_predict() removes rows with missings", {

  test <- training.data[1:3, -1]

  res <- classify_tweets_predict(
    model = ensemble.model
    , x = test
    , na.rm = TRUE
    , threshold = .5
    , .predict.type = "prob"
    , .add = FALSE
    , .debug = FALSE
    , .verbose = FALSE
  )
  expect_true(inherits(res, "data.frame"))
  expect_false(any(c("removed.rows", "removed.rows.nas") %in% names(attributes(res))))

  test[1, "ic299"] <- Inf
  test[1, "char_ratio_punct_no_urls"] <- NaN
  test[1, 1] <- NA

  wrn <- capture_warning(
    classify_tweets_predict(
      model = ensemble.model
      , x = test
      , na.rm = TRUE
      , threshold = .5
      , .predict.type = "prob"
      , .add = FALSE
      , .debug = FALSE
      , .verbose = FALSE
    )
  )
  expect_match(wrn$message, "1 row of `x` is incomplete")
  res <- suppressWarnings(
    classify_tweets_predict(
      model = ensemble.model
      , x = test
      , na.rm = TRUE
      , threshold = .5
      , .predict.type = "prob"
      , .add = FALSE
      , .debug = FALSE
      , .verbose = FALSE
    )
  )

  expect_true(inherits(res, "data.frame"))
  expect_true(all(c("removed.rows", "removed.rows.nas") %in% names(attributes(res))))
  expect_equal(attr(res, "removed.rows"), 1)
  expect_length(attr(res, "removed.rows.nas"), 1)
  expect_named(attr(res, "removed.rows.nas"), "1")
})


test_that("classify_tweets() fails with erreneous input", {

  expect_error(classify_tweets(x = list()), "needs to be a/inherit from data.frame object")
  expect_error(classify_tweets(x = data.frame(x = integer())), "needs to have at least one row")

  df <- data.frame(x = 1L)
  # check na.rm
  expect_error(classify_tweets(df, na.rm = NULL), "`na.rm` must be either TRUE or FALSE")
  expect_error(classify_tweets(df, na.rm = NA), "`na.rm` must be either TRUE or FALSE")
  expect_error(classify_tweets(df, na.rm = logical()), "`na.rm` must be either TRUE or FALSE")
  expect_error(classify_tweets(df, na.rm = c(TRUE, FALSE)), "`na.rm` must be either TRUE or FALSE")

  # check classification threshold
  expect_error(classify_tweets(df, threshold = 1L), "needs to be in \\(0, 1\\)")
  expect_error(classify_tweets(df, threshold = c(1, 2)), "needs to be in \\(0, 1\\)")
  expect_error(classify_tweets(df, threshold = -.1), "needs to be in \\(0, 1\\)")
  expect_error(classify_tweets(df, threshold = 1.1), "needs to be in \\(0, 1\\)")

  # check model list
  expect_error(classify_tweets(df, model = character()), 'Object passed to `model` has the wrong class.')
  expect_error(classify_tweets(df, model = list()), 'Object passed to `model` has the wrong class.')

  # check prediction type
  expect_error(classify_tweets(df, .predict.type = 1L), 'needs to be "prob" or "raw".')
  expect_error(classify_tweets(df, .predict.type = c("prob", "class")), 'needs to be "prob" or "raw".')
  expect_error(classify_tweets(df, .predict.type = "foo"), 'needs to be "prob" or "raw".')

  # check blending criterion input
  expect_error(classify_tweets(df, constituent.models, blend.by = 1L), 'needs to be a single character string.')
  expect_error(classify_tweets(df, constituent.models, blend.by = c("ROC", "AUC")), 'needs to be a single character string')


  # check trainControl input
  expect_error(classify_tweets(df, constituent.models, .train.ctrl = NULL), 'needs to be an object created with caret::trainControl')
  expect_error(classify_tweets(df, constituent.models, .train.ctrl = character()), 'needs to be an object created with caret::trainControl')
  expect_error(classify_tweets(df, constituent.models, .train.ctrl = list(method = "foo")), 'needs to be an object created with caret::trainControl')

  # check trainControl summary function
  if (rlang::is_installed("caret")) {
    ctrl <- caret::trainControl(summaryFunction = NULL)
    expect_error(classify_tweets(df, constituent.models, .train.ctrl = ctrl), '`.train.ctrl\\$summaryFunction` needs to be a function')
    ctrl$summaryFunction <- "foo"
    expect_error(classify_tweets(df, constituent.models, .train.ctrl = ctrl), '`.train.ctrl\\$summaryFunction` needs to be a function')
    ctrl$summaryFunction <- function() NULL
    expect_error(classify_tweets(df, constituent.models, .train.ctrl = ctrl), '`.train.ctrl\\$summaryFunction` needs to be a function')
  } else {
    skip("'caret' not installed")
  }

})

test_that("classify_tweets.caretList() fails if `x` contains wrong features", {

  req <- c("glmnet", "kernlab", "ranger", "xgboost")
  installed <- vapply(req, rlang::is_installed, NA)

  df <- data.frame(x = 1L)
  if (any(idx_ <- !installed)) {
    missing <- paste(dQuote(req[idx_]), collapse = ", ")
    res <- expect_error(classify_tweets(df, model = constituent.models), "packages required to classify tweets are not installed")
    skip(paste("Cannot test classify_tweets() with pre-trained models. The following packages are not installed:", missing))
  } else {
    wrn <- capture_warning(classify_tweets(df, constituent.models, .cache.model = FALSE, .verbose = FALSE))
    expect_match(wrn$message, "missing some predictor variables")

    wrn <- capture_warning(classify_tweets(df, constituent.models, .debug = TRUE, .cache.model = FALSE, .verbose = FALSE))
    expect_match(wrn$message, "is missing the following predictors")
  }
})


test_that("classify_tweets.caretList() return structure and value", {
  # this is actually rather a test of classify_tweets_predict
  req <- c("glmnet", "kernlab", "ranger", "xgboost", "memoise")
  installed <- vapply(req, rlang::is_installed, NA)

  if (any(idx_ <- !installed)) {
    missing <- paste(dQuote(req[idx_]), collapse = ", ")
    skip(paste("Cannot test classify_tweets() with pre-trained models. The following packages are not installed:", missing))
  } else {
    # create temporary cache directory
    tmp_cache <- file.path(".tmp_cache")
    # test
    res <- classify_tweets(training.data[1,], constituent.models, .cache.model = TRUE, .cache.path = tmp_cache)
    expect_true(is.data.frame(res))
    expect_length(res, 2)
    expect_true(nrow(res) == 1)
    expect_named(res, c("prob_yes", "class"))

    res <- classify_tweets(training.data[1,], constituent.models, se = TRUE, .cache.model = TRUE, .cache.path = tmp_cache)
    expect_true(is.data.frame(res))
    expect_length(res, 4)
    expect_true(nrow(res) == 1)
    expect_named(res, c("prob_yes", "prob_yes_025ci", "prob_yes_975ci", "class"))

    res <- classify_tweets(training.data[1,], constituent.models, .predict.type = "raw", .cache.model = TRUE, .cache.path = tmp_cache)
    expect_true(is.data.frame(res))
    expect_length(res, 1)
    expect_true(nrow(res) == 1)
    expect_named(res, c("class"))

    # clear and remove temporary cache directory
    lapply(list.files(tmp_cache, full.names = T, recursive = T, include.dirs = F), file.remove)
    file.remove(tmp_cache)
  }

})


test_that("classify_tweets.caretEnsemble() return structure and value", {
  # this is actually rather a test of classify_tweets_predict
  req <- c("glmnet", "kernlab", "ranger", "xgboost")
  installed <- vapply(req, rlang::is_installed, NA)

  if (any(idx_ <- !installed)) {
    missing <- paste(dQuote(req[idx_]), collapse = ", ")
    skip(paste("Cannot test classify_tweets() with pre-trained ensemble model. The following packages are not installed:", missing))
  } else {
    res <- classify_tweets(training.data[1,], model = ensemble.model)
    expect_true(is.data.frame(res))
    expect_length(res, 2)
    expect_true(nrow(res) == 1)
    expect_named(res, c("prob_yes", "class"))

    res <- classify_tweets(training.data[1,], model = ensemble.model, se = TRUE)
    expect_true(is.data.frame(res))
    expect_length(res, 4)
    expect_true(nrow(res) == 1)
    expect_named(res, c("prob_yes", "prob_yes_025ci", "prob_yes_975ci", "class"))

    res <- classify_tweets(training.data[1,], model = ensemble.model, .predict.type = "raw")
    expect_true(is.data.frame(res))
    expect_length(res, 1)
    expect_true(nrow(res) == 1)
    expect_named(res, c("class"))
  }
})
