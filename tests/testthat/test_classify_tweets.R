test_that("validate_train_ctrl_sum_fun", {

  res <- politicaltweets:::validate_train_ctrl_sum_fun(caret::twoClassSummary, c("yes", "no"), "A")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("result", "message"))
  expect_equivalent(lengths(res), c(1L, 1L))
  expect_false(res$result)

  res <- politicaltweets:::validate_train_ctrl_sum_fun(caret::twoClassSummary, c("yes", "no"), "ROC")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("result", "message"))
  expect_equivalent(lengths(res), c(1L, 0L))
  expect_true(res$result)

})

test_that("classify_tweets_debug_warning()", {

  res <- politicaltweets:::validate_train_ctrl_sum_fun(caret::twoClassSummary, c("yes", "no"), "A")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("result", "message"))
  expect_equivalent(lengths(res), c(1L, 1L))
  expect_false(res$result)

  res <- politicaltweets:::validate_train_ctrl_sum_fun(caret::twoClassSummary, c("yes", "no"), "ROC")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("result", "message"))
  expect_equivalent(lengths(res), c(1L, 0L))
  expect_true(res$result)

})


test_that("classify_tweets() fails with erreneous input", {

  expect_error(classify_tweets(x = list()), "needs to be a/inherit from data.frame object")
  expect_error(classify_tweets(x = data.frame(x = integer())), "needs to have at least one row")

  df <- data.frame(x = 1L)
  # check classification threshold
  expect_error(classify_tweets(df, classification.threshold = 1L), "needs to be in \\(0, 1\\)")
  expect_error(classify_tweets(df, classification.threshold = c(1, 2)), "needs to be in \\(0, 1\\)")
  expect_error(classify_tweets(df, classification.threshold = -.1), "needs to be in \\(0, 1\\)")
  expect_error(classify_tweets(df, classification.threshold = 1.1), "needs to be in \\(0, 1\\)")

  # check blending criterion input
  expect_error(classify_tweets(df, blend.by = 1L), 'needs to be a single character string.')
  expect_error(classify_tweets(df, blend.by = c("ROC", "AUC")), 'needs to be a single character string')

  # check prediction type
  expect_error(classify_tweets(df, .predict.type = 1L), 'needs to be "prob" or "raw".')
  expect_error(classify_tweets(df, .predict.type = c("prob", "class")), 'needs to be "prob" or "raw".')
  expect_error(classify_tweets(df, .predict.type = "foo"), 'needs to be "prob" or "raw".')

  # check model list
  expect_error(classify_tweets(df, .model.list = character()), 'needs to be a "caretList" object')
  expect_error(classify_tweets(df, .model.list = list()), 'needs to be a "caretList" object')
  expect_error(classify_tweets(df, .model.list = as.list(LETTERS)), 'needs to be a "caretList" object')

  # check trainControl input
  expect_error(classify_tweets(df, .train.ctrl = NULL), 'needs to be an object created with caret::trainControl')
  expect_error(classify_tweets(df, .train.ctrl = character()), 'needs to be an object created with caret::trainControl')
  expect_error(classify_tweets(df, .train.ctrl = list(method = "foo")), 'needs to be an object created with caret::trainControl')

  # check trainControl summary function
  ctrl <- caret::trainControl(summaryFunction = NULL)
  expect_error(classify_tweets(df, .train.ctrl = ctrl), '`.train.ctrl\\$summaryFunction` needs to be a function')
  ctrl$summaryFunction <- "foo"
  expect_error(classify_tweets(df, .train.ctrl = ctrl), '`.train.ctrl\\$summaryFunction` needs to be a function')
  ctrl$summaryFunction <- function() NULL
  expect_error(classify_tweets(df, .train.ctrl = ctrl), '`.train.ctrl\\$summaryFunction` needs to be a function')

})

test_that("classify_tweets() fails if `x` contains wrong features", {

  req <- c("glmnet", "kernlab", "ranger", "xgboost")
  installed <- vapply(req, rlang::is_installed, NA)

  df <- data.frame(x = 1L)
  if (any(idx_ <- !installed)) {
    missing <- paste(dQuote(req[idx_]), collapse = ", ")
    res <- expect_error(classify_tweets(df, .model.list = constituent.models), "packages required to classify tweets are not installed")
    skip(paste("Cannot test classify_tweets() with pre-trained models. The following packages are not installed:", missing))
  } else {
    cond <- capture_condition(classify_tweets(df, .cache.model = FALSE, .verbose = FALSE))
    expect_true(inherits(cond, "warning"))
    expect_match(cond$message, "missing some predictor variables")

    cond <- capture_condition(classify_tweets(df, .debug = TRUE, .cache.model = FALSE, .verbose = FALSE))
    expect_true(inherits(cond, "warning"))
    expect_match(cond$message, "is missing the following predictors")
  }
})


test_that("classify_tweets() return structure and value", {

  req <- c("glmnet", "kernlab", "ranger", "xgboost")
  installed <- vapply(req, rlang::is_installed, NA)

  if (any(idx_ <- !installed)) {
    missing <- paste(dQuote(req[idx_]), collapse = ", ")
    skip(paste("Cannot test classify_tweets() with pre-trained models. The following packages are not installed:", missing))
  } else {
    res <- classify_tweets(training.data[1,], .cache.model = FALSE)
    expect_true(is.data.frame(res))
    expect_length(res, 2)
    expect_true(nrow(res) == 1)
    expect_named(res, c("prob_yes", "class"))

    res <- classify_tweets(training.data[1,], se = TRUE, .cache.model = FALSE)
    expect_true(is.data.frame(res))
    expect_length(res, 4)
    expect_true(nrow(res) == 1)
    expect_named(res, c("prob_yes", "prob_yes_025ci", "prob_yes_975ci", "class"))

    res <- classify_tweets(training.data[1,], .predict.type = "raw", .cache.model = FALSE)
    expect_true(is.data.frame(res))
    expect_length(res, 1)
    expect_true(nrow(res) == 1)
    expect_named(res, c("class"))
  }
})
