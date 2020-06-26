test_that("validate_column_mapping()'s return structure and values are correct", {

  for (obj in list(list(), vector())) {
    res <- validate_column_mapping(obj)
    expect_false(res$result)
    expect_match(res$message, "not a data\\.frame")
  }

  for (obj in list(data.frame(), data.table::data.table(), tibble::tibble())) {
    res <- validate_column_mapping(obj)
    expect_false(res$result)
    expect_match(res$message, "column.+missing")
  }

  test <- data.frame(colname = logical(), accepted_types = logical())
  res <- validate_column_mapping(test)
  expect_false(res$result)
  expect_match(res$message, "column types mismatching.+colname.+accepted_types")

  test$colname <- character()
  res <- validate_column_mapping(test)
  expect_false(res$result)
  expect_match(res$message, "^column types mismatching \\(expected\\): 'accepted_types' \\(list\\)$")

  test$accepted_types <- list()
  res <- validate_column_mapping(test)
  expect_false(res$result)
  expect_match(res$message, "must at least have one row")

  test <- tibble(colname = "foo", accepted_types = list(NA))
  res <- validate_column_mapping(test)
  expect_false(res$result)
  expect_match(res$message, "some column names not mapped to character vectors")

  test$accepted_types[[1]] <- NA_character_
  res <- validate_column_mapping(test)
  expect_false(res$result)
  expect_match(res$message, "some entries of column 'accepted_types' contain NAs")

  test$accepted_types[[1]] <- character()
  res <- validate_column_mapping(test)
  expect_false(res$result)
  expect_match(res$message, "at least one accepted type must be listed for each entry of 'colname'")

  test$accepted_types[[1]] <- "bar"
  res <- validate_column_mapping(test)
  expect_true(res$result)
  expect_length(res$message, 0)
})



test_that("check_column_types()'s return structure and values are correct", {

  test <- tibble(foo = "foo")

  res <- check_column_types(.x = test, .map = required.tweets.df.cols)
  expect_type(res, "logical")
  expect_named(res, names(test))
  expect_length(res, ncol(test))
  expect_true(is.na(res))

  test$status_id <- TRUE
  res <- check_column_types(.x = test, .map = required.tweets.df.cols)
  expect_type(res, "logical")
  expect_named(res, names(test))
  expect_length(res, ncol(test))
  expect_equivalent(res, c(NA, FALSE))

  test$status_id <- "12345"
  res <- check_column_types(.x = test, .map = required.tweets.df.cols)
  expect_equivalent(res, c(NA, TRUE))

  test$status_id <- 12345
  res <- check_column_types(.x = test, .map = required.tweets.df.cols)
  expect_equivalent(res, c(NA, FALSE))

  test$status_id <- as.integer(12345)
  res <- check_column_types(.x = test, .map = required.tweets.df.cols)
  expect_equivalent(res, c(NA, TRUE))

  test$foo <- NULL
  test$hashtags <- list(NA_character_)
  res <- check_column_types(.x = test, .map = required.tweets.df.cols)
  expect_equivalent(res, c(TRUE, TRUE))

  res <- check_column_types(.x = tweets.df.prototype, .map = required.tweets.df.cols)
  expect_true(all(res))
})

test_that("get_model_predvars()", {
  res <- get_model_predvars(ensemble.model)
  expect_equal(res, training.features$colname)

  res <- get_model_predvars(constituent.models)
  expect_equal(res, training.features$colname)

  res <- get_model_predvars(constituent.models$glmnet)
  expect_equal(res, training.features$colname)

  expect_error(get_model_predvars(list()))
})


test_that("detect_missings()", {

  test <- data.frame()
  res <- detect_missings(test)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("removed", "rows_na_map"))
  expect_null(res$removed)
  expect_null(res$rows_na_map)

  test <- data.frame(x = 1, na = NA, nan = NaN, inf = Inf)
  wrn <- capture_warning(detect_missings(test))
  expect_match(wrn$message, "1 row of `x` is incomplete")
  res <- suppressWarnings(detect_missings(test))
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("removed", "rows_na_map"))
  expect_type(res$removed, "integer")
  expect_length(res$removed, 1)
  expect_type(res$rows_na_map, "list")
  expect_length(res$removed, 1)
  expect_named(res$rows_na_map, "1")
  expect_named(res$rows_na_map[[1]], c("na", "nan", "inf"))
  expect_identical(res$rows_na_map[[1]], list("na" = "na", "nan" = "nan", "inf" = "inf"))

  test$inf <- NULL
  res <- suppressWarnings(detect_missings(test))
  expect_identical(res$rows_na_map[[1]], list("na" = "na", "nan" = "nan", "inf" = character()))

  test$nan <- NULL
  res <- suppressWarnings(detect_missings(test))
  expect_identical(res$rows_na_map[[1]], list("na" = "na", "nan" = character(), "inf" = character()))

  test$na <- "foo"
  res <- suppressWarnings(detect_missings(test))
  expect_null(res$removed)
  expect_null(res$rows_na_map)
})

