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
  expect_match(res$message, "^column types mismatching \\(expected\\): ‘accepted_types’ \\(list\\)$")

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
