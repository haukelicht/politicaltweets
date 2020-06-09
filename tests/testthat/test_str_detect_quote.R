
test_that("str_detect_one_quote_pattern() return object has expected structure and result", {
  test <- '"hello world said Hauke" waiting "but see this is great whats up "'
  res <- politicaltweets:::str_detect_one_quote_pattern(test, pattern = text.quote.patterns[[2]])
  expect_type(res, "list")
  expect_length(res, 4)
  expect_named(res, c("quotes", "n_quotes", "n_words", "n_chars"))
  expect_true(res$quotes)
  expect_equal(res$n_quotes, 2)
  expect_length(res$n_words, 2)
  expect_identical(res$n_words, c(4L, 8L))
  expect_length(res$n_chars, 2)
  expect_identical(res$n_chars, c(23L, 32L))

  test <- 'hello world'
  res <- politicaltweets:::str_detect_one_quote_pattern(test, pattern = text.quote.patterns[[2]])
  expect_type(res, "list")
  expect_length(res, 4)
  expect_named(res, c("quotes", "n_quotes", "n_words", "n_chars"))
  expect_false(res$quotes)
  expect_equal(res$n_quotes, 0)
  expect_length(res$n_words, 0)
  expect_identical(res$n_words, integer())
  expect_length(res$n_chars, 0)
  expect_identical(res$n_chars, integer())
})

test_that("str_detect_quote_one() return object has expected structure and result", {

  test <- '"hello world said Hauke" waiting “but see this is great whats up”'
  res <- politicaltweets:::str_detect_quote_one(test)
  expect_type(res, "list")
  expect_length(res, 4)
  expect_named(res, c("quotes", "n_quotes", "n_words", "n_chars"))
  expect_true(res$quotes)
  expect_equal(res$n_quotes, 2)
  expect_length(res$n_words, 1)
  expect_identical(res$n_words, 11L)
  expect_length(res$n_chars, 1)
  expect_identical(res$n_chars, 54L)

  test <- 'hello world'
  res <- politicaltweets:::str_detect_quote_one(test)
  expect_type(res, "list")
  expect_length(res, 4)
  expect_named(res, c("quotes", "n_quotes", "n_words", "n_chars"))
  expect_false(res$quotes)
  expect_equal(res$n_quotes, 0)
  expect_equal(res$n_words, 0)
  expect_equal(res$n_chars, 0)
})


test_that("str_detect_quote() fails with errenenous input", {
  expect_error(str_detect_quote(x = list()))
  expect_error(str_detect_quote(x = data.frame()))
  expect_error(str_detect_quote(x = integer()))
  expect_error(str_detect_quote(x = factor()))
  expect_error(str_detect_quote(x = character(), quote.patterns = character()))
  expect_error(str_detect_quote(x = character(), quote.patterns = list()))
  expect_error(str_detect_quote(x = character(), quote.patterns = list(list())))
  expect_error(str_detect_quote(x = character(), quote.patterns = list(list(s = character(), e = character(), min_words = integer()))))
})

test_that("str_detect_quote() fails with errenenous input", {
  res <- str_detect_quote(x = character(), quote.patterns = list(list(s = "", e = "", min_words = 0L)))
  expect_type(res, "list")
  expect_length(res, 0L)

  res <- str_detect_quote(x = character())
  expect_type(res, "list")
  expect_length(res, 0L)

  test <- '"hello world said Hauke" waiting “but see this is great whats up”'
  res <- str_detect_quote(x = test)
  expect_type(res, "list")
  expect_length(res, 1L)
  expect_length(res[[1]], 4L)

  test <- 'hello world'
  res <- str_detect_quote(x = test)
  expect_type(res, "list")
  expect_length(res, 1L)
  expect_length(res[[1]], 4L)

})

