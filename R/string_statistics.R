

#' Compute Emoji statistics for string
#'
#' @param x character vector, vectorized over elements
#' @param emojis.vec character vector specifying Emoji Unicode values to
#'    be matched and extracted.
#'    Defaults to column "code" of the \code{\link[rtweet]{emojis}} data.
#'
#' @return A list (of lists) with elements
#'    \describe{
#'        \item{ratio}{the ratio of Emoji characters relative to all characters of the string}
#'        \item{count}{the number of Emoji characters}
#'        \item{emojis}{a character vector recording all Emojis matched in (the current element of) `x`}
#'    }
#'
#' @importFrom stringi stri_enc_mark
str_emoji_stats <- Vectorize(function(x, emojis.vec = rtweet::emojis$code) {
  if (stri_enc_mark(x) != "UTF-8")
    return(list(ratio = 0.0, count = 0L, emojis = character()))

  split <- strsplit(x, "")[[1]]
  res <- match(split, emojis.vec)

  if (all(is.na(res)))
    return(list(ratio = 0.0, count = 0L, emojis = character()))

  out <- list()

  out$ratio <- mean(!is.na(res))
  out$count <- sum(!is.na(res))
  out$emojis <- emojis.vec[na.omit(res)]

  return(out)
}, vectorize.args = "x", USE.NAMES = FALSE, SIMPLIFY = FALSE)

#' Compute Unicode character count
#'
#' @param x character vector, vectorized over elements
#'
#' @return A numeric (integer) vector
#'
#' @importFrom stringi stri_enc_mark
str_unicode_count <- Vectorize(function(x) {
  sum(stri_enc_mark(strsplit(x, "")[[1]]) == "UTF-8")
}, USE.NAMES = FALSE, SIMPLIFY = TRUE, vectorize.args = "x")

#' Compute Unicode character ratio
#'
#' @param x character vector, vectorized over elements
#'
#' @return A numeric (double) vector
#'
#' @importFrom stringi stri_enc_mark
str_unicode_char_ratio <- Vectorize(function(x) {
  mean(stri_enc_mark(strsplit(x, "")[[1]]) == "UTF-8")
}, USE.NAMES = FALSE, SIMPLIFY = TRUE, vectorize.args = "x")

#' Compute number of characters matching pattern
#'
#' @param x character vector, vectorized over elements
#' @param pattern unit-length character vector, a pattern (regex) to be matched in \code{x}
#'
#' @return An numeric (integer) vector
#'
#' @importFrom stringr str_extract_all
#' @importFrom purrr map_chr
str_pattern_width <- function(x, pattern) {
  nchar(map_chr(str_extract_all(x, pattern), paste, collapse = ""), type = "chars")
}

