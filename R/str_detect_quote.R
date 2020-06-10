
#' Detect a quote pattern in text
#'
#' @param x character vector
#' @param pattern a list with elements
#'      "s" (unit-length character specifying regex to match quote start)
#'      , "e" (unit-length character specifying regex to match quote end)
#'      , and "min_words" unit-length integer specifying the min. No. words to count a match as a quote)
#'      See \code{\link{text.quote.patterns}}.
#' @param .verbose logical. verbosity
#'
#' @return A list with elements
#'    \describe{
#'        \item{quotes}{logical indicating whether or not \code{x} contains a quote that matches the quote pattern (\code{pattern})}
#'        \item{n_quotes}{non-negative integer, the number of quotes detected }
#'        \item{n_words}{non-negative integer, the number of words contained in the quote}
#'        \item{n_chars}{non-negative integer, the number of characters contained in the quote}
#'    }
#'
#' @importFrom stringr str_locate_all
str_detect_one_quote_pattern <- function(x, pattern, .verbose = FALSE) {

  out <- list(quotes = FALSE, n_quotes = 0L, n_words = integer(0L), n_chars = integer(0L))

  loc <- str_locate_all(x, pattern$s)[[1]]
  l <- nrow(loc)

  if (l == 0) return(out)

  if (.verbose) message(sprintf(" - %s ... %s", pattern$s, pattern$e))

  width <- nchar(x)
  if (l == 1) {
    loc[,2] <- width
  } else {
    loc[,2] <- c(loc[-1,1]-1L, width)
  }

  loc[,1] <- loc[,1]+1L

  if (pattern$s == pattern$e) {
    if (l == 1L) return(out)

    loc <- matrix(loc[as.logical(1:l %% 2), ], ncol = 2, byrow = FALSE)
    l <- nrow(loc)
    loc[,2] <- loc[,2] + 1L
  }

  tmp <- list("")
  i <- 1L
  while(i <= l) {
    tmp[[1]] <- substr(x, loc[i, 1], loc[i, 2])

    if (.verbose) message("    -> ",tmp[[1]])

    i <- i + 1L

    if (!grepl(pattern$e, tmp[[1]], perl = TRUE)) next

    n_words <- length(strsplit(trimws(tmp[[1]]), "\\s+")[[1]])
    if (n_words >= pattern$min_words) {
      out$quotes <- TRUE
      out$n_quotes <- out$n_quotes + 1L
      out$n_words <- c(out$n_words, n_words)
      out$n_chars <- c(out$n_chars, nchar(tmp[[1]]))
    }
  }

  return(out)
}

#' Detect quotes in a string
#'
#' @param x unit-length character vector to be parsed
#' @param .quote.patterns list of list of qquote patterns to be detected (see \code{\link{text.quote.patterns}})
#' @param .verbose logical
#'
#' @return a list object
str_detect_quote_one <- function(
  x
  , .quote.patterns = text.quote.patterns
  , .verbose = FALSE
) {
  i <- 1L
  out <- list()
  while(i <= length(.quote.patterns)) {
    out[[i]] <- str_detect_one_quote_pattern(x, pattern = .quote.patterns[[i]], .verbose = .verbose)
    out[[i]] <- vapply(out[[i]], sum, 0)
    i <- i + 1L
  }

  out <- as.list(colSums(do.call(rbind, out)))
  out[[1]] <- as.logical(out[[1]])
  out[2:4] <- lapply(out[2:4], as.integer)

  return(out)
}

#' Detect quotes in strings
#'
#' @param x character vector of strings to be parsed
#' @param quote.patterns list of list of quote patterns to be detected (see \code{\link{text.quote.patterns}})
#' @param verbose logical
#'
#' @return a list object
#'
#' @export
str_detect_quote <- function(
  x
  , quote.patterns = text.quote.patterns
  , verbose = FALSE
) {
  stopifnot(
    "`x` must be a character vector." = is.character(x)
    , "`quote.patterns` must be a list of lists (see `?text.quote.patterns`)" = is.list(quote.patterns)
    , "`quote.patterns` must be a list of lists with min. one element (see `?text.quote.patterns`)" = length(quote.patterns) > 0
    , "`quote.patterns` must be a list of lists with min. one element (see `?text.quote.patterns`)" = all(vapply(quote.patterns, is.recursive, FALSE))
    , "`quote.patterns` must be a list of lists. Child-list elements must be 's' (character), 'e' (character), and 'min_words' (integer). See `?text.quote.patterns`" =
      all(vapply(quote.patterns, function(x) identical(names(x), c("s", "e", "min_words")), FALSE))
    , "`quote.patterns` must be a list of lists. Child-list elements must be 's' (character), 'e' (character), and 'min_words' (integer). See `?text.quote.patterns`" =
      all(vapply(quote.patterns, function(x) all(lengths(x) == 1L), FALSE))
    , "`quote.patterns` must be a list of lists. Child-list elements must be 's' (character), 'e' (character), and 'min_words' (integer). See `?text.quote.patterns`" =
      all(vapply(quote.patterns, function(x) identical(vapply(x, class, "", USE.NAMES = F), c("character", "character", "integer")), FALSE))
    , "`verbose` must be either TRUE or FALSE" = verbose %in% c(TRUE, FALSE)
  )

  lapply(x, str_detect_quote_one, .quote.patterns = quote.patterns, .verbose = verbose)
}
