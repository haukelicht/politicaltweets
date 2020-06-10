
#' Compute tweet features
#'
#' Internal helper to \code{\link{create_tweet_features}}
#'
#' @param x a \code{data.frame} \code{\link[data.table]{data.table}}, or \code{\link[tibble]{tibble}}
#'      recording tweets.
#'      For required column (naming and typing conventions) refer to \code{?\link{required.tweets.df.cols}}.
#'      For an example see \code{?\link{tweets.df.prototype}}.
#' @param .as.data.table logical. Whether or not to return a \code{\link[data.table]{data.table}}.
#'      Defaults to \code{TRUE}.
#'      If \code{FALSE}, the returned object is a \code{\link[tibble]{tibble}}.
#' @param .mentions.regexp unit-length character vector,
#'      specifying the regular expression pattern used to match, count, and remove
#'      mentions in the tweet text (applied to column \code{text}).
#' @param .hashtags.regexp unit-length character vector,
#'      specifying the regular expression pattern used to match, count, and remove
#'      hashtags in the tweet text (applied to column \code{text}).
#' @param .url.regexp unit-length character vector,
#'      specifying the regular expression pattern used to match, count, and remove
#'      URLs in the tweet text (applied to column \code{text}).
#'
#' @return A \code{\link[data.table]{data.table}} if \code{.as.data.table = TRUE} (default),
#'      otherwise a \code{\link[tibble]{tibble}}.
#'      The return object contains all columns contained in \code{x} \emph{plus}
#'       the created tweet features.
#'
#' @import dplyr
#' @import dtplyr
#' @importFrom data.table as.data.table
#' @importFrom lubridate as_date
#' @importFrom stringr str_count str_replace_all
#' @importFrom purrr map map_int map2_int map_lgl discard
#' @importFrom tidyr replace_na
compute_tweet_features <- function(
  x
  , .as.data.table = TRUE
  , .mentions.regexp = "(?<=^|\\W)(@\\w{1,15})(?=\\s|$|\\W)"
  , .hashtags.regexp = "(?<=\\.|^|\\s)(#\\w{1,139})(?=\\s|$|\\W)"
  , .url.regexp = "\\b(([a-z][\\w-]+:(/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)([^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\\'\\\".,<>?«»“”‘’]))"
) {
  if (.as.data.table)
    require(dplyr)
  preprep <- ifelse(.as.data.table, lazy_dt, dplyr::as_tibble)
  postprep <- ifelse(.as.data.table, as.data.table, identity)
  x %>%
    # declare lazy data.table
    preprep() %>%
    # filter(tweet_exists) %>%
    mutate(
      # tweet date
      date = as_date(created_at)
      # text w/o URLs
      , text_no_urls = str_replace_all(text, c("\\bhttps?://\\S+" = "", "\\bwww\\.\\S+" = ""))
      , nchar_no_urls = nchar(text_no_urls, type = "chars")
      # is reply to other tweet?
      , is_reply = !is.na(reply_to_status_id)
      # is indirect, i.e., retweet, quote or reply
      , is_indirect = as.logical(is_retweet + is_quote + is_reply)
      # No. mentions
      , n_mentions = str_count(text, .mentions.regexp)
      # No. hashtags
      , n_hashtags = lengths(map(hashtags, discard, is.na))
      # No. symbols
      , n_symbols = lengths(map(symbols, discard, is.na))
      # No. URLs
      , n_urls1 = lengths(map(urls_expanded_url, discard, is.na))
      , n_urls2 = str_count(text, .url.regexp)
      # No. media
      , temp1 = lengths(map(media_expanded_url, discard, is.na))
      , temp2 = lengths(map(ext_media_expanded_url, discard, is.na))
      , n_media = temp1 + temp2
      # has media?
      , has_media = n_media > 0
      # has photo?
      , has_photo = !is.na(map_int(media_type, match, x = "photo"))
      # has video?
      , has_video = !is.na(map_int(media_type, match, x = "video"))
      # tweet text character-based statistics:
      # No. characters
      , nchar = nchar(text, type = "chars")
      # No. alnum characters
      , nchar_alnum = str_pattern_width(text, "[[:alnum:]]")
      , nchar_alnum_no_urls = str_pattern_width(text_no_urls, "[[:alnum:]]")
      # No. punctuation characters
      , nchar_punct = str_pattern_width(text, "[[:punct:]]")
      , nchar_punct_no_urls = str_pattern_width(text_no_urls, "[[:punct:]]")
      # No. spacec characters
      , nchar_space = str_pattern_width(text, "[[:space:]]")
      # combined No. character length of all mentions
      , nchar_mentions = str_pattern_width(text, .mentions.regexp)
      # combined No. character length of all hashtags
      , nchar_hashtags = str_pattern_width(text, .hashtags.regexp)
      # compute emoji stats (list-column, unpacked below)
      , emoji_stats = str_emoji_stats(text)
      # No. characters
      , nchar_unicode = str_unicode_count(text)
      , char_ratio_unicode = str_unicode_char_ratio(text)
      # quotes statistics
      , quote_stats = str_detect_quote(text)
    ) %>%
    mutate_at(vars(ends_with("_count")), replace_na, 0L) %>%
    # we need to separate this block of computing on created columns to make the query data.table-comfortable
    mutate(
      # No. emojis
      n_emojis = map_int(emoji_stats, "count")
      , nchar_emojis = n_emojis
      # in-text quotes
      , has_quotes = map_lgl(quote_stats, "quotes")
      , n_quotes = map_int(quote_stats, "n_quotes")
      , nchar_quotes = map_int(quote_stats, "n_chars")
      , n_urls = map2_int(n_urls1, n_urls2, max)
      # has URL(s) ?
      , has_urls = n_urls > 0
      , has_mentions = n_mentions > 0
      , has_hashtags = n_hashtags > 0
      , has_symbols = n_symbols > 0
      , has_emojis = n_emojis > 0
    ) %>%
    # compute width ration (width/nchar of text occupied by character type vs total text width/nchar)
    mutate(
      # alpha-numeric character width/total text width
      char_ratio_alnum = nchar_alnum/nchar
      , char_ratio_alnum_no_urls = nchar_alnum_no_urls/nchar_no_urls
      , char_ratio_alnum_dwidth = nchar_alnum_no_urls/display_text_width
      # punctuation character width/total text width
      , char_ratio_punct = nchar_punct/nchar
      , char_ratio_punct_no_urls = nchar_punct_no_urls/nchar_no_urls
      , char_ratio_punct_dwidth = nchar_punct_no_urls/display_text_width
      # space character width/total width
      , char_ratio_space = nchar_space/nchar
      , char_ratio_space_dwidth = nchar_space/display_text_width
      # total width of mentions/total text width
      , char_ratio_mentions = nchar_mentions/nchar
      , char_ratio_mentions_no_urls = nchar_mentions/nchar_no_urls
      # total width of hasthags/total text width
      , char_ratio_hashtags = nchar_hashtags/nchar
      , char_ratio_hashtags_no_urls = nchar_hashtags/nchar_no_urls
      # total width of emojis/total text width
      , char_ratio_emojis = nchar_emojis/nchar
      , char_ratio_emojis_no_urls = nchar_emojis/nchar_no_urls
      # total width of quotes/total text width
      , char_ratio_quotes = nchar_quotes/nchar
      , char_ratio_quotes_no_urls = nchar_quotes/nchar_no_urls
      , char_ratio_quotes_dwidth = nchar_quotes/display_text_width
    ) %>%
    mutate(nchar_display = display_text_width) %>%
    # keep the following columns
    select(
      # -- input columns -- #
      !!setdiff(colnames(x), c("lang", "is_reply", "is_retweet", "is_quote", "favorite_count", "retweet_count", "quote_count", "reply_count"))
      # -- generated columns -- #
      # tweet metadata-level
      , date
      , lang
      , is_reply
      , is_retweet
      , is_quote
      , is_indirect
      , favorite_count
      , retweet_count
      , quote_count
      , reply_count
      , n_media
      , has_media
      , has_photo
      , has_video
      # tweet text-level
      , nchar
      , nchar_no_urls
      , nchar_display
      , n_mentions
      , n_hashtags
      , n_emojis
      , n_symbols
      , n_urls
      , n_quotes
      , has_mentions
      , has_hashtags
      , has_emojis
      , has_symbols
      , has_urls
      , has_quotes
      ## character type counts
      , nchar_alnum
      , nchar_punct
      , nchar_space
      , nchar_mentions
      , nchar_hashtags
      , nchar_emojis
      , nchar_quotes
      , nchar_unicode
      , nchar_alnum_no_urls
      , nchar_punct_no_urls
      ## character type ratios
      , char_ratio_unicode
      , char_ratio_alnum
      , char_ratio_punct
      , char_ratio_space
      , char_ratio_mentions
      , char_ratio_hashtags
      , char_ratio_emojis
      , char_ratio_quotes
      , char_ratio_alnum_no_urls
      , char_ratio_punct_no_urls
      , char_ratio_mentions_no_urls
      , char_ratio_hashtags_no_urls
      , char_ratio_emojis_no_urls
      , char_ratio_quotes_no_urls
      , char_ratio_alnum_dwidth
      , char_ratio_punct_dwidth
      , char_ratio_space_dwidth
      , char_ratio_quotes_dwidth
    ) %>%
    postprep()
}


#' Create tweet features
#'
#' Given a data frame of tweets data, \code{create_tweet_features()} creates tweet features.
#'
#' @param x a \code{data.frame} \code{\link[data.table]{data.table}}, or \code{\link[tibble]{tibble}}
#'      recording tweets.
#'      For required column (naming and typing conventions) refer to \code{?\link{required.tweets.df.cols}}.
#'      For an example see \code{?\link{tweets.df.prototype}}.
#'
#' @param .req.columns.mapping a two-column \code{data.frame} mapping column names to
#'     (character vectors specifying) expected column classes.
#'     The first column must be named \code{colname} and have type character.
#'     The second column must be a list-column of character vectors and named \code{accepted_classes}.
#'     (see \code{?\link{required.tweets.df.cols}}).
#'
#' @param ... Additional arguments passed to \code{\link{compute_tweet_features}}.
#'
#' @return A \code{\link[data.table]{data.table}}.
#'      If, in addition, you pass the parameter-argument mapping \code{.as.data.table = FALSE}
#'      (e.g. \code{create_tweet_features(df, .as.data.table = FALSE)}),
#'       the return object is a \code{\link[tibble]{tibble}} instead.
#'
#' @note Names and types/classes of \code{x} are expected to match the values mapped in
#'     \code{\link{required.tweets.df.cols}} (for an example, see \code{\link{tweets.df.prototype}}).
#'     An error will be raised if
#'      (a) any required column is missing (column name-based check),
#'      or (b) any the class of any required column does not match any of the accepted classes.
#'
#' @section Complementary with the \code{rtweet} package:
#'     The column naming and typing conventions (see \code{\link{required.tweets.df.cols}})
#'      conform with the data returned by \code{rtweet} functions
#'      \code{\link[rtweet]{lookup_statuses}} and \code{\link[rtweet]{lookup_tweets}}
#'      if called with \code{parse = TRUE} (the default behavior).
#'
#'
#' @export
create_tweet_features <- function(
  x
  , .req.columns.mapping = required.tweets.df.cols
  , ...
) {

  stopifnot("`x` needs to be a/inherit from data.frame object." = inherits(x, "data.frame"))

  tmp <- validate_column_mapping(.req.columns.mapping)
  if (!tmp$result)
    stop("`.req.columns.mapping` has not the expected structure/format: ", tmp$message, call. = FALSE)

  if (length(idxs_ <- which(!.req.columns.mapping$colname %in% names(x))) > 0)
    stop("The following columns are required but missing from `x`: ", col_msg(idxs_), call. = FALSE)

  col_class_check <- tryCatch(check_column_class(x, .req.columns.mapping), error = function(err) err)

  if (inherits(col_class_check, "error"))
    stop("Error raised when trying to check required columns' classes. Error message reads ", col_class_check$message, call. = FALSE)

  if (any(!na.omit(col_class_check))) {
    idxs_ <- which(.req.columns.mapping$colname %in% names(col_class_check)[!col_class_check])
    stop("The following columns of `x` have wrong classes (correct classes in parantheses): ", col_msg(idxs_, .req.columns.mapping), call. = FALSE)
  }

  compute_tweet_features(x)

}
