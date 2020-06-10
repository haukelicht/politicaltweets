#' Text quote patterns
#'
#' Quote pattern beginnings and ends
#'
#' @format A list of list of quote patterns beginnings and ends.
#' Each top-level list element is a list with elements
#' \describe{
#'   \item{s}{a unit-length character, specifying quote pattern beginning}
#'   \item{e}{a unit-length character,quote pattern ending}
#'   \item{min_words}{a unit-length non-negative integer, specifying how many words the quote pattern must at least entail to be counted a quote}
#' }
"text.quote.patterns"

#' Required tweets data frame columns
#'
#' Required tweets data frame columns
#'
#' @format A \code{\link[tibble]{tibble}} with columns
#'      \describe{
#'        \item{colname}{a character vector, specifying required column names}
#'        \item{accepted_classes}{a list column of character vectors, each element mapping the accepted classed to \code{colname}}
#'      }
"required.tweets.df.cols"

#' Tweets data frame prototype
#'
#' A prototypical \bold{tweets data frame} as expected, for example, by
#'     \code{\link{creat_tweet_features}}.
#'
#' @format A \code{\link[tibble]{tibble}} with columns
#'      \describe{
#'         \item{status_id}{("character" or "integer") tweet status ID}
#'         \item{created_at}{("POSIXt", "integer", or "character") tweet creation time}
#'         \item{text}{("character") tweet text}
#'         \item{display_text_width}{("integer") the character width of a tweet's text on display}
#'         \item{lang}{("character") tweet language}
#'         \item{reply_to_status_id}{("character", "integer") status ID of tweet the current entity replied to, if applicable (otherwise \code{NA})}
#'         \item{is_retweet}{("logical") is a retweet?}
#'         \item{is_quote}{("logical") is a quote?}
#'         \item{hashtags}{("list") list-column of character vectors of hashtags contained in the tweet}
#'         \item{symbols}{("list") list-column of character vectors of symbols contained in the tweet}
#'         \item{urls_expanded_url}{("list") list-column of character vectors of expanded (not short) URLs of hyperlinks contained in the tweet}
#'         \item{media_expanded_url}{("list") list-column of character vectors of expanded (not short) URLs of media contained in the tweet}
#'         \item{ext_media_expanded_url}{("list") list-column of character vectors of expanded (not short) URLs of media attached to the tweet}
#'         \item{media_type}{("list") list-column of character vectors of media types ("photo", "video" or \code{NA}) attached to the tweet}
#'         \item{favorite_count}{("integer") number of times tweet was marked as favorite}
#'         \item{retweet_count}{("integer") number of times tweet was retweeted}
#'         \item{quote_count}{("integer")  number of times tweet was quoted}
#'         \item{reply_count}{("integer")  number of times users replied to the tweet}
#'     }
#' @note These are the naming and typing conventions of the \code{rtweet} package \url{https://rtweet.info/}.
#'     For a full description of what these fields/columns record, see \url{https://developer.twitter.com/en/docs/tweets/data-dictionary/overview/intro-to-tweet-json}
"tweets.df.prototype"
