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
#'        \item{accepted_types}{a list column of character vectors, each element mapping the accepted type(s) to \code{colname} (see \code{?typeof})}
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

#' First 300 principal components of LASER representations of tweets in labeled dataset
#'
#' \code{\link[stats]{prcomp}} object fitted to the LASER embedding representations
#'     of tweet texts contained in the training and validation data used
#'     to obtain the ensemble classifier distributed with
#'     the \code{politicaltweets} package
#'
#' @format A \code{\link[stats]{prcomp}} object, which is a list with the following elements:
#'     \describe{
#'        \item{sdev}{standard deviations of the principal components}
#'        \item{rotation}{a \code{matrix} of variable loadings. Row names "e0001", ..., "e1024"; column names "PC1", ..., "PC300"  }
#'        \item{center}{\code{FALSE}, since no centering applied}
#'        \item{scale}{\code{FALSE}, since no scaling applied}
#'      }
#'
#' @examples
#' \dontrun{
#'  # create fake 'test' observation (1-row matrix)
#'  X <- matrix(
#'    rnorm(1024)
#'    , nrow = 1
#'    # column names must be "e0001", ..., "e1024"
#'    , dimnames = list(c("test"), sprintf("e%04d", 1:1024))
#'  )
#'
#'  # use precomp predict method to obtain PCs
#'  predict(laser.embedding.prcomp, newdata = ndat)
#' }
"laser.embedding.prcomp"

#' 300 independent components of LASER representations of tweets in labeled dataset
#'
#' \code{\link[fastICA]{fastICA}} object fitted to the LASER embedding representations
#'     of tweet texts contained in the training and validation data used
#'     to obtain the ensemble classifier distributed with
#'     the \code{politicaltweets} package
#'
#' @format A \code{\link[fastICA]{fastICA}} object, which is a list with the following elements:
#'     \describe{
#'        \item{K}{The 1024x300 'pre-whitening' \code{matrix} projecting embeddings onto the first 300 principal components}
#'        \item{W}{The 300x300 'un-mixing' \code{matrix} estimated from the original model input data}
#'        \item{A}{The 300x1024 'mixing' \code{matrix} estimated from the original model input data}
#'        \item{X.means}{a \code{double} vector with 1024 elements recording embedding vector means in the original model input data}
#'        \item{runtime}{\code{difftime} object reporting the time (in seconds) whitening and un-mixing took}
#'        \item{params}{a list mapping the parameter used when fitting \code{\link[fastICA]{fastICA}}}
#'      }
#'
#' @examples \dontrun{
#'  # create fake 'test' observation (1-row matrix)
#'  X <- matrix(
#'    rnorm(1024)
#'    , nrow = 1
#'    # column names must be "e0001", ..., "e1024"
#'    , dimnames = list(c("test"), sprintf("e%04d", 1:1024))
#'  )
#'  #'  # obtain independent component representation of x
#'  X %*% laser.embedding.icomp$K %*% laser.embedding.icomp$W
#' }
"laser.embedding.icomp"

#' Constituent model (pre-trained base learners)
#'
#' List of constituent models trained with \code{\link[caretEnsemble]{caretList}}.
#'
#' @format A list of \code{\link[caret]{train}} objects.
#'    \itemize{
#'      \item{\code{glmnet}: a generalized linear model (GLM) with Elastic-Net regularization (\code{\link[glmnet]{glmnet}})}
#'      \item{\code{svmRadial}: a Support Vector Machine (SVM) with a radial kernel (\code{\link[kernlab]{ksvm}} with \code{kernel = "rbfdot"})}
#'      \item{\code{ranger}: a Random Forest (\code{\link[range]{ranger}})}
#'      \item{\code{xgbTree}: an eXtreme Gradient Boosting (XGBoost) machine (\code{\link[xgboost]{xgboost}} with \code{learner = "tree"})}
#'    }
#' @details
#' Individual models were tuned to individually defined tuning grids using
#'       5-times repeated 10-fold cross validation to 6292 labeled tweets.
#'      "Best" models were selected among these candidates based on which
#'       tuning parameter combinations yielded the highest F1 value.
#'      These "best" candidates were then trained to the complete training data
#'       in a 10-times repeated 10-fold cross validation scheme.
"constituent.models"

#' Ensemble model (pre-trained ensemble classifier)
#'
#' Pre-trained ensemble classifier blended with \code{metric = "PR-AUC"}
#'     \code{\link{constituent.models}} using \code{\link[caretEnsemble]{caretEnsemble}}.
#'
#' @format A 'caretEnsemble' object.
#'    Element 'models' is identical to \code{\link{constituent.models}}
#'    Element 'ens_model' is the ensemble classifier induced by PR-AUC-based blending.
#'
#' @details
#' Individual models were tuned to individually defined tuning grids using
#'       5-times repeated 10-fold cross validation to 6292 labeled tweets.
#'      "Best" models were selected among these candidates based on which
#'       tuning parameter combinations yielded the highest F1 value.
#'      These "best" candidates were then trained to the complete training data
#'       in a 10-times repeated 10-fold cross validation scheme.
#'
#'      The ensemble classifier was then obtained using \code{\link[caretEnsemble]{caretEnsemble}}
#'       to blend them according to their test set performance (measured by PR-AUC) in these 100 resamples.
"ensemble.model"
