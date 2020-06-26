#' Create tweet text representations
#'
#' Function obtains the LASER embedding of tweet texts,
#'  as well as their principal and independent component representations.
#'
#' @param x a \code{data.frame} \code{\link[data.table]{data.table}}, or \code{\link[tibble]{tibble}}
#'      recording tweets.
#'      For required column (naming and typing conventions) refer to \code{?\link{required.tweets.df.cols}}.
#'      For an example see \code{?\link{tweets.df.prototype}}.
#' @param .keep.embeddings logical. Tweet text LASER embedding representations
#'     in return object?
#'     Default is \code{FALSE} because for each tweet a 1024 element long double vector is returned
#'     which may consume substantial amounts of working memory.
#' @param .compute.pcs logical. Project Tweet text LASER embedding onto pre-defined principal component space?
#'     Default is \code{TRUE}.
#' @param .compute.ics logical. Project Tweet text LASER embedding onto pre-defined independent component space?
#'     Default is \code{TRUE}.
#' @param .req.columns.mapping a two-column \code{data.frame} mapping column names to
#'     (character vectors specifying) expected column classes.
#'     The first column must be named \code{colname} and have type character.
#'     The second column must be a list-column of character vectors and named \code{accepted_types}.
#'     Default maps column name "status_id" to classes "character" or "integer",
#'      and "text" to "lang" to "character".
#' @param ... Additional arguments passed to \code{\link[laserize]{laserize}}
#'
#' @return A list of four elements:
#'     \describe{
#'       \item{embeddings}{
#'           If \code{.keep.embeddings = FALSE} (the default), \code{NULL}.
#'           Otherwise, a \code{matrix} recording LASER embedding representations of tweet texts.
#'           Row names correspond to \code{x$status_id} (but see Note below).
#'           Column names refer to the 1024 embedding vectors and are named "e0001", ..., "e1024".
#'
#'       }
#'       \item{pcs}{
#'           If \code{.compute.pcs = TRUE} (the default),
#'           a \code{matrix} recording principal component representation of tweet text LASER embeddings.
#'           Row names correspond to \code{x$status_id} (but see Note below).
#'           Column names refer to the 300 principal components and are named "pc1", ..., "pc300".
#'           Otherwise \code{NULL}.
#'       }
#'       \item{ics}{
#'           If \code{.compute.ics = TRUE} (the default),
#'           a \code{matrix} recording independent component representation of tweet text LASER embeddings.
#'           Row names correspond to \code{x$status_id} (but see Note below).
#'           Column names refer to the 300 independent components and are named "ic1", ..., "ic300".
#'           Otherwise \code{NULL}.
#'       }
#'       \item{removed}{integer vector, reporting the indexes of rows removed from \code{x} because \code{is.na(x$text)} is \code{TRUE}}
#'     }
#' @note Note that, strictly speaking, matrices row names do not always correspond to \code{x$status_id},
#'     as when \code{is.na(x$text)} is \code{TRUE} for some, these rows will not be contained
#'     in returned matrixes, and their status IDs will hence not be present among row names.
#'
#' @details
#'      Tweet text LASER embedding representations are obtained using \code{\link[laserize]{laserize}}
#'       (see \url{https://github.com/haukelicht/laserize}).
#'      Each text is represented by a numeric vector with 1024 elements
#'       (the dimensionality of the LASER embedding space).
#'
#'      To further reduce the dimensionality of tweet text representations,
#'       principal component analysis (PCA) and independent component analysis (ICA)
#'       can be applied by setting \code{.compute.pcs} and \code{.compute.ics} to \code{TRUE}, respectively.
#'
#'      These lower-representations are obtained by applying
#'       the rotation (PCA) and de-noising (ICA) matrixes obtained by applying
#'       PCA and ICA to the model training and validation data before training classifiers.
#'       (The resulting objects can be accessed via \code{\link{laser.embedding.prcomp}} and \code{\link{laser.embedding.icomp}}.)
#'
#'      Specifically, when training the constituent models of the ensemble classifier, instead of
#'       using the full 1024 elements long representation for texts as features,
#'       the dimensionality of the embedding space has been reduced to 300
#'       using independent component analysis (ICA) via \code{\link[fastICA]{fastICA}}.
#'
#'      To project new tweet texts' LASER embeddings onto the independent components space
#'       used during model training, we first need to obtain LASER embedding representations
#'       of the new tweets' texts,
#'       and then use the projection matrixes obtained when applying the dimensionality reduction
#'       techniques to the original model training and validation data to
#'       obtain representations of new tweets in the independent component space.
#'
#'      ICA estimates \emph{W} such that \emph{XKW = S}, where here
#'      \emph{X} is a \emph{n}-times-1024 matrix of the \emph{n} tweet text embedding re presentations,
#'       \emph{K} is a 1024-times-300 pre-whitening matrix that projects \emph{X} onto the
#'        first 300 principal components and is estimated from the original data,
#'       \emph{W} is the 300-times-300 un-mixing matrix estimated from the original data,
#'       and \emph{S} is the \emph{n}-times-300 source matrix of independent components.
#'
#'      If \code{e} is the LASER embeddings matrix of tweet texts in \code{x},
#'       we thus compute the dot product of
#'       \code{e}, \code{laser.embedding.icomp$K}, and \code{laser.embedding.icomp$W}
#'       to obtain \code{ics}, the independent component representation of \code{e}.
#'
#'
#'
#' @import laserize
#' @importFrom rlang is_installed
#' @export
create_tweet_text_representations <- function(
  x
  , .keep.embeddings = FALSE
  , .compute.pcs = TRUE
  , .compute.ics = TRUE
  , .req.columns.mapping = tibble::tribble(
       ~colname, ~accepted_types,
    "status_id", c("character", "integer"),
         "text", c("character"),
         "lang", c("character")
    )
  , ...
) {
  if (!rlang::is_installed("laserize")) {
    err_msg <- paste(
      paste("The", sQuote("laserize"), "R package is not installed and setup on your system.")
      , 'Solution: Run `remotes::install_github("haukelicht/laserize")` and refer to `?laserize::setup_laser`.'
      , sep = "\n"
    )
    stop(err_msg, call. = FALSE)
  } else {
    suppressPackageStartupMessages(require("laserize", quietly = TRUE))
  }

  stopifnot(
    "`x` needs to be a/inherit from data.frame object." = inherits(x, "data.frame")
    , "`.keep.embeddings` needs to be either TRUE or FALSE." = is.logical(.keep.embeddings) && !is.na(.keep.embeddings)
    , "`.compute.pcs` needs to be either TRUE or FALSE." = is.logical(.compute.pcs) && !is.na(.compute.pcs)
    , "`.compute.ics` needs to be either TRUE or FALSE." = is.logical(.compute.ics) && !is.na(.compute.ics)
  )

  tmp <- validate_column_mapping(.req.columns.mapping)
  if (!tmp$result)
    stop("`.req.columns.mapping` has not the expected structure/format: ", tmp$message, call. = FALSE)

  if (length(idxs_ <- which(!.req.columns.mapping$colname %in% names(x))) > 0)
    stop("The following columns are required but missing from `x`: ", col_msg(idxs_), call. = FALSE)

  col_class_check <- tryCatch(check_column_types(x, .req.columns.mapping), error = function(err) err)

  if (inherits(col_class_check, "error"))
    stop("Error raised when trying to check required columns' classes. Error message reads ", col_class_check$message, call. = FALSE)

  if (any(!na.omit(col_class_check))) {
    idxs_ <- which(.req.columns.mapping$colname %in% names(col_class_check)[!col_class_check])
    stop("The following columns of `x` have wrong classes (correct classes in parantheses): ", col_msg(idxs_, .req.columns.mapping), call. = FALSE)
  }

  out <- list(embeddings = NULL, pcs = NULL, ics = NULL, removed = which(is.na(x$text)))

  if (inherits(x, "data.table"))
    x <- as.data.frame(x[, .(status_id, text, lang)])

  # obtain LASER embeddings
  message("Obtaining LASER embedding representations of tweet texts")
  out$embeddings <- tryCatch(
    laserize(
      x = transmute(x[!is.na(x$text), ], id = status_id, text, lang = ifelse(is.na(lang), "unkown", lang))
      , simplify = TRUE
      , ...
    )
    , error = function(err) err
  )

  if (inherits(out$embeddings, "error"))
    stop("Error when trying to obtain LASER embedding representations of tweet texts. Error message reads ", out$embeddings$message)

  # assign column names
  dimnames(out$embeddings)[[2]] <- sprintf("e%04d", 1:1024)

  # obtain PCs
  if (.compute.pcs) {
    message("Obtaining principal component representation of tweet text LASER embeddings")
    out$pcs <- predict(laser.embedding.prcomp, newdata = out$embeddings)
  }

  if (.compute.ics) {
    message("Obtaining independent component representation of tweet text LASER embeddings")
    out$ics <- (out$embeddings-laser.embedding.icomp$X.means) %*% laser.embedding.icomp$K %*% laser.embedding.icomp$W
  }

  if (!.keep.embeddings)
    out <- c(list(embeddings = NULL), out[-1])

  return(out)
}
