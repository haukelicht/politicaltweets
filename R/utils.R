#' Validate column mapping
#'
#' Function validates a column mapping
#'
#' @param col.map expects a valid "column mapping",
#'     that is a data frame-like object that
#'     \enumerate{
#'        \item{inherits from \code{data.frame}}
#'        \item{has two columns named "colname" and "accepted_types"}
#'        \item{"colname" is type character and "accepted_types" is a list column}
#'        \item{has at least one row}
#'        \item{each element of "accepted_types" is a complete (non-NA) character vector}
#'        \item{the \code{lengths} of "accepted_types" is ≥ 1 for all its elements}
#'     }
#'
#' @return a list with two elements
#'     \describe{
#'        \item{result}{a logical indicating whether \code{col.map} is a valid column mapping data frame}
#'        \item{message}{a character vector, recording the reason why \code{col.map} is invalid if \code{result} is \code{FALSE}}
#'     }
validate_column_mapping <- function(col.map) {
  out <- list(result = FALSE, message = character())
  if (!inherits(col.map, "data.frame")) {
    out$message <- "not a data.frame"
    return(out)
  }
  req_names <- c("colname", "accepted_types")
  if (any(idx_ <- !req_names %in% names(col.map))) {
    out$message <- sprintf(
      "column%s %s missing."
      , ifelse(sum(idx_) > 1, "s", "")
      , paste(sQuote(req_names[idx_]), collapse = " and ")
    )
    return(out)
  }
  req_types <- c("character", "list")
  has_types <- vapply(col.map, typeof, NA_character_)
  if (any(idx_ <- !req_types == has_types[req_names])) {
    out$message <- paste(
      "column types mismatching (expected):"
      , paste(sprintf("%s (%s)", sQuote(req_names[idx_]), req_types[idx_]), collapse = " and ")
    )
    return(out)
  }
  if (nrow(col.map) == 0) {
    out$message <- "must at least have one row"
    return(out)
  }
  entries_types <- vapply(col.map$accepted_types, typeof, NA_character_)
  if (any(not_character <- entries_types != "character")) {
    idx_ <- which(not_character)
    out$message <- paste(
      "some column names not mapped to character vectors (actual vector type):"
      , paste(sprintf("%s (%s)", sQuote(col.map$colname[idx_]), entries_types[idx_]), collapse = ", ")
    )
    return(out)
  }
  any_NAs <- vapply(lapply(col.map$accepted_types, is.na), any, NA)
  if (any(any_NAs)) {
    idx_ <- which(any_NAs)
    out$message <- paste(
      "some entries of column 'accepted_types' contain NAs:"
      , paste(sQuote(col.map$colname[idx_]), collapse = ", ")
    )
    return(out)
  }
  if (any(idx_ <- lengths(col.map$accepted_types) < 1)) {
    out$message <- "at least one accepted type must be listed for each entry of 'colname'"
    return(out)
  }
  out$result <- TRUE
  return(out)
}


#' Check input data column types
#'
#' Internal helper:
#'      Function takes a data frame \code{.x}
#'      and another data frame \code{.map} that
#'      maps expected column names to vectors specifying columns' accepted/expected
#'      classes, and checks that all columns in \code{.x}
#'      have the expected classes specified in  \code{.map}
#' @param .x a \code{data.frame} to be checked
#' @param .map a two-column \code{data.frame} mapping column names to
#'     (character vectors specifying) expected column classes.
#'     The first column must be named \code{colname} and have type character.
#'     The second column must be a list-column of character vectors and named \code{accepted_types}.
#'     (see \code{?\link{required.tweets.df.cols}}).
#'
#' @return a named logical vector with length equal to \code{length(.x)}.
#'     Each element is corresponds to a column of \code{.x} (names correspond).
#'     An element is \code{TRUE}, if the class of the column in \code{.x} matches
#'      any of the classes specified in \code{.map[.map$colname == <col>, "accepted_types"]}.
#'     It is \code{NA}, if a column of \code{.x} is not mapped in \code{.map}
#'     Otherwise, it is \code{FALSE}.
check_column_types <- function(.x, .map) {
  types <- lapply(.x, typeof)
  out <- logical()
  for (c in names(types)) {
    l <- length(idx_ <- which(.map$colname == c))
    if (l == 0) {
      out[[c]] <- NA
    } else if (l == 2) {
      stop("'", c, "' matches multiple entries in `.map`")
    } else {
      out[[c]] <- any(types[[c]] %in% .map$accepted_types[[idx_]])
    }
  }
  return(out)
}


#' Create column check message
#'
#' Internal helper
#
#' @param col.idxs integer
#'
#' @return character
col_msg <- function(col.idxs, .col.map) {
  sprintf(
    "\n  - %s (%s)"
    , sQuote(.col.map$colname[col.idxs])
    , vapply(.col.map$accepted_types[col.idxs], paste, NA_character_, collapse = "/")
  )
}

#' Check dependencies
#'
#' Check if package dependencies are all installed
#'
#' @return
#' @importFrom rlang is_installed
check_dependencies <- function(pkgs) {

  installed <- vapply(pkgs$name, is_installed, NA)

  if (any(!installed)) {
    pkgs <- pkgs[which(!installed), ]

    err_msg <- sprintf(
      "%d packages required to classify tweets are not installed:\n%s"
      , nrow(pkgs)
      , paste(
        sprintf(
          " - %s (Please install via %s: `%s`)"
          , dQuote(pkgs$name)
          , pkgs$source
          , ifelse(
            pkgs$source == "CRAN"
            , sprintf('install.pacakges("%s")', pkgs$name)
            , sprintf('devtools::install_github("%s/%s")', pkgs$github_path, pkgs$name)
          )
        )
        , collapse = "\n"
      )
    )

    stop(err_msg, call. = FALSE)
  }
}

#' Get model(s) predictor variable/feature names
#'
#' Function returns names of predictor variable/features used to train \code{object}
#'
#' @param object a
#'     \code{caret} "\link[caret]{train}",
#'     \code{caretEnsemble} "\link[caretEnsemble]{caretList}",
#'     \code{caretEnsemble} "\link[caretEnsemble]{caretStack}", or
#'     \code{caretEnsemble} "\link[caretEnsemble]{caretEnsemble}" object.
#' @param ... Additional arguments ignored
#'
#' @section Model lists, stacks and ensembles:
#'     If \code{object} is a "caretList", "caretStack" or "caretEnsmeble",
#'      \code{get_model_predvars} is called on each constituent models,
#'      the returned vectors are concatenated,
#'      and \code{unique} is applied to return the variables used in any model.
#'
#' @return a character vector of model(s) predictor variable/feature names.
get_model_predvars <- function(object, ...){
  UseMethod("get_model_predvars", object)
}
#' @describeIn get_model_predvars Default method, currently not implemented.
get_model_predvars.default <- function(object, ...) {
  stop("Method not implemented for objects of class", class(object))
}
#' @describeIn get_model_predvars Method when \code{object} is a 'train' object
get_model_predvars.train <- function(object, ...) {
  names(attr(object$terms, "dataClasses"))[-1]
}
#' @describeIn get_model_predvars Method when \code{object} is a 'caretList' object
get_model_predvars.caretList <- function(object, ...) {
  unique(unlist(lapply(object, get_model_predvars.train)))
}
#' @describeIn get_model_predvars Method when \code{object} is a 'caretStack' object
get_model_predvars.caretStack <- function(object, ...) {
  get_model_predvars.caretList(object$models)
}
#' @describeIn get_model_predvars Method when \code{object} is a 'caretEnsemble' object
get_model_predvars.caretEnsemble <- function(object, ...) {
  get_model_predvars.caretList(object$models)
}




#' Detect missings
#'
#' Function detects rows of for which any column is \code{NA}, \code{NaN} or (±) \code{Inf}.
#'
#' @param .x a \code{data.frame}
#'
#' @return A list with elements
#'     \describe{
#'       \item{removed}{
#'         An integer vector of indexes of rows with any \code{NA}, \code{NaN} or (±) \code{Inf}
#'         \code{NULL} if all rows are complete (all values non-missings).
#'        }
#'       \item{rows_na_map}{
#'         \code{NULL}  if \code{removed} is \code{NULL}.
#'         Otherwise, a named list (names correspond to \code{removed}).
#'         Each list element is a list with elements "na", "nan" and "inf",
#'         each a character vector of column names with
#'         \code{NA}, \code{NaN} and (±) \code{Inf} values, respectively.
#'       }
#'     }
detect_missings <- function(.x) {
  # missing matrix
  mm <- has_na <- is.na(.x)
  # additional checks for numeric columns
  num_cols <- which(vapply(.x, is.numeric, NA))
  if (length(na.omit(num_cols)) > 0) {
    has_nan <- matrix(apply(as.data.frame(.x[, num_cols]), 2, is.nan), nrow = nrow(.x))
    has_inf <- matrix(apply(as.data.frame(.x[, num_cols]), 2, is.infinite), nrow = nrow(.x))
    colnames(has_nan) <- colnames(has_inf) <- names(.x)[num_cols]

    # determine any missings
    mm[, num_cols] <- mm[, num_cols] | has_nan | has_inf
    has_na[, num_cols] <- has_na[, num_cols] & !has_nan
  } else {
    mm <- has_na
    has_nan <- has_inf <- has_na & FALSE
  }

  out <- list(removed = NULL, rows_na_map = NULL)
  if (any(flags_ <- apply(mm, 1, any))) {
    idxs_ <- which(flags_)
    n_ <- length(idxs_)
    warning(
      paste(
        n_, ifelse(n_ == 1, "row", "rows"), "of `x`",  ifelse(n_ == 1, "is", "are"), "incomplete."
        , "Removed by listwise deletion."
        , "Attribute 'removed.rows' lists removed rows' indexes;"
        , "Attribute 'removed.rows.nas' maps removed rows' NA, NaN and Inf columns."
      )
      , immediate. = FALSE
      , call. = FALSE
    )

    out$removed <- idxs_
    out$rows_na_map <- setNames(
      lapply(idxs_, function(i) {
        ret <- list(
          na = names(which(has_na[i,]))
          , nan = names(which(has_nan[i,]))
          , inf = names(which(has_inf[i,]))
        )
        ret[is.null(ret)] <- character()
        return(ret)
      })
      , as.character(idxs_)
    )

  }

  return(out)
}
