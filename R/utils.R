validate_column_mapping <- function(col.map) {
  out <- list(result = FALSE, message = character())
  if (!inherits(col.map, "data.frame")) {
    out$message <- "not a data.frame"
    return(out)
  }
  req_names <- c("colname", "accepted_classes")
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
  if (any(idx_ <- lengths(col.map$accepted_classes) < 1)) {
    out$message <- "at least one accepted class must be listed for each colname"
    return(out)
  }
  out$result <- TRUE
  return(out)
}


#' Check input data column classes
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
#'     The second column must be a list-column of character vectors and named \code{accepted_classes}.
#'     (see \code{?\link{required.tweets.df.cols}}).
#'
#' @return a named logical vector with length equal to \code{length(.x)}.
#'     Each element is corresponds to a column of \code{.x} (names correspond).
#'     An element is \code{TRUE}, if the class of the column in \code{.x} matches
#'      any of the classes specified in \code{.map[.map$colname == <col>, "accepted_classes"]}.
#'     It is \code{NA}, if a column of \code{.x} is not mapped in \code{.map}
#'     Otherwise, it is \code{FALSE}.
check_column_class <- function(.x, .map) {
  classes <- lapply(.x, class)
  out <- logical()
  for (c in names(classes)) {
    l <- length(idx_ <- which(.map$colname == c))
    if (l == 0) {
      out[[c]] <- NA
    } else if (l == 2) {
      stop("'", c, "' matches multiple entries in `.map`")
    } else {
      out[[c]] <- any(classes[[c]] %in% .map$accepted_classes[[idx_]])
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
    , vapply(.col.map$accepted_classes[col.idxs], paste, NA_character_, collapse = "/")
  )
}

