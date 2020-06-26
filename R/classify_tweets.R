# helpers ----

#' Validate \code{trainControl} summary function
#'
#' Internal helper to \code{\link{classify_tweets}}
#'
#' @param fun summary function, needs to conform to \code{caret} standards see \code{?\link[caret]{twoClassSummary}}
#' @param levs character with two elements, specifying outcome classes
#' @param metric evaluation metric expected in \code{fun}'s return object
#'
#' @return a list with elements "result" (unit-length logical) and "message" (character)
#'
#' @examples \dontrun{
#' library(caret)
#' # returns "result" FALSE and a "message"
#' validate_train_ctrl_sum_fun(twoClassSummary, c("yes", "no"), "A")
#' # returns "result" TRUE and no "message"
#' validate_train_ctrl_sum_fun(twoClassSummary, c("yes", "no"), "ROC")
#' }
validate_train_ctrl_sum_fun <- function(fun, levs, metric) {
  as.character.call <- function(x, ...) {tmp <- as.character(x); if (length(tmp) == 1) return(tmp); paste(tmp[c(2,1,3)], collapse = "")}
  args <- lapply(as.list(match.call()), as.character.call)

  out <- list(result = TRUE, message = character())
  res <- fun(data = data.frame(yes = .1, no = .9, obs = factor(levs[1], levs), pred = factor(levs[2], levs)), lev = levs)

  if (!metric %in% names(res)){
    out$message <- paste(
      sprintf("`%s` needs to comfort to evaluation metrics reported by `%s`.", args$metric, args$fun)
      , sprintf("But a test call of `%s` returned only the following metrics:", args$fun)
      , paste(dQuote(names(res)), collapse = ", ")
    )
    out$result <- FALSE
  }
  return(out)
}

#' Raise \code{\link{classify_tweets}} debugging warning
#'
#' Internal helper to \code{\link{classify_tweets}}
#' @param .x input data frame
#' @param .predvars character vector of names of required preedictor variables.
#'
#' @return none (raises warning invisibly)
classify_tweets_debug_warning <- function(.x, .predvars) {

  if (length(missing_ <- which(!.predvars %in% names(.x))) > 0)
    warning("`x` is missing the following predictors: ", paste("\n  -", dQuote(.predvars[missing_])), call. = FALSE)

  return(invisible())
}

#' \code{classify_tweets} core module
#'
#' Does part of the heavy lifting for \code{classify_tweets}'s methods
#'
#' @param model an 'caretEnsemble' object
#' @param x a 'data.frame' object (see \code{?\link{classify_tweets}})
#' @param na.rm logical. list-wise remove rows with missings?
#' @param threshold double in (0,1) (see \code{?\link{classify_tweets}})
#' @param .predict.type "prob" or "raw" (see \code{?\link{classify_tweets}})
#' @param .add logical (see \code{?\link{classify_tweets}})
#' @param .debug logical (see \code{?\link{classify_tweets}})
#' @param .verbose logical (see \code{?\link{classify_tweets}})
#' @param ... arguments passed to predict method (see \code{?\link{classify_tweets}})
#'
#' @return a data.frame
#'
#' @import caret
#' @import caretEnsemble
classify_tweets_predict <- function(model, x, na.rm, threshold, .predict.type, .add, .debug, .verbose, ...) {

  # compute model weights
  wgts <- coef(model$ens_model$finalModel)[-1]
  wgts <- wgts/sum(wgts)

  if (.verbose) message("Constituent models (weights): ", paste(sprintf("%s (%0.3f)", dQuote(names(wgts)), wgts), collapse = ", "))

  predvars <- get_model_predvars(model$models)

  if (any(!predvars %in% names(x))) {
    # if in debug mode ...
    if (.debug){
      classify_tweets_debug_warning(x, predvars)
    } else {
      # else, give suggestive warning message ...
      warning(
        paste(
          "It seems that the data frame you passed to argument `x` is missing some predictor variables."
          , "To check which predictor variables need to be contained in `x`, call `classify_tweets` again but with `.debug = TRUE`."
        )
        , call. = FALSE
      )
    }

    stop("Cannot predict outcome class for samples contained in `x`. Required predictor variables missing.", call. = FALSE)
  }

  if (na.rm) {
    missings <- detect_missings(x[, predvars])
    if (!is.null(missings$removed))
      x <- x[-missings$removed, ]
  }

  # try prediction
  try_pred <- tryCatch(predict(model, x[1, ], type = .predict.type, ...), error = function(err) err)

  # if fails
  if (inherits(try_pred, "error"))
    stop("Cannot predict outcome class for samples contained in `x`. Error message reads ", sQuote(try_pred$message), call. = FALSE)

  # predict new samples'
  if (.verbose) message("Classifying ", nrow(x), " samples")
  out <- predict(model, x, type = .predict.type, ...)

  pos <- model$ens_model$levels[1]
  if (is.recursive(out)) {
    names(out) <- paste0("prob_", pos, c("", "_025ci", "_975ci"))
  } else if (is.numeric(out)) {
    out <- setNames(data.frame(out), paste0("prob_", pos))
  } else {
    out <- data.frame(class = out)
  }

  if (is.null(out$class))
    out$class <- factor(out[[1]] >= threshold, c(T, F), model$ens_model$levels)

  if (.add) {
    out <- cbind(out, x)
  }

  if (na.rm) {
    attr(out, "removed.rows") <- missings$removed
    attr(out, "removed.rows.nas") <- missings$rows_na_map
  }

  return(out)
}


# generic function ----

#' Classify tweets into political and non-political
#'
#' @description Function takes a data frame of tweet features as input,
#'     and obtains a prediction for each sample using an ensemble classifier.
#'
#' @details \code{classify_tweets} can handle two types of model input:
#'     \enumerate{
#'          \item{Lists of pre-trained base learner models:
#'              This is the default behavior if the input to argument \code{model}
#'              is a 'caretList' object (i.e., a list of pre-trained base learners).
#'              In this case, the base learners are first "blended" into a greedy ensemble
#'              classifier, and the resulting ensemble model is then used to classify
#'              samples in \code{x}.
#'          }
#'          \item{Pre-trained ensemble classifiers: If the input to argument \code{model}
#'              is a 'caretEnsemble' object, this ensemble model is directly used to classify
#'              samples in \code{x}.
#'          }
#'     }
#'
#' @param x a data frame object of tweet features/predictor variables
#'
#' @param model Either
#'     \itemize{
#'        \item{a 'caretEnsemble' object (created with \code{\link[caretEnsemble]{caretEnsemble}}), or}
#'        \item{a 'caretList' object, (i.e., a list of \code{\link[caret]{train}}
#'      objects obtained with \code{\link[caretEnsemble]{caretList}}).}
#'     }
#'     Defaults to the 'caretEnsemble' object \code{\link{ensemble.model}}.
#'     See section "Using \code{classify_tweets} when \code{model} is a 'caretList' object" for details.
#'
#' @param na.rm logical. List-wise remove rows with missings?
#'     If \code{TRUE} (default), rows with any missing values (\code{NA}, \code{NaN}, or \code{Inf})
#'      on predictor variables (see \code{?\link{get_model_predvars}}) are dropped.
#'     Information on removed rows is recorded in attributes
#'      "removed.rows" (indexes) and "removed.rows.nas"
#'      (list of lists recording predictor variable/feature names with missing values).
#'
#' @param threshold a unit-length double vector in (0, 1),
#'     specifying the (predicted) probability threshold used to classify samples
#'     as positive (i.e., "political") instances.
#'
#' @param ... Additional arguments passed to specific method and \code{\link[caretEnsemble]{predict}}.
#'
#' @param .predict.type a unit-length character string,
#'     either "prob" (obtain predicted probabilities, the default) or
#'     or "raw" (obtain predicted classes)
#'
#' @param .add logical: Column-bind (add) predictions to \code{x} before returning? Default is \code{FALSE}.
#'
#' @param .verbose logical. Print messages to console informing about what the function is doing.
#'
#' @param .debug logical. Defaults to \code{FALSE}. If \code{TRUE} a message will be printed to the console if
#'     predicting new samples classes fails that informs about the source of the error.
#'
#' @section Using \code{classify_tweets} when \code{model} is a 'caretList' object:
#'
#'    By default, four constituent models are used to create the ensemble classifier
#'    (see \code{?\link{constituent.models}}):
#'    \itemize{
#'      \item{\code{glmnet}: a generalized linear model (GLM) with Elastic-Net regularization (\code{\link[glmnet]{glmnet}})}
#'      \item{\code{svmRadial}: a Support Vector Machine (SVM) with a radial kernel (\code{\link[kernlab]{ksvm}} with \code{kernel = "rbfdot"})}
#'      \item{\code{ranger}: a Random Forest (\code{\link[range]{ranger}})}
#'      \item{\code{xgbTree}: an eXtreme Gradient Boosting (XGBoost) machine (\code{\link[xgboost]{xgboost}} with \code{learner = "tree"})}
#'    }
#'
#'     The ensemble classifier is obtain by "blending" constituent models using a generalized linear model (GLM)
#'     This is done by a call to the \code{\link[caretEnsemble]{caretEnsemble}} function
#'      (see \code{vignette("caretEnsemble-intro", package = "caretEnsemble")}).
#'
#'     The \code{blend.by} determines which evaluation metric is used to "blend" constituent models.
#'     It is passed to the \code{metric} argument when calling \code{caretEnsemble}, which, in turn,
#'      forwards \code{metric} to \code{\link[caret]{train}} when training the GLM with \code{method = "glm"}.
#'
#' @section Classifying samples in \code{x}:
#'
#'     To classify samples in \code{x}, the ensemble model is passed to
#'      the \code{object} argument when calling \code{caretEnsemble}'s \code{predict} method.
#'
#'     By default  (\code{.predict.type = "prob"}), predicted probabilities for
#'      the "yes" (political) class are obtained, and a classification into
#'      "yes" and "no" is induced based on the \code{threshold} (default is .5).
#'     That is, all samples with a predicted probability ≥ \code{threshold} are classified as "yes" instances.
#'
#'     Alternatively, you can directly obtain an assignment into classes by
#'      setting \code{.predict.type = "raw"}.
#'     CAUTION: In the latter case, \code{threshold} will have no effect,
#'      and the default threshold of .5 is always used.
#'
#' @return A data frame of predictions.
#'     Check attribute "removed.rows" for indexes of removed rows
#'     and "removed.rows.nas" for corresponding missing value information
#'     if \code{na.rm = TRUE}.
#'
#' @export
classify_tweets <- function(
  x
  , model = ensemble.model
  , na.rm = TRUE
  , threshold = .5
  , ...
  , .predict.type = "prob"
  , .add = FALSE
  , .verbose = TRUE
  , .debug = FALSE
) {

  # check x
  stopifnot(
    TRUE
    # check x input
    , "`x` needs to be a/inherit from data.frame object" = inherits(x, "data.frame")
    , "`x` needs to have at least one row" = nrow(x) > 0
    # check na.rm input
    , "`na.rm` must be either TRUE or FALSE." = !is.null(na.rm)
    , "`na.rm` must be either TRUE or FALSE." = length(na.rm) == 1L
    , "`na.rm` must be either TRUE or FALSE." = is.logical(na.rm) & !is.na(na.rm)
    # check classification threshold
    , "`threshold` needs to be in (0, 1)." = is.double(threshold)
    , "`threshold` needs to be in (0, 1)." = length(threshold) == 1
    , "`threshold` needs to be in (0, 1)." = threshold > 0
    , "`threshold` needs to be in (0, 1)." = threshold < 1
    # check prediction type
    , '`.predict.type` needs to be "prob" or "raw".' = is.character(.predict.type)
    , '`.predict.type` needs to be "prob" or "raw".' = length(.predict.type) == 1
    , '`.predict.type` needs to be "prob" or "raw".' = .predict.type %in% c("prob", "raw")
  )

  # determine method
  UseMethod("classify_tweets", model)

}

# methods ----


#' @describeIn classify_tweets Default method (when \code{model} is neither a 'caretList' or 'caretEnsemble' object)
#' @export
classify_tweets.default <- function(
  x
  , model
  , na.rm = TRUE
  , threshold = .5
  , ...
  , .predict.type = "prob"
  , .add = FALSE
  , .verbose = TRUE
  , .debug = FALSE
) {
  stop(
    paste(
      "Object passed to `model` has the wrong class."
      , sprintf("(`classify_tweets` is not implemented for objects of class %s)", dQuote(class(model)))
      , "\nHint: `model` must be"
      , "\n  - a “caretList” object (i.e., a list of pre-trained caret “train” objects, see `?caretEnsemble::caretList`) or "
      , "\n  - a “caretEnsemble”  object (see `?caretEnsemble::caretEnsemble`)"
    )
    , call. = FALSE
  )
}


#' @describeIn classify_tweets Method when \code{model} is a 'caretEnsemble' object
#'     (i.e., a pre-trained ensemble model)
#' @export
classify_tweets.caretEnsemble <- function(
  x
  , model = ensemble.model
  , na.rm = TRUE
  , threshold = .5
  , ...
  , .predict.type = "prob"
  , .add = FALSE
  , .verbose = TRUE
  , .debug = FALSE
) {

  # check dependencies
  check_dependencies(
    pkgs = data.frame(
      name = unique(unlist(lapply(lapply(model$models, `[[`, "modelInfo"), `[[`, "library")))
      , source = "CRAN"
      , github_path = NA_character_
    )
  )

  if (.verbose) message("Using pre-trained ensemble classifier")

  classify_tweets_predict(model, x, na.rm, threshold, .predict.type, .add, .debug, .verbose, ...)

}



#' @describeIn classify_tweets Method when \code{model} is a 'caretList' object (i.e., a list of pre-trained base learner models)
#'
#' @param blend.by a unit-length character string determining the evaluation metric
#'     based on which constituent models (base learners) should be blended into the
#'     ensemble classifier (see section "Ensemble classifier")
#'
#' @param .train.ctrl a list object created by calling \code{\link[caret]{trainControl}}.
#'     Make sure that the summary function you specify when setting up training controls
#'      (argument \code{summaryFunction} of \code{\link[caret]{trainControl}})
#'      returns the evaluation metric used to blend models, i.e., \code{blend.by}.
#'
#' @param .cache.model logical. Cache ensemble classifiers
#'      obtained from \code{model} using \code{blend.by}.
#'      Default is \code{FALSE}.
#'
#' @param .cache.path unit-length character, specifying where to write cached ensemble classifiers
#'      if \code{.cache.model = TRUE}.
#'      Default is "cache" in package directory in file system (see \code{getOption("politicaltweets.cache.path")}).
#'
#' @import caret
#' @import caretEnsemble
#' @importFrom memoise memoize cache_filesystem
#'
#' @export
classify_tweets.caretList <- function(
  x
  , model = constituent.models
  , na.rm = TRUE
  , threshold = .5
  , blend.by = "PR-AUC"
  , .train.ctrl = trainControl(
    method = "repeatedcv", number = 10, repeats = 10
    , search = "grid"
    , returnData = FALSE, returnResamp = "none", savePredictions = "none"
    , classProbs = TRUE
    , summaryFunction = superSumFun
    , allowParallel = TRUE
  )
  , ...
  , .predict.type = "prob"
  , .add = FALSE
  , .verbose = TRUE
  , .debug = FALSE
  , .cache.model = FALSE
  , .cache.path = getOption("politicaltweets.cache.path")
){

  stopifnot(
    TRUE
    # check blending criterion input
    , "`blend.by` needs to be a single character string." = is.character(blend.by)
    , "`blend.by` needs to be a single character string." = length(blend.by) == 1L
    # check trainControl input
    , "`.train.ctrl` needs to be an object created with caret::trainControl()." = !is.null(.train.ctrl)
    , "`.train.ctrl` needs to be an object created with caret::trainControl()." = inherits(.train.ctrl, "list")
    , "`.train.ctrl` needs to be an object created with caret::trainControl()." = all(names(formals(caret::trainControl)) %in% names(.train.ctrl))
    # check trainControl summary function
    , "`.train.ctrl$summaryFunction` needs to be a function with arguments 'data', 'lev', and 'model' (see ?caret::defaultSummary)" = !is.null(.train.ctrl$summaryFunction)
    , "`.train.ctrl$summaryFunction` needs to be a function with arguments 'data', 'lev', and 'model' (see ?caret::defaultSummary)" = is.function(.train.ctrl$summaryFunction)
    , "`.train.ctrl$summaryFunction` needs to be a function with arguments 'data', 'lev', and 'model' (see ?caret::defaultSummary)" = identical(names(formals(.train.ctrl$summaryFunction)), c("data", "lev", "model"))
  )

  # check predictor variables in `x`
  predvars <- get_model_predvars(model)

  if (any(!predvars %in% names(x))) {
    # if in debug mode ...
    if (.debug){
      classify_tweets_debug_warning(x, predvars)
    } else {
      # else, give suggestive warning message ...
      warning(
        paste(
          "It seems that the data frame you passed to argument `x` is missing some predictor variables."
          , "To check which predictor variables need to be contained in `x`, call `classify_tweets` again but with `.debug = TRUE`."
        )
        , call. = FALSE
      )
    }

    stop("Cannot predict outcome class for samples contained in `x`. Required predictor variables missing.", call. = FALSE)
  }

  # test trainControl summary function
  sumfun_test <- validate_train_ctrl_sum_fun(.train.ctrl$summaryFunction, model[[1]]$levels, blend.by)
  if (!sumfun_test$result) stop(sumfun_test$message, call. = FALSE)

  # check dependencies
  check_dependencies(
    pkgs = data.frame(
      name = unique(unlist(lapply(lapply(model, `[[`, "modelInfo"), `[[`, "library")))
      , source = "CRAN"
      , github_path = NA_character_
    )
  )

  # setup file caching
  if (.cache.model) {
    fc <- cache_filesystem(.cache.path)
    mce <- memoize(caretEnsemble, cache = fc)
  } else {
    mce <- caretEnsemble
  }

  # try creating ensemble model
  if (.verbose) message("Creating ensemble classifier by blending models using the ", dQuote(blend.by), " metric (this may take a couple of seconds)")
  ensemble <- tryCatch(
    suppressMessages(mce(model, metric = blend.by, trControl = .train.ctrl))
    , error = function(err) err
  )

  if (inherits(ensemble, "error"))
    stop("Could not create ensemble classifier: Error message reads ", sQuote(ensemble$message), call. = FALSE)

  classify_tweets_predict(ensemble, x, na.rm, threshold, .predict.type, .add, .debug, .verbose, ...)

}



