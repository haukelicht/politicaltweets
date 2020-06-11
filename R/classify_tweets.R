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
#' @param .ensemble \code{\link[caretEnsemble]{caretEnsemble}} object
#'
#' @return none (raises warning invisibly)
classify_tweets_debug_warning <- function(.x, .model.list) {
  
  which_is <- function(x, what) which(vapply(x, function(.x, .w = what) any(.x %in% .w), FALSE))
  d_classes <- lapply(.x, class)
  
  d_ <- character()
  for (p in names(fct_cols <- which_is(d_classes, "factor"))) d_ <- c(d_, p, paste0(p, levels(.x[[p]])))
  for (p in names(lgl_cols <- which_is(d_classes, "logical"))) d_ <- c(d_, p, paste0(p, c("TRUE", "FALSE")))
  
  
  d_ <- c(d_, names(.x)[-c(fct_cols, lgl_cols)])
  
  p_names <- Reduce(union, lapply(lapply(.model.list, `[`, "coefnames"), `[`, 1))[[1]]
  
  if (length(missing <- which(!p_names %in% d_)) > 0)
    warning("`x` is missing the following predictors: ", paste("\n  -", dQuote(p_names[missing])), call. = FALSE)
  
  return(invisible())
}


#' Classify tweets into political and non-political
#' 
#' Function takes a data frame of tweet features as input, and obtains a prediction for each sample 
#'     using a pre-trained ensemble classifier
#' 
#' @param x a data frame object of tweet features/predictor variables
#' 
#' @param blend.by a unit-length character string determining the evaluation metric 
#'     based on which constituent models (base learners) should be blended into the 
#'     ensemble classifier (see section "Ensemble classifier")
#'     
#' @param classification.threshold a unit-length double vector in (0, 1), 
#'     specifying the (predicted) probability threshold used to classify samples 
#'     as positive (i.e., "political") instances.
#' 
#' @param .predict.type a unit-length character string, 
#'     either "prob" (obtain predicted probabilities) or  
#'     or "raw" (obtain predicted classes)
#'     
#' @param .model.list a list of \code{\link[caret]{train}} objects obtained with \code{\link[caretEnsemble]{caretList}}.
#'     Defaults to \code{\link{constituent.models}}
#'     See section "Constituent models" for details.
#'      
#' @param .train.ctrl a list object created by calling \code{\link[caret]{trainControl}}.
#'     Make sure that the summary function you specify when setting up training controls
#'      (argument \code{summaryFunction} of \code{\link[caret]{trainControl}})
#'      returns the evaluation metric used to blend models, i.e., \code{blend.by}.
#'     
#' @param .verbose logical. Print messages to console infroming about what the function is doing.
#' 
#' @param .debug logical. Defaults to \code{FALSE}. If \code{TRUE} a message will be printed to the console if 
#'     predicting new samples classes fails that informs about the source of the error.
#'     
#' @param .add logical: Column-bind (add) predictions to \code{x} before returning? Default is \code{FALSE}.
#' 
#' @param .cache.model logical. Cache ensemble classifiers 
#'      obtained from \code{.model.list} using \code{blend.by}.
#'      Default is \code{FALSE}.
#'      
#' @param .cache.path unit-length character, specifying where to write cached ensemble classifiers
#'      if \code{.cache.model = TRUE}.
#'      Default is "cache" in package directory in file system (see \code{getOption("politicaltweets.cache.path")}). 
#'      
#' @param ... Additional arguments passed to \code{\link[caretEnsemble]{predict}}.
#'     
#' @section Constituent models (pre-trained base learners):
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
#' 
#' @section The ensemble classifier:
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
#'      "yes" and "no" is induced based on the \code{classification.threshold} (default is .5).
#'     That is, all samples with a predicted probability ≥ \code{classification.threshold} are classified as "yes" instances.
#'      
#'     Alternatively, you can directly obtain an assignment into classes by 
#'      setting \code{.predict.type = "raw"}. 
#'     CAUTION: In the latter case, \code{classification.threshold} will have no effect, 
#'      and the default threshold of .5 is always used.
#'     
#' 
#' @import caret
#' @import caretEnsemble
#' @importFrom memoise memoize cache_filesystem
#'  
#' @export 
classify_tweets <- function(
  x
  , blend.by = "PR-AUC"
  , classification.threshold = .5
  , .predict.type = "prob"
  , .add = FALSE
  , .model.list = constituent.models
  , .train.ctrl = trainControl(
    method = "repeatedcv", number = 10, repeats = 10
    , search = "grid"
    , returnData = FALSE, returnResamp = "none", savePredictions = "none"
    , classProbs = TRUE
    , summaryFunction = superSumFun
    , allowParallel = TRUE
  )
  , ...
  , .verbose = TRUE
  , .debug = FALSE
  , .cache.model = FALSE
  , .cache.path = getOption("politicaltweets.cache.path")
){

  stopifnot(
    TRUE
    # check x input
    , "`x` needs to be a/inherit from data.frame object" = inherits(x, "data.frame")
    , "`x` needs to have at least one row" = nrow(x) > 0
    # check classification threshold
    , "`classification.threshold` needs to be in (0, 1)." = is.double(classification.threshold)
    , "`classification.threshold` needs to be in (0, 1)." = length(classification.threshold) == 1
    , "`classification.threshold` needs to be in (0, 1)." = classification.threshold > 0
    , "`classification.threshold` needs to be in (0, 1)." = classification.threshold < 1
    # check prediction type
    , '`.predict.type` needs to be "prob" or "raw".' = is.character(.predict.type)
    , '`.predict.type` needs to be "prob" or "raw".' = length(.predict.type) == 1
    , '`.predict.type` needs to be "prob" or "raw".' = .predict.type %in% c("prob", "raw")
    # check blending criterion input
    , "`blend.by` needs to be a single character string." = is.character(blend.by) 
    , "`blend.by` needs to be a single character string." = length(blend.by) == 1L
    # check model list
    , '`.model.list` needs to be a "caretList" object, a list of `train` objects' = inherits(.model.list, "caretList") && is.recursive(.model.list)
    , '`.model.list` needs to be a "caretList" object, a list of `train` objects' = length(.model.list) > 0
    , '`.model.list` needs to be a "caretList" object, a list of `train` objects' = all(vapply(.model.list, inherits, NA, what = "train"))
    # check trainControl input
    , "`.train.ctrl` needs to be an object created with caret::trainControl()." = !is.null(.train.ctrl)
    , "`.train.ctrl` needs to be an object created with caret::trainControl()." = inherits(.train.ctrl, "list")
    , "`.train.ctrl` needs to be an object created with caret::trainControl()." = all(names(formals(caret::trainControl)) %in% names(.train.ctrl))
    # check trainControl summary function
    , "`.train.ctrl$summaryFunction` needs to be a function with arguments 'data', 'lev', and 'model' (see ?caret::defaultSummary)" = !is.null(.train.ctrl$summaryFunction)
    , "`.train.ctrl$summaryFunction` needs to be a function with arguments 'data', 'lev', and 'model' (see ?caret::defaultSummary)" = is.function(.train.ctrl$summaryFunction)
    , "`.train.ctrl$summaryFunction` needs to be a function with arguments 'data', 'lev', and 'model' (see ?caret::defaultSummary)" = identical(names(formals(.train.ctrl$summaryFunction)), c("data", "lev", "model"))
  )
  
  # test trainControl summary function
  sumfun_test <- validate_train_ctrl_sum_fun(.train.ctrl$summaryFunction, .model.list[[1]]$levels, blend.by) 
  if (!sumfun_test$result) stop(sumfun_test$message, call. = FALSE)
  
  # check dependencies
  check_dependencies(
    pkgs = data.frame(
      name = unique(unlist(lapply(lapply(.model.list, `[[`, "modelInfo"), `[[`, "library")))
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
    suppressMessages(mce(.model.list, metric = blend.by, trControl = .train.ctrl))
    , error = function(err) err
  )
  
  if (inherits(ensemble, "error"))
    stop("Could not create ensemble classifier: Error message reads ", sQuote(ensemble$message), call. = FALSE)
  
  # compute model weights
  wgts <- coef(ensemble$ens_model$finalModel)[-1]
  wgts <- wgts/sum(wgts)
  
  if (.verbose) message("Constituent models (weights): ", paste(sprintf("%s (%0.3f)", dQuote(names(wgts)), wgts), collapse = ", "))
  
  # try prediction
  try_pred <- tryCatch(predict(ensemble, x[1, ], type = .predict.type, ...), error = function(err) err)
  
  # if fails
  if (inherits(try_pred, "error")) {
    # if in debug mode ...
    if (.debug){
      classify_tweets_debug_warning(x, .model.list)
    } else if (grepl("^object .+ not found$", try_pred$message)) {
      # else, give suggestive warning message ...
      warning(
        paste(
          "It seems that the data frame you passed to argument `x` is missing some predictor variables."
          , "To check which predictor variables need to be contained in `x`, call `classify_tweets` again but with `.debug = TRUE`." 
        )
        , call. = FALSE  
      )
    }
    
    # stop
    stop("Cannot predict outcome class for samples contained in `x`. Error message reads ", sQuote(try_pred$message), call. = FALSE)
  }
  
  # predict new samples' 
  if (.verbose) message("Classifying ", nrow(x), " samples")
  out <- predict(ensemble, x, type = .predict.type, ...)
  
  pos <- ensemble$ens_model$levels[1]
  if (is.recursive(out)) {
    names(out) <- paste0("prob_", pos, c("", "_025ci", "_975ci"))
  } else if (is.numeric(out)) {
    out <- setNames(data.frame(out), paste0("prob_", pos))
  } else {
    out <- data.frame(class = out)
  }
  
  if (is.null(out$class)) 
    out$class <- factor(out[[1]] >= classification.threshold, c(T, F), ensemble$ens_model$levels)
  
  if (.add) {
    out <- cbind(out, x)
  }
  
  return(out)
}