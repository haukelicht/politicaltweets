#' Custom two-class summary function
#' 
#' Function used to compute performance metrics when running \code{\link[caret]{train}}.
#' @details The following metrics are returned as a named numeric vector:
#'     \itemize{
#'         \item{\code{Accuracy}: (TP+TN)/N}
#'         \item{\code{AccuracyNull}: Prevalence of "positive" class}
#'         \item{\code{AccuracyPValue}: p-value of \code{Accuracy} compared to \code{AccuracyNull}}
#'         \item{\code{Balanced Accuracy}: (\code{Sensitivity}+\code{Specificity})/2}
#'         \item{\code{Precision}: TP/(TP+FP) ('How many instance \emph{labeled positive} are correctly classified?')}
#'         \item{\code{Recall}: TP/(TP+FN) ('How many of \emph{truly positive} instances are labeled correctly?')}
#'         \item{\code{Sensitivity}: TP/(TP+FN) = \code{Recall} (true-positive rate)}
#'         \item{\code{Specificity}: TN/(TN+FP) = 1/\code{Recall} (inverse Recall, true-negative rate)}
#'         \item{\code{Kappa}: }
#'         \item{\code{logLoss}: negative log-likelihood of the binomial distribution}
#'         \item{\code{AUC}: Area under the Receiver Operating Characteristic (ROC) curve}
#'         \item{\code{PR-AUC}: Area under the Precision-Recall ROC curve}
#'         \item{\code{F0.5}: F-measure (see Notes) with β = .5 (twice as much weight on Precision as on Recall)}
#'         \item{\code{F1}: F-measure (see Notes) with β = 1 (Precision and Recall weighted equally)}
#'         \item{\code{F2}: F-measure (see Notes) with β = 2 (twice as much weight on Recall as on Precision)}
#'     }
#' Note: The F-measure is computed as (1+β²) x (Precision x Recall)/(β²xPrecision + Recall) 
#' 
#' @param data a data frame with columns \code{obs} and \code{pred} for the observed and predicted outcomes, 
#'     and columns with predicted probabilities for each outcome class. 
#'     See the \code{classProbs} argument to \code{\link[caret]{trainControl}}.
#'
#' @param lev a character vector of factors levels for the response.
#'     First element is passed to \code{\link[caret]{confusionMatrix}}'s \code{positive}.
#' 
#' @param model	a character string for the model name 
#'     (as taken from the \code{method} argument of \code{\link[caret]{train}}.)
#'     
#' @param pred A vector of numeric data (could be a factor)
#' 
#' @param obs A vector of numeric data (could be a factor)
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' dat <- data.frame(
#'   pred = sample(1:2, 10, replace = T)
#'   , obs = sample(1:2, 10, replace = T)
#' ) %>% 
#'   mutate(
#'     `1` = ifelse(pred == 1, sample(seq(.51, .99, length.out = 100), 10), sample(seq(0.01, .49, length.out = 100), 10))
#'     , `2` = 1-`1`
#'   ) %>% 
#'   mutate_at(1:2, as.factor)
#' superSumFun(dat, levels(dat$obs))
#' } 
#' 
#' @import caret
#' @import caret
#' @importFrom ModelMetrics auc
#' @importFrom MLmetrics PRAUC
superSumFun <- function(data, lev = NULL, model = NULL) {
  if (inherits(data, "tbl_df"))
    data <- as.data.frame(data)

  
  lvls <- levels(data$obs)

  if (length(lvls) > 2) 
    stop(paste("Your outcome has", length(lvls), "levels. summary_fun() can only handle two outcome classes."), call. = FALSE)
  
  if (!all(lev %in% colnames(data)))
    stop("`data` must contain columns recording class probabilities. Set `classProbs = TRUE` in `trainControl`.", call. = FALSE)
  
  caret:::requireNamespaceQuietStop("ModelMetrics")
  caret:::requireNamespaceQuietStop("MLmetrics")
  
  if (!all(levels(data[, "pred"]) == lvls)) 
    stop("levels of observed and predicted data do not match", call. = FALSE)
  
  
  lloss <- caret::mnLogLoss(data = data, lev = lev, model = model)
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 1), data[, lvls[1]])
  pr_auc <- try(MLmetrics::PRAUC(y_pred = data[, lev[1]], y_true = ifelse(data$obs == lev[1], 1, 0)), silent = TRUE)
  if (inherits(pr_auc, "try-error")) pr_auc <- NA
  
  CM <- caret::confusionMatrix(data$pred, data$obs, positive = lev[1], mode = "everything")
  
  out <- c(
    CM$overall[c("Accuracy", "AccuracyNull", "AccuracyPValue")]
    , CM$byClass[c("Balanced Accuracy", "Precision", "Recall", "Sensitivity", "Specificity")]
    , CM$overall["Kappa"]
    , lloss 
    , c(
      "AUC" = rocAUC
      , "PR-AUC" = pr_auc
      , "F0.5" = caret:::F_meas.default(data = data$pred, reference = data$obs, relevant = lev[1], beta = 0.5)
      , "F1" = caret:::F_meas.default(data = data$pred, reference = data$obs, relevant = lev[1], beta = 1)
      , "F2" = caret:::F_meas.default(data = data$pred, reference = data$obs, relevant = lev[1], beta = 2)
    )
  )
  
  return(out)
}
