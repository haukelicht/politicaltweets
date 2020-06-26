fp <- file.path(".", "..", "..", "data", "fits", "baseline", "ensemble_model_list_10x10cv.RData")
if (file.exists(fp)) {
  constituent.models <- suppressWarnings(readRDS(fp))

  format(object.size(constituent.models), "Mb")
  training.data <- constituent.models[[1]]$trainingData
  usethis::use_data(training.data, overwrite = TRUE)

  training.features <- setNames(tibble::enframe(lapply(training.data[-1], class)), c("colname", "accepted_types"))
  usethis::use_data(training.features, overwrite = TRUE)

  constituent.models <- lapply(constituent.models, function(m) {m$trainingData <- NULL; m})
  format(object.size(constituent.models), "Mb")
  class(constituent.models) <- "caretList"
  usethis::use_data(constituent.models, overwrite = TRUE)
}
