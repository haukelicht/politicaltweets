fp <- file.path(".", "..", "..", "data", "fits", "baseline", "ensemble_prauc_10x10cv.RData")
if (file.exists(fp)) {
  ensemble.model <- suppressWarnings(readRDS(fp))

  format(object.size(ensemble.model), "Mb")

  ensemble.model$models <- lapply(ensemble.model$models, function(m) {m$trainingData <- NULL; m})
  format(object.size(ensemble.model), "Mb")
  class(ensemble.model$models) <- "caretList"
  usethis::use_data(ensemble.model, overwrite = TRUE)
}
