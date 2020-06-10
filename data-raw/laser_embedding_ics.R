fp <- file.path(".", "..", "..", "data", "fits", "laser_embeddings_ica.RData")
if (file.exists(fp)) {
  laser.embedding.icomp <- readRDS(fp)
  if ("S" %in% names(laser.embedding.icomp))
    laser.embedding.icomp$S <- NULL
  usethis::use_data(laser.embedding.icomp, overwrite = TRUE)
}
