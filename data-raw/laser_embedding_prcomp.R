fp <- file.path(".", "..", "..", "data", "fits", "laser_embeddings_pca.RData")
if (file.exists(fp)) {
  laser.embedding.prcomp <- readRDS(fp)
  if (length(idx_ <- which(names(laser.embedding.prcomp) == "x")) == 1)
    laser.embedding.prcomp <- laser.embedding.prcomp[-idx_]
  str(laser.embedding.prcomp,1)
  names(laser.embedding.prcomp$sdev) <- rownames(laser.embedding.prcomp$rotation) <- sprintf("e%04d", 1:1024)
  colnames(laser.embedding.prcomp$rotation) <- tolower(colnames(laser.embedding.prcomp$rotation))
  class(laser.embedding.prcomp) <- c("prcomp", class(laser.embedding.prcomp))
  usethis::use_data(laser.embedding.prcomp, overwrite = TRUE)
}
