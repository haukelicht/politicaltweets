required.tweets.df.cols <- tibble::tribble(
  ~colname, ~accepted_classes,
  "status_id",c("character", "integer"),
  "created_at",c("POSIXt", "integer", "character"),
  "text","character",
  "display_text_width",c("double","integer"),
  "lang","character",
  "reply_to_status_id",c("character", "integer"),
  "is_retweet","logical",
  "is_quote","logical",
  "hashtags","list",
  "symbols","list",
  "urls_expanded_url","list",
  "media_expanded_url","list",
  "ext_media_expanded_url","list",
  "media_type","list",
  "favorite_count",c("double","integer"),
  "retweet_count",c("double","integer"),
  "quote_count",c("double","integer"),
  "reply_count",c("double","integer")
)
usethis::use_data(required.tweets.df.cols, overwrite = TRUE)
