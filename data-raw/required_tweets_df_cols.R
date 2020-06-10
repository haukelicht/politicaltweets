required.tweets.df.cols <- tibble::tribble(
  ~colname, ~accepted_classes,
  "status_id",c("character", "integer"),
  "created_at",c("POSIXt", "integer", "character"),
  "text","character",
  "display_text_width","integer",
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
  "favorite_count","integer",
  "retweet_count","integer",
  "quote_count","integer",
  "reply_count","integer"
)
usethis::use_data(required.tweets.df.cols, overwrite = TRUE)
