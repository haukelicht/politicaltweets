tweets.df.prototype <- tibble::tibble(
  status_id = "12345678",
  created_at = lubridate::ymd_hms("2020-01-01 00:00:01"),
  text = "Hallo Welt!",
  display_text_width = 11L,
  lang = "de",
  reply_to_status_id = NA_character_,
  is_retweet = FALSE,
  is_quote = FALSE,
  hashtags = list(NA_character_),
  symbols = list(NA_character_),
  urls_expanded_url = list(NA_character_),
  media_expanded_url = list(NA_character_),
  ext_media_expanded_url = list(NA_character_),
  media_type = list(NA_character_),
  favorite_count = 1L,
  retweet_count = 1L,
  quote_count = 1L,
  reply_count = 1L
)
usethis::use_data(tweets.df.prototype, overwrite = TRUE)
