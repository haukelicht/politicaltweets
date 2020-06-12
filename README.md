# politicaltweets: Classify political tweets

The `politicaltweets` R package provides functions to preprocess and classify tweets data according to whether or not they are political based on a pre-trained ensemble classifier.

## Installation

```r
remotes::install_github("haukelicht/politicaltweets")
```

Note that all but one package dependencies are distributed via CRAN.
The one exeption is the [`laserize` package](https://github.com/haukelicht/laserize), which can be installed from GitHub.

## Usage

To classify a tweet, five steps are required

1. Query the tweet data from the Twitter API using [`rtweet`'s](https://rtweet.info/) `lookup_statuses()` (with `parse = TRUE`).
2. pass the parsed tweets data to argument `x` of `create_tweet_features()` to create data frame of tweet features
3. pass the parsed tweets data to argument `x` of `create_tweet_text_representations()` with `.compute.pcs = FALSE` to create obtain tweet text embeding representations[^embedding]
4. combine the tweet features and text representation objects in a data frame.
5. pass the resulting data frame to argument `x` of `classify_tweets()`

[^embedding]: It obtains tweet text [LASER embedding representations](https://github.com/facebookresearch/LASER) using the [`laserize` package](https://github.com/haukelicht/laserize) and projects tweets LASER representations onto a pre-defined independent component space.

A minimal workin example:

```r
library(dplyr)
library(politicaltweets)

# instead of querying data from the Tweet API (step 1)
# below we use a prototypical tweets data frame
glimpse(tweets.df.prototype)

# step 2
tfeats <- create_tweet_features(tweets.df.prototype, .as.data.table = FALSE)

# step 3
ttreps <- create_tweet_text_representations(tweets.df.prototype, .compute.pcs = FALSE)

# step 4
temp <- as_tibble(tfeats) %>%
  left_join(mutate(as_tibble(ttreps$ics), status_id = rownames(ttreps$ics)))
  
# step 5
preds <- classify_tweets(temp, .debug = TRUE) 

# inspect the result
cbind(temp[, c("text", "lang")], preds)
```

## Details

### Required format of `x`

All functions exported by `politicaltweets` expect that data passed to 
their arguments `x` conforms the naming and typing conventions of tweets data frames set by 
the [`rtweet` package](https://rtweet.info/).

A prototypical tweets data frame is distributed with the `politicaltweets` package, 
see `?tweets.df.prototype`.
(Moreover, `politicaltweets::required.tweets.df.cols` maps required columns to the accepted classes.)

### Using `classify_tweets()` with a pre-trained ensemble classifier

`classify_tweets()` can handle two types of model input:

By default, `classify_tweets()` uses a list of four pre-trained models (see `?constituent.modles` for details) 
"blends" them into an ensemble classifier using `blend.by = "PR-AUC"` (maximize the area under the precision-recall curve).

More generally,  `classify_tweets()` can handle two types of model inputs: 

1. *Lists of pre-trained base learner models*:  if the input to argument `model` is a 'caretList' object (i.e., a list of pre-trained base learners). In this case, the base learners are first "blended" into a greedy ensemble classifier, and the resulting ensemble model is then used to classify samples in `x`.
2. *Pre-trained ensemble classifiers*: If the input to argument `model` is a 'caretEnsemble' object, this ensemble model is directly used to classify samples in `x`.

Thus you can train o


