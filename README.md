# politicaltweets: Classify political tweets

The `politicaltweets` R package provides functions to preprocess and classify tweets data according to whether or not they are political based on a pre-trained ensemble classifier.

## Installation

```r
remotes::install_github("haukelicht/politicaltweets")
```

Note that all but one package dependencies are distributed via CRAN.
The one exeption is the [`laserize` package](https://github.com/haukelicht/laserize), which can be installed from GitHub.

## Usage

To classify a tweet, two steps are necessary

1. query the tweet data from the Twitter API using [`rtweet`'s](https://rtweet.info/) `lookup_statuses()` (with `parse = TRUE`)
2. pass the parsed tweets data to argument `x` of `classify_tweet()`

## Details

Under the hood, `classify_tweet` does the following.

1. It creates (additional) tweet features using `create_tweet_features()`
2. It obtains tweet text [LASER embedding representations](https://github.com/facebookresearch/LASER) using the [`laserize` package](https://github.com/haukelicht/laserize) and project tweets LASER representations onto a pre-defined independent component space using `create_tweet_text_representations()`
3. Tweet features and and tweet text independent component representations are used to predict their class ('political' vs. 'non-political') using the pre-trained ensemble classifier distributed with the `politicaltweets` package.

**Importantly**, all functions exported by `politicaltweets` expect that data passed to 
their arguments `x` conforms the naming and typing conventions of tweets data frames set by 
the [`rtweet` package](https://rtweet.info/).

A prototypical tweets data frame is distributed with the `politicaltweets` package, 
see `?tweets.df.prototype`.
(Moreover, `politicaltweets::required.tweets.df.cols` maps required columns to the accepted classes.)


