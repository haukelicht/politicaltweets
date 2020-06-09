#' Text quote patterns
#'
#' Quote pattern beginnings and ends
#'
#' @format A list of list of quote patterns beginnings and ends.
#' Each top-level list element is a list with elements
#' \describe{
#'   \item{s}{a unit-length character, specifying quote pattern beginning}
#'   \item{e}{a unit-length character,quote pattern ending}
#'   \item{min_words}{a unit-length non-negative integer, specifying how many words the quote pattern must at least entail to be counted a quote}
#' }
"text.quote.patterns"
