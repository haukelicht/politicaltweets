% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_model_predvars}
\alias{get_model_predvars}
\alias{get_model_predvars.default}
\alias{get_model_predvars.train}
\alias{get_model_predvars.caretList}
\alias{get_model_predvars.caretStack}
\alias{get_model_predvars.caretEnsemble}
\title{Get model(s) predictor variable/feature names}
\usage{
get_model_predvars(object, ...)

\method{get_model_predvars}{default}(object, ...)

\method{get_model_predvars}{train}(object, ...)

\method{get_model_predvars}{caretList}(object, ...)

\method{get_model_predvars}{caretStack}(object, ...)

\method{get_model_predvars}{caretEnsemble}(object, ...)
}
\arguments{
\item{object}{a
\code{caret} "\link[caret]{train}",
\code{caretEnsemble} "\link[caretEnsemble]{caretList}",
\code{caretEnsemble} "\link[caretEnsemble]{caretStack}", or
\code{caretEnsemble} "\link[caretEnsemble]{caretEnsemble}" object.}

\item{...}{Additional arguments ignored}
}
\value{
a character vector of model(s) predictor variable/feature names.
}
\description{
Function returns names of predictor variable/features used to train \code{object}
}
\section{Methods (by class)}{
\itemize{
\item \code{default}: Default method, currently not implemented.

\item \code{train}: Method when \code{object} is a 'train' object

\item \code{caretList}: Method when \code{object} is a 'caretList' object

\item \code{caretStack}: Method when \code{object} is a 'caretStack' object

\item \code{caretEnsemble}: Method when \code{object} is a 'caretEnsemble' object
}}

\section{Model lists, stacks and ensembles}{

    If \code{object} is a "caretList", "caretStack" or "caretEnsmeble",
     \code{get_model_predvars} is called on each constituent models,
     the returned vectors are concatenated,
     and \code{unique} is applied to return the variables used in any model.
}

