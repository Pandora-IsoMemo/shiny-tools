% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-plotTitles.R
\name{validateInitText}
\alias{validateInitText}
\title{Validate Init Text}
\usage{
validateInitText(
  initText,
  type = c("none", "ggplot", "base"),
  availableElements = c("title", "axis")
)
}
\arguments{
\item{initText}{(list) optional, named list with title definitions, or output of \code{plotTitlesServer}}

\item{type}{(character) Type of the plot to add titles to, one of "none", "ggplot", "base".}

\item{availableElements}{(character) set of available labels for specifying the format of text.
May contain elements from \code{c("title", "axis", "legend")}.}
}
\description{
If elements are missing in initText, add those with default values
}