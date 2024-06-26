% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-plotTitles.R
\name{defaultInitText}
\alias{defaultInitText}
\title{Default Init Text}
\usage{
defaultInitText(
  type = c("none", "ggplot", "base"),
  availableElements = c("title", "axis")
)
}
\arguments{
\item{type}{(character) Type of the plot to add titles to, one of "none", "ggplot", "base".}

\item{availableElements}{(character) set of available labels for specifying the format of text.
May contain elements from \code{c("title", "axis", "legend")}.}
}
\description{
Initial list with default text elements
}
