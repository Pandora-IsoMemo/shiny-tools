% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-plotExport.R
\name{extractType}
\alias{extractType}
\title{Extract Type}
\usage{
extractType(plotType)
}
\arguments{
\item{plotType}{(character) one of "none", "ggplot", "ggplot_only_titles". Adds the option to
format titles and ranges of a plot within the export UI (currently only for ggplots). For
\code{plotType == "ggplot_only_titles"} only titles can be adjusted. This prevents that custom
formatting of axis ranges might be overwritten by \code{formatRangesOfGGplot()}.}
}
\value{
(character) type required for \code{plotTitlesServer} or \code{plotRangesServer}
}
\description{
Extract Type
}
