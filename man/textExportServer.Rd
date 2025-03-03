% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-textExport.R
\name{textExportButton}
\alias{textExportButton}
\alias{textExportServer}
\title{Text Export Button}
\usage{
textExportButton(id, label = "Export")

textExportServer(id, outFun, filename)
}
\arguments{
\item{id}{namespace id}

\item{label}{(character) button label}

\item{outFun}{(reactive) a reactive function returning the output for export, e.g. print, summary, ...}

\item{filename}{(character) name of file without file extension}
}
\description{
Backend for export module
}
