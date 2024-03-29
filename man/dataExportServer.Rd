% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-dataExport.R
\name{dataExportServer}
\alias{dataExportServer}
\title{Server function for data export}
\usage{
dataExportServer(id, dataFun, filename = "data")
}
\arguments{
\item{id}{namespace id}

\item{dataFun}{(reactive) a reactive function returning a data.frame for export}

\item{filename}{(character) name of file without file extension}
}
\description{
Backend for data export module
}
