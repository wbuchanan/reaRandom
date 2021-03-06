% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reaRandom.R
\docType{class}
\name{REARandom-class}
\alias{REARandom}
\alias{REARandom-class}
\alias{reaRandom-class}
\title{REARandom}
\description{
Defines the base object used for API requests
}
\details{
Generates a new S4 object with methods to access random numbers
provided by Random.org's API
}
\section{Slots}{

\describe{
\item{\code{requestHome}}{The URL where the API requests will be submitted}

\item{\code{apiKey}}{The API Key for Random.org API Access}

\item{\code{jsonrpc}}{The version of the JSONRPC protocol}

\item{\code{method}}{The API method call to issue}

\item{\code{parameters}}{The parameters to be passed to the API Method}

\item{\code{id}}{An ID variable used to match requests with payloads}
}}
\author{
Billy Buchanan
}

