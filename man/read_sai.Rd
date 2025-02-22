% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saitem.R
\name{read_sai}
\alias{read_sai}
\alias{.jsai_results}
\alias{.jsai_jresults}
\title{Read an SA-item}
\usage{
read_sai(jsa)

.jsai_results(jsa, items = NULL)

.jsai_jresults(jsa)
}
\arguments{
\item{jsa}{Java SA-item object.}

\item{items}{vector of characters containing the variables to extract.
See \code{\link[rjd3x13:x13_dictionary]{rjd3x13::x13_dictionary()}} or \code{\link[rjd3tramoseats:tramoseats_dictionary]{rjd3tramoseats::tramoseats_dictionary()}}.
By default, extracts all the possible variables.}
}
\description{
\code{.jsai_results()} extracts specific variables of the model of the SA-item while
\code{read_sai()} extracts all the information of a SA-item (see details).
\code{.jsai_jresults()} extracts the Java object of the results of a SA-item.
}
\details{
A SA-item contains more information than just the results of a model.
All those informations are extracted with the \code{read_sai()} function that
returns a list with 5 objects:
\itemize{
\item \code{ts}: the raw time series.
\item \code{domainSpec}: initial specification. Reference for any relaxing of some
elements of the specification.
\item \code{estimationSpec}: specification used for the current estimation.
\item \code{pointSpec}: specification corresponding to the results of the current
estimation (fully identified model).
\item \code{results}: the result of the model.
}
}
