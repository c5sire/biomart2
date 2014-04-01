\name{get_bm}
\alias{get_bm}
\title{Get a table based on filters and attributes.}
\usage{
get_bm(mart, dataset, filters = NULL, attributes)
}
\arguments{
  \item{mart}{mart object}

  \item{dataset}{a dataset}

  \item{filters}{a vector variable=value pairs.}

  \item{attributes}{vector of attributes}
}
\description{
The main difference to the biomaRt library is here that the
filters are given as pairs of variable name and value
separated by equal sign. Several filters can be given as a
vector of such pairs.
}
\examples{
library(biomart2)

#Uncomment the following to run locally

host = "germplasmdb.cip.cgiar.org"

#list_marts(host)

}

