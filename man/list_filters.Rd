\name{list_filters}
\alias{list_filters}
\title{List filters}
\usage{
list_filters(mart, dataset)
}
\arguments{
  \item{mart}{mart object}

  \item{dataset}{a dataset}
}
\value{
list of filters
}
\description{
This returns a rather crude list of filters. A simple list
of filter names can be obtained using 'get_filter_names'.
If a predefined list of permitted values exists it can be
retrieved using 'get_filter_values'.
}
\seealso{
get_filter_names

get_filter_values
}

