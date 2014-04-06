\name{list_datasets}
\alias{list_datasets}
\title{List datasets}
\usage{
list_datasets(host, path = "martservice", port = 9001)
}
\arguments{
  \item{host}{The url without the http://}

  \item{path}{the path on the server; only if other than
  default 'martservice'}

  \item{port}{the port on the server}
}
\value{
a list object containing the parameters and the result as a
dataframe in $mart
}
\description{
List datasets
}

