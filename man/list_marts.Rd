\name{list_marts}
\alias{list_marts}
\title{List marts}
\usage{
list_marts(host, path = "martservice", port = 8080)
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
Valid mart names are those listed in the 'config' column of
the resulting dataframe 'marts' in the response list.
}

