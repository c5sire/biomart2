\name{get_bm}
\alias{get_bm}
\title{Get a table based on filters and attributes.}
\usage{
get_bm(mart, dataset, filters = NULL, attributes, limit = NULL)
}
\arguments{
  \item{mart}{mart object}

  \item{dataset}{a dataset}

  \item{filters}{a vector variable=value pairs.}

  \item{attributes}{vector of attributes}

  \item{limit}{a number to limit the amount of returned
  records}
}
\description{
The main difference to the biomaRt library is here that the
filters are given as pairs of variable name and value
separated by equal sign. Several filters can be given as a
vector of such pairs. For a 'multiselection' filter
multiple values can be given as a comma seperated list (see
examples.)
}
\examples{
library(biomart2)

#Uncomment the following to run locally

host <- "germplasmdb.cip.cgiar.org"

if(interactive()){
  marts <- list_marts(host)
  mart  <- marts$mart$config[[4]] #dspotatov01_config
  dataset<-list_datasets(marts, mart)
  
  x=get_bm(marts, dataset$name[1], , c("acipnumber","cultvrname","female","male"),20)
  x=get_bm(marts, dataset$name[1],"fbiostat=500 Advanced or Improved cultivar", 
           c("acipnumber","cultvrname","female","male"),20)
  x=get_bm(marts, dataset$name[1],c("fbiostat=500 Advanced or Improved cultivar","fcountry=ARG,AUS"), 
           c("acipnumber","cultvrname","female","male"),20)
  x=get_bm(marts, dataset$name[1],c("fbiostat=500 Advanced or Improved cultivar","fcountry=ARG,PER"), 
           c("acipnumber","cultvrname","female","male","country"),200)
  
}
}

