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

