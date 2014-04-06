#' biomart2
#'
#' To access the new API of biomart as of version 0.8.
#'
#' @name biomart2-package
#' @example inst/examples/example-bm.R
#' @docType package
NULL


library(RCurl)
library(RJSONIO)
library(stringr)
library(tools)

curlPerformWithPB <- function(url){
  h <- basicTextGatherer()
  curlPerform(url=url, customrequest='HEAD',
              header=1L, nobody=1L, headerfunction=h$update)
  
  if(grepl('Transfer-Encoding: chunked', h$value())) {
    size <- 1
  } else {
    size <- as.numeric(strsplit(strsplit(h$value(),'\r\nContent-Type')[[1]][1],
                                'Content-Length: ')[[1]][2])
  }
  #print(size)
  if(size == 0) size = 100
  bar <- txtProgressBar(0, size)
  h2 <- basicTextGatherer()
  get <- curlPerform(url=url, noprogress=0L,
                     writefunction=h2$update, 
                     progressfunction=function(down,up)
                       setTxtProgressBar(bar, down[2]))
  
  h2$value()
}

#' List marts
#'
#' @param host The url without the http://
#' @param path the path on the server; only if other than default 'martservice'
#' @param port the port on the server
#' @return a list object containing the parameters and the result
#' @export
list_marts <- function(host, path="martservice", port=9001){
  #try(assertCondition(url.exists(host) ))
  url = paste("http://",host,":",port,"/",path,"/marts", ".",format="json", sep="")
  res = "ERROR"
  if(url.exists(url)){
    data = curlPerformWithPB(url)
    data = fromJSON(data)
    if(length(data)>0){
      n = length(data)
      res = matrix("",nrow=n, ncol=8)
      res = as.data.frame(res)
      for(j in 1:8){ res[,j] = as.character(res[,j])}
      names(res) = names(data[[1]])
      
      for(i in 1:n){
        for(j in 1:8){
          res[i,j] = data[[i]][[j]]
        }
        
      }
      
    }
  }
  list(host = host, path = path, port=port, marts = res)
}

#'List datasets
#'
#'Given a mart object and a martname return a table of datasets.
#'
#'@param mart mart object/list
#'@param martname a speficic mart
#'@return a table of datasets
#'
#'@export
list_datasets <- function(mart, martname){
  res = character()
  url = paste("http://", mart$host,":", mart$port, "/", mart$path, "/datasets?mart=",martname,
              sep="")
  if(url.exists(url)){
    data = curlPerformWithPB(url)
    data = xmlToList(data)
    
    n = length(data)
    if(n>0){
      
      res = matrix("",n, 4)
      res = as.data.frame(res)
      names(res) = names(data[[1]])
      for(j in 1:4) res[,j] = as.character(res[,j])
      for(i in 1:n){
        res[i,j] = data[[i]][[j]]
      }
    }
    
  }
  res
}

#'List attributes
#'
#'List attributes in a dataset
#'@param mart mart object
#'@param dataset a dataset
#'@return list of attributes
#'@export
list_attributes <- function(mart, dataset){
  url = paste("http://", mart$host,":", mart$port, "/", mart$path, "/attributes?datasets=",dataset,
              "&config=", dataset ,"_config",sep="")
  if(url.exists(url)){
    data = curlPerformWithPB(url)
    data = xmlToList(data)
    res = character()
    n = length(data)
    if(n>0){
      
      res = matrix("",n, 8)
      res = as.data.frame(res)
      names(res) = names(data[[1]]$.attrs)
      for(j in 1:8) res[,j] = as.character(res[,j])
      for(i in 1:n){
        x = data[[i]]$.attrs
        res[i,] = as.character(x)
      }
    }
    
  }
  res
}

#'List filters
#'
#'List filters in a dataset
#'@param mart mart object
#'@param dataset a dataset
#'@return list of filters
#'@export
list_filters <- function(mart, dataset){
  url = paste("http://", mart$host,":", mart$port, "/", mart$path, "/filters?datasets=",dataset,
              "&config=", dataset ,"_config",sep="")
  if(url.exists(url)){
    data = curlPerformWithPB(url)
    data = xmlToList(data)
  }
  data
}

make_attr <- function(attr){
  xml = ""
  for(i in 1:length(attr)){
    xml = paste(xml,"<Attribute name=\"",attr[i],"\"/>", sep="")
  }
  xml
}

make_fltr <- function(fltr=NULL){
  if(!is.null(fltr)){
    
  
  xml = ""
  for(i in 1:length(fltr)){
    x=str_split(fltr[i],"=")[[1]]
    xml = paste(xml,"<Filter name=\"",x[1],"\" value=\"",x[2],"\"/>", sep="")
  }
  xml
  } else {
    ""
  }
}

make_qry <- function(dataset, fltr=NULL, attr, limit=NULL){
  if(!is.null(limit)){  
  top = limit
  } else {
  top = -1
  }
  qry = paste('<!DOCTYPE Query><Query client=\"true\" processor=\"JSON\" limit=\"',
	top,'\" header=\"1\">',
			  sep="")
  qry = paste(qry, 
    "<Dataset name=\"",dataset,"\">",
              sep="")
  qry = paste(qry, make_fltr(fltr), make_attr(attr),
     '</Dataset></Query>', sep="")
  qry
}


#'Get a table based on filters and attributes.
#'
#'The main difference to the biomaRt library is here that the filters are given as pairs of variable name and value separated by equal sign.
#'Several filters can be given as a vector of such pairs.
#'
#'@param mart mart object
#'@param dataset a dataset
#'@param filters a vector variable=value pairs.
#'@param attributes vector of attributes
#'@example inst/examples/example-bm.R
#'@export
get_bm <- function(mart, dataset, filters=NULL, attributes, limit=NULL){
  url = paste("http://",mart$host,":",mart$port,"/", mart$path,"/results", sep="")
  query <- function(querystring,curl) {
    h <- basicTextGatherer()
    curlPerform(url=url,
                postfields=paste('query',curlEscape(REST), sep='='),
                writefunction = h$update,
                verbose = FALSE
    )
    result <- fromJSON(h$value())  
    data   <- data.frame(t(sapply(result$data, unlist)), stringsAsFactors=FALSE)
    data
  }
  REST <- make_qry(dataset, filters, attributes, limit)
  x=query(REST)
  x
}
