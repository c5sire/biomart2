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
library(XML)

curlPerformWithPB <- function(url){
  h <- basicTextGatherer()
  curlPerform(url=url, customrequest='HEAD', 
              #httpheader=c(Accept="text/json", 'Content-Type' = "text/json; charset=utf-8"),
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
#' Valid mart names are those listed in the 'config' column of the resulting dataframe 'marts' in the response list.
#'
#' @param host The url without the http://
#' @param path the path on the server; only if other than default 'martservice'
#' @param port the port on the server
#' @return a list object containing the parameters and the result as a dataframe in $mart
#' @example inst/examples/example-bm.R
#' @export
list_marts <- function(host, path="martservice", port=8080){
  url = paste("http://",host,":",port,"/",path,"/marts", ".",format="xml", sep="")
  res = "ERROR"
  data = res
  if(url.exists(url)){
    data = curlPerformWithPB(url)
    data = xmlToList(data)
    data = as.data.frame(matrix(data,ncol=9,byrow=TRUE), stringsAsFactors=FALSE)
    names(data) = c("mart", "config","group","isHidden","meta","operation","description","displayName","name")
  }
  list(host = host, path = path, port=port, marts = data)
}

#'List datasets
#' 
#' Given a mart object and a martname return a table of datasets.
#'
#' @param mart mart object/list
#' @param martname a speficic mart
#' @return a table of datasets
#'
#' @export
list_datasets <- function(mart, martname){
  res = character()
  url = paste("http://", mart$host,":", mart$port, "/", mart$path, "/datasets?mart=",martname,
              sep="")
  if(url.exists(url)){
    data = curlPerformWithPB(url)
    data = xmlToList(data)
    
    n = length(data[1])
    if(n>0){
      
      res = matrix("",n, 4)
      res = as.data.frame(res)
      names(res) = c("isHidden", "description", "displayName", "name")
      for(j in 1:4) res[,j] = as.character(res[,j])
      for(j in 1:4){
        for(i in 1:n){
          res[i,j] = data[[j]][[i]]
        }
        
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
    #xml="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
    #data = xmlToList(paste(xml,data,sep="\n"))
    doc = xmlParse(data)
    data = xmlToList(doc)
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
#'This returns a rather crude list of filters. A simple list of filter names can be obtained using 'get_filter_names'.
#'If a predefined list of permitted values exists it can be retrieved using 'get_filter_values'.
#'@param mart mart object
#'@param dataset a dataset
#'@return list of filters
#'@seealso get_filter_names
#'@seealso get_filter_values
#'@export
list_filters <- function(mart, dataset){
  url = paste("http://", mart$host,":", mart$port, "/", mart$path, "/filters?datasets=",dataset,
              "&config=", dataset ,"_config",sep="")
  if(url.exists(url)){
    data = curlPerformWithPB(url)
    data = xmlParse(data)
    data = xmlToList(data)
  }
  data
}

#' Extracts a list of filter names from the filter list as a result of 'list_filters'.
#' 
#' @param filters result of 'list_filters'
#' @return a vector of filter names
#' @seealso list_filters
#' @export
get_filter_names <- function(filters){
  n = length(filters)
  res = character(n)
  for(i in 1:n){
    res[i] = filters[[i]]$.attrs[['name']]
  }
  res
}

#' Extracts a list of filter values from the filter list as a result of 'list_filters'.
#' 
#' @param filters result of 'list_filters'
#' @param afilter a specific filter name
#' @return a vector of filter names
#' @seealso list_filters
#' @export
get_filter_values <- function(filters, afilter){
  gfn = get_filter_names(filters)
  n = which(gfn==afilter)
  v = filters[[n]]$values
  if(!is.null(v)){
    m = length(v)
    res = character(m)
    for(i in 1:m){
      res[i]= v[i]$value[['name']]
    }
    v = res
  }
  v
}

#' Gets the details of a filter configuration.
#' 
#' @param filters result of 'list_filters'
#' @param afilter a specific filter name
#' @return a character vector of named filter details
#' @seealso list_filters
#' @export
describe_filter <- function(filters,afilter){
  gfn = get_filter_names(filters)
  n = which(gfn==afilter)
  filters[[n]]$.attr
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
#'For a 'multiselection' filter multiple values can be given as a comma seperated list (see examples.)
#'
#'@param mart mart object
#'@param dataset a dataset
#'@param filters a vector variable=value pairs.
#'@param attributes vector of attributes
#'@param limit a number to limit the amount of returned records
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
    x = h$value()
    if(str_detect(x,"Not Found")) stop("ERROR: Page not found.")
    result <- fromJSON(x)  
    data   <- data.frame(t(sapply(result$data, unlist)), stringsAsFactors=FALSE)
    data
  }
  REST <- make_qry(dataset, filters, attributes, limit)
  x=query(REST)
  x
}
