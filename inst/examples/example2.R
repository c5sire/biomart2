library(biomart2)

#Uncomment the following to run locally

host <- "germplasmdb.cip.cgiar.org"

#mart <- list_marts(host)
#mart

datasets <- list_datasets(mart, "dspotatov01_config")
dataset <- "dspotatov01"

#list_attributes(mart, dataset)
#list_filters(mart, dataset)

#examples:

get_bm(mart, dataset, , "acipnumber")
get_bm(mart, dataset, , c("acipnumber"),20)
get_bm(mart, dataset, , c("acipnumber"),20)
get_bm(mart, dataset, , c("acipnumber","cultvrname","female","male"),20)
get_bm(mart, dataset,"fbiostat=500 Advanced or Improved cultivar", c("acipnumber","cultvrname","female","male"),20)
get_bm(mart, dataset,c("fbiostat=500 Advanced or Improved cultivar","fcountry=ARG,AUS"), c("acipnumber","cultvrname","female","male"),20)
get_bm(mart, dataset,c("fbiostat=500 Advanced or Improved cultivar","fcountry=ARG,AUS"), c("acipnumber","cultvrname","female","male","country"),20)
