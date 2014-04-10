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
