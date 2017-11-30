library(igraph)
library(data.table)
library(dplyr)

setwd('E:/apps')
bio<-read.csv("pat_cit_edge_BIO.csv",header=T)
ee<-read.csv("pat_cit_edge_EE&IT.csv",header=T)

get_adj <- function(df,y){
  df <- subset(df,year==y) %>% select(-year)
  cit.net <- graph.data.frame(df)
  amat <- as_adj(cit.net,names=TRUE,sparse=FALSE)
  cname <- as.vector(sort(sapply(colnames(amat),as.integer)))
  cname <- as.character(cname)
  amat <- amat[cname,cname]
  return(amat)
}

for(i in 2005:2012) {
  mat <- get_adj(bio,i)
  fname = paste('patcit_matrix_bio_',as.character(i),'.csv',sep='')
  write.csv(mat,fname)
}

for(i in 2005:2012) {
  mat <- get_adj(ee,i)
  fname = paste('patcit_matrix_EE&IT_',as.character(i),'.csv',sep='')
  write.csv(mat,fname)
}
