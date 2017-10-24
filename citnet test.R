install.packages(igraph)
library(igraph)
library(data.table)
library(dplyr)

setwd('J:/data')
# practice
cit.data<-read.csv("citnet.csv",header=T)

get_adj <- function(df,y){
  df <- subset(df,year==y) %>% select(-year)
  cit.net <- graph.data.frame(df)
  amat <- as_adj(cit.net,names=TRUE,sparse=FALSE)
  cname <- sort(colnames(amat))
  amat <- amat[cname,cname]
  return(amat)
}

for(i in 2005:2008) {
  mat <- get_adj(cit.data,i)
  fname = paste('appcit_matrix_',as.character(i),'.csv',sep='')
  write.csv(mat,fname)
}


#real
setwd('E:/apps')
cit.data<-read.csv("db_table/citnet.csv",header=T)

get_adj <- function(df,y){
  cit.net <- graph.data.frame(subset(df,year=y))
  amat <- as_adj(cit.net,names=TRUE,sparse=FALSE)
  return(amat)
}