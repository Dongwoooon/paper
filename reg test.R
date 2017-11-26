install.packages('censReg')

library(plm)
library(dplyr)

setwd('E:/apps/feature')

# practice
x <- read.csv("X_bio_avgna.csv",header=T)
y1 <- read.csv('lit_a_3_bio.csv',header=T)
y2 <- read.csv('lit_d_3_bio.csv',header=T) 

df <- merge(x,y1,by=c('gvkey','year'),all.x=T) %>% merge(y2,by=c('gvkey','year'),all.x=T)
df[is.na(df)] <- 0
df <- subset(df,year>2007)

indep <- names(x)
f=indep[3]
for (i in 4:length(indep)){
  f <- paste(f,indep[i],sep='+')
}

f1 <- paste('lit_a_3 ~',f,sep='')
f2 <- paste('lit_d_3 ~',f,sep='')

df1 <- select(df,-lit_d_3)
df2 <- select(df,-lit_a_3)

model1 <- plm(as.formula(f1) , data = df1, model = "within", effect = "twoways")
summary(model1)

model2 <- plm(as.formula(f2) , data = df2, model = "within", effect = "twoways")
summary(model2)



x <- read.csv("X_EE&IT_avgna.csv",header=T)
y1 <- read.csv('lit_a_4_EE&IT.csv',header=T)
y2 <- read.csv('lit_d_4_EE&IT.csv',header=T) 

df <- merge(x,y1,by=c('gvkey','year'),all.x=T) %>% merge(y2,by=c('gvkey','year'),all.x=T)
df[is.na(df)] <- 0
df <- subset(df,year<2012 & year>2007)

indep <- names(x)
f=indep[3]
for (i in 4:length(indep)){
  f <- paste(f,indep[i],sep='+')
}

f1 <- paste('lit_a_4 ~',f,sep='')
f2 <- paste('lit_d_4 ~',f,sep='')

df1 <- select(df,-lit_d_4)
df2 <- select(df,-lit_a_4)

model1 <- plm(as.formula(f1) , data = df1, model = "within", effect = "twoways")
summary(model1)

model2 <- plm(as.formula(f2) , data = df2, model = "within", effect = "twoways")
summary(model2)



#real
setwd('E:/apps')
bio<-read.csv("app_cit_edge_BIO.csv",header=T)
ee<-read.csv("app_cit_edge_EE&IT.csv",header=T)

get_adj <- function(df,y){
  df <- subset(df,year==y) %>% select(-year)
  cit.net <- graph.data.frame(df)
  amat <- as_adj(cit.net,names=TRUE,sparse=FALSE)
  cname <- as.vector(sort(sapply(colnames(amat),as.integer)))
  cname <- as.character(cname)
  amat <- amat[cname,cname]
  return(amat)
}

df <- subset(bio,year==2009) %>% select(-year)
cit.net <- graph.data.frame(df)
amat <- as_adj(cit.net,names=TRUE,sparse=FALSE)
cname <- as.vector(sort(sapply(colnames(amat),as.integer)))
cname <- as.character(cname)
amat <- amat[cname,cname]

for(i in 2005:2012) {
  mat <- get_adj(bio,i)
  fname = paste('appcit_matrix_bio_',as.character(i),'.csv',sep='')
  write.csv(mat,fname)
}

for(i in 2005:2012) {
  mat <- get_adj(ee,i)
  fname = paste('appcit_matrix_EE&IT_',as.character(i),'.csv',sep='')
  write.csv(mat,fname)
}
