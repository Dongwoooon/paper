library(MASS)
library(pscl)
library(boot)
library(dplyr)
library(stargazer)

setwd('E:/apps/feature')

x1 <- read.csv("X_bio_avgna_1.csv",header=T) 
x2 <- read.csv("X_EE&IT_avgna_1.csv",header=T) 

x1 <- subset(x1,year>2005)
x2 <- subset(x2,year>2005)

y1 <- read.csv('lit_t1_nc_bio.csv',header=T)
y2 <- read.csv('lit_t1_nc_EE&IT.csv',header=T) 

df1 <- merge(x1,y1,by=c('gvkey','year'),all.x=T) 
df2 <- merge(x2,y2,by=c('gvkey','year'),all.x=T)
df1[is.na(df1)] <- 0
df2[is.na(df2)] <- 0

df1$year = as.factor(df1$year)
df1$gvkey = as.factor(df1$gvkey)
df2$year = as.factor(df2$year)
df2$gvkey = as.factor(df2$gvkey)

df2$ind <- 1
df1$ind <- 0

df <- rbind(df1,df2)
df <- df %>% select(-c(ap_ratio,bwd,mkt_3,rnd,margin,self,self_fwd))
df$pstock <- log(df$pstock)
df <- df %>% select(lit_t1,app_cit,pat_cit,pstock,everything())

summary(df$lit_t1)
summary(df$lit_t1_nc)

sum(df$lit_t1)
sum(df$lit_t1_nc)

# desc stat
d <- df %>% select(-c(gvkey,year,ind))

stargazer(d, type = "text", title="Descriptive statistics", digits=2, out="desc.txt")

cor <- round(cor(d),3)

cor[upper.tri(cor)] <- ""

stargazer(as.matrix(cor), title = "corr matrix", type="text",out="corr.txt")

# distribution
hist(df$app_cit, breaks=100)
hist(d2$app_cit, breaks=100)

hist(d1$bwd, breaks=100)
hist(d2$bwd, breaks=100)

hist(df$lit_t1, breaks=20, freq=T, labels=T)
hist(d2$lit_t1)

# zinb regression
d <- df
d <- df[df$lit_t1<5,]

### zinb (lit_t1)
indep <- names(d)[5:(length(d))]
indep <- indep[! indep %in% 'lit_t1']
f=indep[1]

for (i in 2:length(indep)){
  f <- paste(f,indep[i],sep='+')
} 
f <- paste(f,'+year',sep='')

f2 <- paste('lit_t1 ~ pat_cit+pat_cit*ind+ap_ratio_1*ind+',f,sep='')

pat <- zeroinfl(as.formula(f2), d, dist = "negbin", EM = TRUE)

summary(ap)
summary(pat)

pat1 <- glm.nb(as.formula(f2), data=d)

vuong(ap,ap1)
vuong(pat,pat1)

stargazer(pat,ap,summary(pat)$coefficients$zero, summary(ap)$coefficients$zero, type="text",
          dep.var.labels=c("patent citation","application citation"),out="models_1.txt")

### zinb (lit_nc)
#replcae y1, y2
d <- df
indep <- names(d)[5:(length(d))]
indep <- indep[! indep %in% 'lit_t1_nc']
f=indep[1]

for (i in 2:length(indep)){
  f <- paste(f,indep[i],sep='+')
} 
f <- paste(f,'+year',sep='')

f1 <- paste('lit_t1_nc ~ pat_cit+pat_cit*ind+ap_ratio_1*ind+',f,sep='')
f2 <- paste('lit_t1_nc ~ pat_cit+pat_cit*ind+ap_ratio*ind+',f,sep='')

pat_n <- zeroinfl(as.formula(f1), d, dist = "negbin", EM = TRUE)
pat_n2 <- zeroinfl(as.formula(f2), d, dist = "negbin", EM = TRUE)

summary(pat_n)

m1 <- pat_n
m0 <- zeroinfl(lit_t1_nc ~ 1, d, dist = "negbin")

1-(logLik(m1)-22)/logLik(m0)  ### McFadden’s pseudo R2 (adjusted)

stargazer(pat_n,summary(pat_n)$coefficients$zero, type="text",
          dep.var.labels=c("Non-counter litigation"),out="models_2.txt")

stargazer(pat_n2,summary(pat_n2)$coefficients$zero, type="text",
          dep.var.labels=c("Non-counter litigation"),out="models_3.txt")

### zinb (lit_c)
#replcae y1, y2
d <- df
indep <- names(d)[5:(length(d))]
indep <- indep[! indep %in% 'lit_t1_c']
f=indep[1]

for (i in 2:length(indep)){
  f <- paste(f,indep[i],sep='+')
} 
f <- paste(f,'+year',sep='')

f1 <- paste('lit_t1_c ~ pat_cit+pat_cit*ind+ap_ratio_1*ind+',f,sep='')

pat_c <- zeroinfl(as.formula(f1), d, dist = "negbin", EM = TRUE)

summary(pat_c)

m1 <- pat_c
m0 <- zeroinfl(lit_t1_c ~ 1, d, dist = "negbin")

1-(logLik(m1)-22)/logLik(m0)  ### McFadden’s pseudo R2 (adjusted)

stargazer(pat_c,summary(pat_c)$coefficients$zero, type="text",
          dep.var.labels=c("Counter litigation"),out="models_c.txt")
