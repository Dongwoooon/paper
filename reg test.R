library(MASS)
library(pscl)
library(boot)
library(dplyr)
library(stargazer)

setwd('E:/apps/feature')

x1 <- read.csv("X_bio_avgna.csv",header=T) 
x2 <- read.csv("X_EE&IT_avgna.csv",header=T) 

x1 <- subset(x1,year>2005)
x2 <- subset(x2,year>2005)

y1 <- read.csv('lit_t1_bio.csv',header=T)
y2 <- read.csv('lit_t1_EE&IT.csv',header=T) 

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

f1 <- paste('lit_t1 ~ app_cit+app_cit*ind+',f,sep='')
f2 <- paste('lit_t1 ~ pat_cit+pat_cit*ind+',f,sep='')

ap <- zeroinfl(as.formula(f1), d, dist = "negbin", EM = TRUE)
pat <- zeroinfl(as.formula(f2), d, dist = "negbin", EM = TRUE)

summary(ap)
summary(pat)

ap1 <- glm.nb(as.formula(f1), data=d)
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

f1 <- paste('lit_t1_nc ~ app_cit+app_cit*ind+',f,sep='')
f2 <- paste('lit_t1_nc ~ pat_cit+pat_cit*ind+',f,sep='')

ap <- zeroinfl(as.formula(f1), d, dist = "negbin", EM = TRUE)
pat <- zeroinfl(as.formula(f2), d, dist = "negbin", EM = TRUE)

summary(ap)
summary(pat)

stargazer(pat,ap,summary(pat)$coefficients$zero, summary(ap)$coefficients$zero, type="text",
          dep.var.labels=c("patent citation","application citation"),out="models_2.txt")

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

f1 <- paste('lit_t1_c ~ app_cit+app_cit*ind+',f,sep='')
f2 <- paste('lit_t1_c ~ pat_cit+pat_cit*ind+',f,sep='')

ap <- zeroinfl(as.formula(f1), d, dist = "negbin", EM = TRUE)
pat <- zeroinfl(as.formula(f2), d, dist = "negbin", EM = TRUE)

summary(ap)
summary(pat)

stargazer(pat,ap,summary(pat)$coefficients$zero, summary(ap)$coefficients$zero, type="text",
          dep.var.labels=c("patent citation","application citation"),out="models_3.txt")

### merged zinb -> fail
d1 <- d %>% select(-c(num_claims))

indep <- names(d1)[5:(length(d1))]
indep <- indep[! indep %in% 'lit_t1']
f=indep[1]

for (i in 2:length(indep)){
  f <- paste(f,indep[i],sep='+')
} 
f <- paste(f,'+year',sep='')

f3 <- paste('lit_t1 ~ pat_cit+pat_cit*ind+app_cit+app_cit*ind+',f,sep='')
f3 <- 'lit_t1 ~ pat_cit+pat_cit*ind+app_cit+app_cit*ind+fwd+self_ratio+self_ratio_fwd+roa+rdi+mkt_4+herf_cls+pstock+emp+year'
mer <- zeroinfl(as.formula(f3), d, dist = "negbin", EM = TRUE)

d2 <- d[d$ind==1,]
f3 <- 'lit_t1 ~ app_cit+num_claims+fwd+self_ratio+self_ratio_fwd+roa+rdi+mkt_4+herf_cls+pstock+emp+year'
mer <- zeroinfl(as.formula(f3), d2, dist = "negbin", EM = TRUE)

summary(mer)

stargazer(pat,ap,mer, summary(pat)$coefficients$zero, summary(ap)$coefficients$zero,summary(mer)$coefficients$zero,
          type="text", dep.var.labels=c("patent citation","application citation"),out="models_2.txt")

### citation, patent stock, m/s, firm size by litigation
d <- df %>% select(lit_t1,app_cit,pat_cit,pstock,mkt_4,emp)
d1 <- df1 %>% select(lit_t1,app_cit,pat_cit,pstock,mkt_4,emp)
d1$pstock <- log(d1$pstock)

d11 <- d1[d1$lit_t1==0,]
d12 <- d1[d1$lit_t1>0,]
quantile(d12$lit_t1, prob = seq(0, 1, length = 11), type = 5)
d13 <- d1[d1$lit_t1>=3,]

d2 <- df2 %>% select(lit_t1,app_cit,pat_cit,pstock,mkt_4,emp)
d2$pstock <- log(d2$pstock)
d21 <- d2[d2$lit_t1==0,]
d22 <- d2[d2$lit_t1>0,]

quantile(d22$lit_t1, prob = seq(0, 1, length = 11), type = 5)
d23 <- d2[d2$lit_t1>1,]
d24 <- d2[d2$lit_t1>=7,]

stargazer(d11,d12,d13,d21,d22,d23,d24, type = "text", title="Descriptive statistics", digits=2, out="desc.txt")
