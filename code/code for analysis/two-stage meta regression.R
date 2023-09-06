# load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(optimx)
library(mvmeta)
library(meta)
library(grid)
library(forestploter)
library(forestplot)
library(arm)
library(ggplot2)
library(ggiraphExtra)
library(cowplot)
library(ggcorrplot)
library(tidyverse)
library(caret)
library(car)
library(glmnet)
library(foreign)
library(metafor)
library(Rtsne)
library(ggExtra)
library(ggsci)
library(pastecs)
library(Rmisc)

################################################################################
# load data
################################################################################

setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]

data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]

################################################################################
# form accdata
################################################################################

# 1. form accdata---------------------------------------------------------------

# accdata
cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata = data[,colnames(data) %in% cols]
accdata$inci_as_delta = accdata$inci_as
accdata$acc3 = accdata$hw
for (i in 1992:2019){
  y = i-1990
  accdata[which(accdata$year==y),]$inci_as_delta = accdata[which(accdata$year==y),]$inci_as_delta-
    accdata[which(accdata$year==y-1),]$inci_as
  accdata[which(accdata$year==y),]$acc3 = accdata[which(accdata$year==y),]$hw+
    accdata[which(accdata$year==y-1),]$hw+accdata[which(accdata$year==y-2),]$hw
}
accdata = accdata[which(accdata$year>1),]
accdata$year = accdata$year-2

# accdata_low
accdata_low = accdata[which(accdata$gnigroup=='mid and low income'),]

# accdata_high
accdata_high = data_high[,colnames(data) %in% cols]
accdata_high$acc7 = accdata_high$hw
accdata_high$inci_as_delta = accdata_high$inci_as
for (i in 1996:2019){
  y = i-1990
  accdata_high[which(accdata_high$year==y),]$inci_as_delta = accdata_high[which(accdata_high$year==y),]$inci_as_delta-
    accdata_high[which(accdata_high$year==y-1),]$inci_as
  accdata_high[which(accdata_high$year==y),]$acc7 = accdata_high[which(accdata_high$year==y),]$hw+
    accdata_high[which(accdata_high$year==y-1),]$hw+accdata_high[which(accdata_high$year==y-2),]$hw+
    accdata_high[which(accdata_high$year==y-3),]$hw+accdata_high[which(accdata_high$year==y-4),]$hw+
    accdata_high[which(accdata_high$year==y-5),]$hw+accdata_high[which(accdata_high$year==y-6),]$hw
}
accdata_high = accdata_high[which(accdata_high$year>5),]
accdata_high$year = accdata_high$year-6

################################################################################
# two-stage meta regression of hw:year
################################################################################

country = unique(data$country)
highcountry = unique(data[which(data$gnigroup=='high income'),]$country)
lowcountry = unique(data[which(data$gnigroup=='mid and low income'),]$country)

# 1. calculate beta-----------------------------------------------------------
result1 = matrix(0,nrow=length(country),ncol=4,dimnames=list(country,c("beta","SE","Estimate","p")))

for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1$inci_as = scale(data1$inci_as)
  data1$hw = scale(data1$hw)
  data1$gdp = scale(data1$gdp)
  
  model = lm(inci_as~year*hw*gdp,data=data1)
  fit = summary(model)
  beta = signif(fit$coefficients[5,1],3)
  p = signif(fit$coefficients[5,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[5,1],3)
  CI_high = signif(confint(model)[5,2],3)
  result1[i,1:2] = fit$coefficients[5,1:2]
  result1[i,3] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result1[i,4] = p
  
}

write.csv(result1,'beta for year hw in 137 countries.csv')

# 2 forestplot---------------------------------------------------------------

# all 137 countries
result1 = as.data.frame(result1)
result1$beta = as.numeric(result1$beta)
result1$SE = as.numeric(result1$SE)
result1 = result1[order(result1[,1],decreasing = T),]
dat1 <- escalc(measure = "MN", mi =result1[,1], sdi = result1[,2]*sqrt(30),
               ni =rep(1,137),slab=rownames(result1))
res1 <- rma(yi, vi, data = dat1)
png(filename = "forestplot of beta for hw year in 137 countrie.png", 
    width = 2000,height = 8000,units = "px",bg = "transparent",res = 300)
forest(res1,digits=c(2,2),cex=0.9,header=c('country','beta for year:hw'))
dev.off()
summary(res1)


# high-income countries

result_high = result1[which(rownames(result1) %in% highcountry),]
result_high = result_high[order(result_high[,1],decreasing = T),]
dat1 <- escalc(measure = "MN", mi =result_high[,1], sdi = result_high[,2]*sqrt(30),
               ni =rep(1,40),slab=rownames(result_high))
res1 <- rma(yi, vi, data = dat1)
png(filename = "forestplot of beta for hw year in high-income countries.png", 
    width = 2000,height = 8000,units = "px",bg = "transparent",res = 300)
forest(res1,digits=c(2,2),cex=0.9,header=c('country','beta for year:hw'))
dev.off()
summary(res1)


# low- to middle-ncome countries

result_low = result1[which((rownames(result1) %in% lowcountry)),]
result_low = result_low[order(result_low[,1],decreasing = T),]
dat1 <- escalc(measure = "MN", mi =result_low[,1], sdi = result_low[,2]*sqrt(30),
               ni =rep(1,97),slab=rownames(result_low))
res1 <- rma(yi, vi, data = dat1)
png(filename = "forestplot of beta for hw year in mid and low-income countries.png", 
    width = 2000,height = 8000,units = "px",bg = "transparent",res = 300)
forest(res1,digits=c(2,2),cex=0.9,header=c('country','beta for year:hw'))
dev.off()
summary(res1)


################################################################################
# two-stage meta regression of acc:year
################################################################################

# 1. all countries--------------------------------------------------------------

country = unique(data$country)
highcountry = unique(data[which(data$gnigroup=='high income'),]$country)
lowcountry = unique(data[which(data$gnigroup=='mid and low income'),]$country)

# *1.1 calculate beta-----------------------------------------------------------
result1 = matrix(0,nrow=length(country),ncol=3,dimnames=list(country,c("beta","se","p")))

for (i in seq(length(country))){
  
  data1 = accdata[which(accdata$country==country[i]),]
  data1$inci_as = scale(data1$inci_as)
  data1$acc3 = scale(data1$acc3)
  data1$gdp = scale(data1$gdp)
  
  model = lm(inci_as~year*acc3*gdp,data=data1)
  fit = summary(model)
  result1[i,1:3] = fit$coefficients[5,c(1,2,4)]
  
}

write.csv(result1,'beta for year acc3 in 137 countries.csv')

# *1.2 forestplot---------------------------------------------------------------

result1 = result1[order(result1[,1],decreasing = T),]
dat1 <- escalc(measure = "MN", mi =result1[,1], sdi = result1[,2]*sqrt(28),
               ni =rep(1,137),slab=rownames(result1))
res1 <- rma(yi, vi, data = dat1)
png(filename = "forestplot of beta for year acc3 in 137 countrie.png", 
    width = 2000,height = 8000,units = "px",bg = "transparent",res = 300)
forest(res1,digits=c(2,2),cex=0.9,header=c('country','beta for year:acc3'))
dev.off()
summary(res1)


# 2. high-income countries------------------------------------------------------

result_high = matrix(0,nrow=length(highcountry),ncol=4,dimnames=list(highcountry,c("beta","SE","Estimate","Pr(>|t|)")))

for (i in seq(length(highcountry))){
  
  data1 = accdata_high[which(accdata_high$country==highcountry[i]),]
  data1$inci_as = scale(data1$inci_as)
  data1$acc7 = scale(data1$acc7)
  data1$gdp = scale(data1$gdp)
  
  model = lm(inci_as~year*acc7*gdp,data=data1)
  fit = summary(model)
  beta = signif(fit$coefficients[5,1],3)
  p = signif(fit$coefficients[5,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[5,1],3)
  CI_high = signif(confint(model)[5,2],3)
  result_high[i,1:2] = fit$coefficients[5,1:2]
  result_high[i,3] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result_high[i,4] = p
  
}

write.csv(result_high,'beta for year acc7 in high income countries.csv')

result_high = as.data.frame(result_high)
result_high$beta = as.numeric(result_high$beta)
result_high$SE = as.numeric(result_high$SE)
result_high = result_high[order(result_high[,1],decreasing = T),]
dat1 <- escalc(measure = "MN", mi =result_high[,1], sdi = result_high[,2]*sqrt(24),
               ni =rep(1,40),slab=rownames(result_high))
res1 <- rma(yi, vi, data = dat1)
png(filename = "forestplot of beta for year acc7 in high-income countries.png", 
    width = 2000,height = 8000,units = "px",bg = "transparent",res = 300)
forest(res1,digits=c(2,2),cex=0.9,header=c('country','beta for year:acc7'))
dev.off()
summary(res1)


# 3. middle- and low-income countries-------------------------------------------

result_low = matrix(0,nrow=length(lowcountry),ncol=4,dimnames=list(lowcountry,c("beta","SE","Estimate","Pr(>|t|)")))

for (i in seq(length(lowcountry))){
  
  data1 = accdata_low[which(accdata_low$country==lowcountry[i]),]
  data1$inci_as = scale(data1$inci_as)
  data1$acc3 = scale(data1$acc3)
  data1$gdp = scale(data1$gdp)
  
  
  model = lm(inci_as~year*acc3*gdp,data=data1)
  fit = summary(model)
  beta = signif(fit$coefficients[5,1],3)
  p = signif(fit$coefficients[5,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[5,1],3)
  CI_high = signif(confint(model)[5,2],3)
  result_low[i,1:2] = fit$coefficients[5,1:2]
  result_low[i,3] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result_low[i,4] = p
  
}

write.csv(result_low,'beta for year acc3 in low income countries.csv')
result_low = as.data.frame(result_low)
result_low$beta = as.numeric(result_low$beta)
result_low$SE = as.numeric(result_low$SE)
result_low = result_low[order(result_low[,1],decreasing = T),]
dat1 <- escalc(measure = "MN", mi =result_low[,1], sdi = result_low[,2]*sqrt(28),
               ni =rep(1,97),slab=rownames(result_low))
res1 <- rma(yi, vi, data = dat1)
png(filename = "forestplot of beta for year acc3 in mid and low-income countries.png", 
    width = 2000,height = 8000,units = "px",bg = "transparent",res = 300)
forest(res1,digits=c(2,2),cex=0.9,header=c('country','beta for year:acc3'))
dev.off()
summary(res1)





### middle- and low-income countries controlling mort_as------------------------


result_low = matrix(0,nrow=length(lowcountry),ncol=4,dimnames=list(lowcountry,c("beta","SE","Estimate","Pr(>|t|)")))

for (i in seq(length(lowcountry))){
  
  data1 = accdata_low[which(accdata_low$country==lowcountry[i]),]
  data1$inci_as = scale(data1$inci_as)
  data1$acc3 = scale(data1$acc3)
  data1$gdp = scale(data1$gdp)
  data1$mort_as = scale(data1$mort_as)
  
  
  model = lm(inci_as~year*acc3*gdp+mort_as,data=data1)
  fit = summary(model)
  beta = signif(fit$coefficients[6,1],3)
  p = signif(fit$coefficients[6,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[6,1],3)
  CI_high = signif(confint(model)[6,2],3)
  result_low[i,1:2] = fit$coefficients[6,1:2]
  result_low[i,3] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result_low[i,4] = p
  
}

result_low = as.data.frame(result_low)
result_low$beta = as.numeric(result_low$beta)
result_low$SE = as.numeric(result_low$SE)
result_low = result_low[order(result_low[,1],decreasing = T),]
dat1 <- escalc(measure = "MN", mi =result_low[,1], sdi = result_low[,2]*sqrt(28),
               ni =rep(1,97),slab=rownames(result_low))
res1 <- rma(yi, vi, data = dat1)
summary(res1)
