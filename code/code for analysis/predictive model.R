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
library(reshape2)
library(ggsignif)

################################################################################
# load data
################################################################################

setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]
country = unique(data$country)
data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]

################################################################################
# form accdata and incrementdata
################################################################################

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
# predictive models to calculate attributable incidence number
################################################################################

# 1. all countries--------------------------------------------------------------

model1 = lmer(inci_as~year*acc3*gdp+(1+year|country),data = accdata)
summary(model1)

pred_data = accdata
pred_data$acc3 = rep(accdata[which(accdata$year==0),]$acc3,28)
pred_data$inci_as_pred = predict(model1,newdata=pred_data)

# *1.1 visualization------------------------------------------------------------
plot_data1 = aggregate(accdata$inci_as,list(accdata$year),mean)
plot_data2 = aggregate(pred_data$inci_as_pred,list(pred_data$year),mean)
plot_data = cbind(plot_data1$Group.1+1992,plot_data1$x,plot_data2$x)
plot_data = as.data.frame(plot_data)
colnames(plot_data) = c('year','inci_as','inci_as_pred')

p1 = ggplot(plot_data)+
  geom_point(aes(x = year, y = inci_as,color='real'),size=10,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as,color='real'),linewidth=4)+
  geom_point(aes(x = year, y = inci_as_pred,color='predict'),size=10,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as_pred,color='predict'),linewidth=4)+
  xlab("year")+
  ylab("age-standardized incidence")+
  theme_bw()+
  theme(text = element_text(size = 30))+
  scale_colour_manual(name="",values=c('real'='#01468B','predict'='#EC0000'))

ggsave(p1,filename = paste('predict and real AS incidence using constant acc3.png'),
       width=13,height=10)

# *1.2 attributable incidence number--------------------------------------------
pop_diff = matrix(0,nrow=137,ncol=28,dimnames=list(country,1992:2019))

for (i in 1:137){
  data1 = pred_data[which(pred_data$country==country[i]),]
  diff = data1$inci_as-data1$inci_as_pred
  pop_diff[i,] = diff*data$pop[which(data$country==country[i] & data$year>1)]/100000
}

sum(pop_diff)

write.csv(pop_diff,'population difference predicted using constant acc3 and predicted incidence.csv')

pop_diff_year = colSums(pop_diff)
hist_by_year = cbind(c(1992:2019),pop_diff_year)
hist_by_year = as.data.frame(hist_by_year)
colnames(hist_by_year) = c('Year','number')

p1 = ggplot(hist_by_year, aes(x=Year, y=number))+
  geom_bar(stat="identity", fill="#01468B", colour="black")+
  xlab('Year')+
  ylab('Attributable incidence number')+
  theme_bw()+
  theme(text = element_text(size = 30))
ggsave(p1,filename = paste('histogram of attributable incidence number 1.png'),
       width=10,height=10)


# 2. high income countries------------------------------------------------------


model2 = lmer(inci_as~acc7*year*gdp+(1+year|country),data = accdata_high)
summary(model2)

pred_data = accdata_high
pred_data$acc7 = rep(accdata_high[which(accdata_high$year==0),]$acc7,24)
pred_data$inci_as_pred0 = predict(model2,newdata=accdata_high)
pred_data$inci_as_pred = predict(model2,newdata=pred_data)

# *2.1 evaluate the prediction--------------------------------------------------

# calculate different regression indexed
res=residuals(model2,type="pearson")
ks.test(res, "pnorm")
AIC(model2)

mae <- function(x,y) mean(abs(y - x))
RSq = function(x, y) {cor(x, y)^2}

country_high = unique(accdata_high$country)
result_high = matrix(nrow = 40,ncol=4,dimnames = list(country_high,c('RMSE','R-squared','cor','MAE')))
for (i in 1:40){
  data1 = pred_data[which(pred_data$country==country_high[i]),]
  yhat = data1$inci_as_pred0
  ytrue = data1$inci_as
  result_high[i,1] = RMSE(ytrue,yhat)
  result_high[i,2] = RSq(ytrue,yhat)
  result_high[i,3] = cor(ytrue,yhat)
  result_high[i,4] = mae(ytrue,yhat)
}

write.csv(result_high,'prediction effect in high income countries.csv')


# compare with model without hw
model2_compare = lmer(inci_as~year*gdp+(1+year|country),data = accdata_high)
summary(model2_compare)
pred_data_compare = accdata_high
pred_data_compare$inci_as_pred_compare = predict(model2_compare,newdata=accdata_high)

result_high_compare = matrix(nrow = 40,ncol=4,dimnames = list(country_high,c('RMSE','R-squared','cor','MAE')))
for (i in 1:40){
  data1 = pred_data_compare[which(pred_data_compare$country==country_high[i]),]
  yhat = data1$inci_as_pred_compare
  ytrue = data1$inci_as
  result_high_compare[i,1] = RMSE(ytrue,yhat)
  result_high_compare[i,2] = RSq(ytrue,yhat)
  result_high_compare[i,3] = cor(ytrue,yhat)
  result_high_compare[i,4] = mae(ytrue,yhat)
}

write.csv(result_high_compare,'prediction effect in high income countries without acc.csv')

# *2.2 visualization------------------------------------------------------------

plot_data0 = aggregate(pred_data$inci_as_pred0,list(pred_data$year),mean)
plot_data1 = aggregate(pred_data$inci_as,list(pred_data$year),mean)
plot_data2 = aggregate(pred_data$inci_as_pred,list(pred_data$year),mean)
plot_data = cbind(plot_data1$Group.1+1996,plot_data0$x,plot_data1$x,plot_data2$x)
plot_data = as.data.frame(plot_data)
colnames(plot_data) = c('year','inci_as_pred0','inci_as','inci_as_pred')

p1 = ggplot(plot_data)+
  geom_point(aes(x = year, y = inci_as,color='Real'),size=10,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as,color='Real'),linewidth=4)+
  geom_point(aes(x = year, y = inci_as_pred0,color='Predict using real ACC7'),size=10,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as_pred0,color='Predict using real ACC7'),linewidth=4)+
  geom_point(aes(x = year, y = inci_as_pred,color='Predict using constant ACC7'),size=10,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as_pred,color='Predict using constant ACC7'),linewidth=4)+
  xlab("Year")+
  ylab("Age-standardized incidence")+
  theme_minimal()+
  theme(text = element_text(size = 30),
        legend.position = c(.3,.95),
        legend.key = element_rect(colour ="transparent",fill="transparent"))+
  scale_colour_manual(name="",values=c('Real'='#01468B','Predict using real ACC7'='#42B540',
                                       'Predict using constant ACC7'='#EC0000'))

ggsave(p1,filename = paste('predict and real AS incidence using constant acc7 in high.png'),
       width=10,height=10)

# *2.3 attributable incidence number--------------------------------------------
country_high = unique(accdata_high$country)
pop_diff_high = matrix(0,nrow=40,ncol=24,dimnames=list(country_high,1996:2019))

for (i in 1:40){
  data1 = pred_data[which(pred_data$country==country_high[i]),]
  diff = data1$inci_as_pred0-data1$inci_as_pred
  pop_diff_high[i,] = diff*data$pop[which(data$country==country[i] & data$year>5)]/100000
}

sum(pop_diff_high)

write.csv(pop_diff_high,'population difference predicted using constant acc7 and predicted incidence in high.csv')


# 3. mid and low income countries-----------------------------------------------

model3 = lmer(inci_as~year*acc3*gdp+(1+year|country),data = accdata_low)
summary(model3)

pred_data = accdata_low
pred_data$acc3 = rep(accdata_low[which(accdata_low$year==0),]$acc3,28)
pred_data$inci_as_pred0 = predict(model3,newdata=accdata_low)
pred_data$inci_as_pred = predict(model3,newdata=pred_data)

# *3.1 evaluate the prediction--------------------------------------------------

res=residuals(model3,type="pearson")
ks.test(res, "pnorm")
AIC(model3)

mae <- function(x,y) mean(abs(y - x))
RSq = function(x, y) {cor(x, y)^2}

country_low = unique(accdata_low$country)
result_low = matrix(nrow = 97,ncol=4,dimnames = list(country_low,c('RMSE','R-squared','cor','MAE')))
for (i in 1:97){
  data1 = pred_data[which(pred_data$country==country_low[i]),]
  yhat = data1$inci_as_pred0
  ytrue = data1$inci_as
  result_low[i,1] = RMSE(ytrue,yhat)
  result_low[i,2] = RSq(ytrue,yhat)
  result_low[i,3] = cor(ytrue,yhat)
  result_low[i,4] = mae(ytrue,yhat)
}

write.csv(result_low,'prediction effect in low to middle income countries.csv')

# compare with model without hw
model3_compare = lmer(inci_as~year*gdp+(1+year|country),data = accdata_low)
summary(model3_compare)
pred_data_compare = accdata_low
pred_data_compare$inci_as_pred_compare = predict(model3_compare,newdata=accdata_low)

result_low_compare = matrix(nrow = 97,ncol=4,dimnames = list(country_low,c('RMSE','R-squared','cor','MAE')))
for (i in 1:97){
  data1 = pred_data_compare[which(pred_data_compare$country==country_low[i]),]
  yhat = data1$inci_as_pred_compare
  ytrue = data1$inci_as
  result_low_compare[i,1] = RMSE(ytrue,yhat)
  result_low_compare[i,2] = RSq(ytrue,yhat)
  result_low_compare[i,3] = cor(ytrue,yhat)
  result_low_compare[i,4] = mae(ytrue,yhat)
}

write.csv(result_low_compare,'prediction effect in low income countries without acc.csv')


# *3.2 visualization------------------------------------------------------------
plot_data0 = aggregate(pred_data$inci_as_pred0,list(pred_data$year),mean)
plot_data1 = aggregate(pred_data$inci_as,list(pred_data$year),mean)
plot_data2 = aggregate(pred_data$inci_as_pred,list(pred_data$year),mean)
plot_data = cbind(plot_data1$Group.1+1992,plot_data0$x,plot_data1$x,plot_data2$x)
plot_data = as.data.frame(plot_data)
colnames(plot_data) = c('year','inci_as_pred0','inci_as','inci_as_pred')

p1 = ggplot(plot_data)+
  geom_point(aes(x = year, y = inci_as,color='Recorded'),size=4,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as,color='Recorded'),linewidth=2)+
  geom_point(aes(x = year, y = inci_as_pred0,color='Predicted using recorded ACC3'),size=4,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as_pred0,color='Predicted using recorded ACC3'),linewidth=2)+
  geom_point(aes(x = year, y = inci_as_pred,color='Predicted using constant ACC3'),size=4,alpha=0.5)+
  geom_line(aes(x = year, y = inci_as_pred,color='Predicted using constant ACC3'),linewidth=2)+
  xlab("Year")+
  ylab("Age-standardized incidence")+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),
        legend.position = c(.35,.9),
        legend.key = element_rect(colour ="transparent",fill="transparent"),
        axis.text.x= element_text(angle=60, hjust=1))+
  scale_colour_manual(name="",values=c('Recorded'='#01468B',
                                       'Predicted using recorded ACC3'='#42B540',
                                       'Predicted using constant ACC3'='#EC0000'))

ggsave(p1,filename = paste('predict and real AS incidence using constant acc3 in mid and low.pdf'),
       width=4,height=4)

# *3.3 attributable incidence number--------------------------------------------
country_low = unique(accdata_low$country)
pop_diff_low = matrix(0,nrow=97,ncol=28,dimnames=list(country_low,1992:2019))

for (i in 1:97){
  data1 = pred_data[which(pred_data$country==country_low[i]),]
  diff = data1$inci_as_pred0-data1$inci_as_pred
  pop_diff_low[i,] = diff*data$pop[which(data$country==country[i] & data$year>1)]/100000
}

sum(pop_diff_low)

write.csv(pop_diff_low,'population difference predicted using constant acc3 and predicted incidence in mid and low.csv')

# 4. visualization--------------------------------------------------------------

RMSE_result = c(result_high[,1],result_high_compare[,1],result_low[,1],result_low_compare[,1])
RMSE_result = as.data.frame(RMSE_result)
RMSE_result$Countries = c(rep('High-income',80),rep('Low- to middle-income',194))
RMSE_result$Models = c(rep('With heatwave',40),rep('Without heatwave',40),
                       rep('With heatwave',97),rep('Without heatwave',97))
colnames(RMSE_result) = c('RMSE','Countries','Models')
RMSE_result$Countries = as.factor(RMSE_result$Countries)
RMSE_result$Models = as.factor(RMSE_result$Models)
                  
p1 = ggplot(RMSE_result,aes(x=Countries,y=RMSE,color=Models))+
  geom_boxplot(linewidth=1,fill='white')+
  geom_point(shape=20,size=2,
             position = position_jitterdodge(jitter.width = 0.1, seed = 202), show.legend = FALSE)+
  stat_boxplot(geom='errorbar',size=1,show.legend = FALSE)+
  stat_compare_means(aes(group=Models),label="p.signif",
                     method = "t.test",paired = TRUE,show.legend = FALSE)+
  geom_signif(annotations = c("",""),
              y_position = c(1.85,1.85),
              xmin = c(0.8,1.8),
              xmax = c(1.2,2.2),
              tip_length = c(0,0,0,0),
              color='black')+
  ylab("RMSE")+
  scale_color_manual(values=c('#EC0000','#01468B'))+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),
        legend.position = c(0.2,.85),
        legend.key = element_rect(colour ="transparent",fill="transparent"))

ggsave(p1,filename='boxplot of comparision of RMSE in prediction with and without acc.pdf',
       width=4,height=4)

# t test
RMSE_result1 = RMSE_result[which(RMSE_result$Countries=='High-income'),]
t_test <- t.test(RMSE~Models, RMSE_result1, paired = TRUE, alternative = 'two.sided')
t_test

RMSE_result2 = RMSE_result[which(RMSE_result$Countries=='Low- to middle-income'),]
t_test <- t.test(RMSE~Models, RMSE_result2, paired = TRUE, alternative = 'two.sided')
t_test













