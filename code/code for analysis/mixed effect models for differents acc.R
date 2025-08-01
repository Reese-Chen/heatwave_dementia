# load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(tidymodels)
library(reshape)
library(car)
library(apaTables)
library(effectsize)

################################################################################
# load data
################################################################################

setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]

################################################################################
# data reformation
################################################################################

# 1. form accdata--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup')
accdata1 = data[,colnames(data) %in% cols]
accdata2 = data[,colnames(data) %in% cols]
accdata3 = data[,colnames(data) %in% cols]
accdata4 = data[,colnames(data) %in% cols]
accdata5 = data[,colnames(data) %in% cols]
accdata6 = data[,colnames(data) %in% cols]
accdata7 = data[,colnames(data) %in% cols]
accdata8 = data[,colnames(data) %in% cols]

# accdata1
accdata1$acc1 = accdata1$hw
accdata1_high = accdata1[which(accdata1$gnigroup=='high income'),]
accdata1_low = accdata1[which(accdata1$gnigroup=='mid and low income'),]

# accdata2
accdata2$acc2 = accdata2$hw
for (i in 1991:2019){
  y = i-1990
  accdata2[which(accdata2$year==y),]$acc2 = accdata2[which(accdata2$year==y),]$hw+
    accdata2[which(accdata2$year==y-1),]$hw
}
accdata2 = accdata2[which(accdata2$year>0),]
accdata2$year = accdata2$year-1

accdata2_high = accdata2[which(accdata2$gnigroup=='high income'),]
accdata2_low = accdata2[which(accdata2$gnigroup=='mid and low income'),]

#accdata3
accdata3$acc3 = accdata3$hw
for (i in 1992:2019){
  y = i-1990
  accdata3[which(accdata3$year==y),]$acc3 = accdata3[which(accdata3$year==y),]$hw+
    accdata3[which(accdata3$year==y-1),]$hw+
    accdata3[which(accdata3$year==y-2),]$hw
}
accdata3 = accdata3[which(accdata3$year>1),]
accdata3$year = accdata3$year-2

accdata3_high = accdata3[which(accdata3$gnigroup=='high income'),]
accdata3_low = accdata3[which(accdata3$gnigroup=='mid and low income'),]

#accdata4
accdata4$acc4 = accdata4$hw
for (i in 1993:2019){
  y = i-1990
  accdata4[which(accdata4$year==y),]$acc4 = accdata4[which(accdata4$year==y),]$hw+
    accdata4[which(accdata4$year==y-1),]$hw+
    accdata4[which(accdata4$year==y-2),]$hw+
    accdata4[which(accdata4$year==y-3),]$hw
}
accdata4 = accdata4[which(accdata4$year>2),]
accdata4$year = accdata4$year-3

accdata4_high = accdata4[which(accdata4$gnigroup=='high income'),]
accdata4_low = accdata4[which(accdata4$gnigroup=='mid and low income'),]

#accdata5
accdata5$acc5 = accdata5$hw
for (i in 1994:2019){
  y = i-1990
  accdata5[which(accdata5$year==y),]$acc5 = accdata5[which(accdata5$year==y),]$hw+
    accdata5[which(accdata5$year==y-1),]$hw+
    accdata5[which(accdata5$year==y-2),]$hw+
    accdata5[which(accdata5$year==y-3),]$hw+
    accdata5[which(accdata5$year==y-4),]$hw
}
accdata5 = accdata5[which(accdata5$year>3),]
accdata5$year = accdata5$year-4

accdata5_high = accdata5[which(accdata5$gnigroup=='high income'),]
accdata5_low = accdata5[which(accdata5$gnigroup=='mid and low income'),]

#accdata6
accdata6$acc6 = accdata6$hw
for (i in 1995:2019){
  y = i-1990
  accdata6[which(accdata6$year==y),]$acc6 = accdata6[which(accdata6$year==y),]$hw+
    accdata6[which(accdata6$year==y-1),]$hw+
    accdata6[which(accdata6$year==y-2),]$hw+
    accdata6[which(accdata6$year==y-3),]$hw+
    accdata6[which(accdata6$year==y-4),]$hw+
    accdata6[which(accdata6$year==y-5),]$hw
}
accdata6 = accdata6[which(accdata6$year>4),]
accdata6$year = accdata6$year-5

accdata6_high = accdata6[which(accdata6$gnigroup=='high income'),]
accdata6_low = accdata6[which(accdata6$gnigroup=='mid and low income'),]


#accdata7
accdata7$acc7 = accdata7$hw
for (i in 1996:2019){
  y = i-1990
  accdata7[which(accdata7$year==y),]$acc7 = accdata7[which(accdata7$year==y),]$hw+
    accdata7[which(accdata7$year==y-1),]$hw+accdata7[which(accdata7$year==y-2),]$hw+
    accdata7[which(accdata7$year==y-3),]$hw+accdata7[which(accdata7$year==y-4),]$hw+
    accdata7[which(accdata7$year==y-5),]$hw+accdata7[which(accdata7$year==y-6),]$hw
}
accdata7 = accdata7[which(accdata7$year>5),]
accdata7$year = accdata7$year-6

accdata7_high = accdata7[which(accdata7$gnigroup=='high income'),]
accdata7_low = accdata7[which(accdata7$gnigroup=='mid and low income'),]

#accdata8
accdata8$acc8 = accdata8$hw
for (i in 1997:2019){
  y = i-1990
  accdata8[which(accdata8$year==y),]$acc8 = accdata8[which(accdata8$year==y),]$hw+
    accdata8[which(accdata8$year==y-1),]$hw+
    accdata8[which(accdata8$year==y-2),]$hw+
    accdata8[which(accdata8$year==y-3),]$hw+
    accdata8[which(accdata8$year==y-4),]$hw+
    accdata8[which(accdata8$year==y-5),]$hw+
    accdata8[which(accdata8$year==y-6),]$hw+
    accdata8[which(accdata8$year==y-7),]$hw
}
accdata8 = accdata8[which(accdata8$year>6),]
accdata8$year = accdata8$year-7

accdata8_high = accdata8[which(accdata8$gnigroup=='high income'),]
accdata8_low = accdata8[which(accdata8$gnigroup=='mid and low income'),]

################################################################################
# mixed effect models
################################################################################

# 1.  data preprocessing--------------------------------------------------------

scale_by_country <- function(df) {
  countries <- unique(df$country)
  scaled_df <- data.frame()
  
  for (c in countries) {
    subset <- df[df$country == c, ]
    subset[, c(2:3, 7)] <- scale(subset[, c(2:3, 7)])
    scaled_df <- rbind(scaled_df, subset)
  }
  
  return(scaled_df)
}

# all countries

accdata1 <- scale_by_country(accdata1)
accdata2 <- scale_by_country(accdata2)
accdata3 <- scale_by_country(accdata3)
accdata4 <- scale_by_country(accdata4)
accdata5 <- scale_by_country(accdata5)
accdata6 <- scale_by_country(accdata6)
accdata7 <- scale_by_country(accdata7)
accdata8 <- scale_by_country(accdata8)

# high income countries

accdata1_high <- scale_by_country(accdata1_high)
accdata2_high <- scale_by_country(accdata2_high)
accdata3_high <- scale_by_country(accdata3_high)
accdata4_high <- scale_by_country(accdata4_high)
accdata5_high <- scale_by_country(accdata5_high)
accdata6_high <- scale_by_country(accdata6_high)
accdata7_high <- scale_by_country(accdata7_high)
accdata8_high <- scale_by_country(accdata8_high)

# low- to middle income countries

accdata1_low <- scale_by_country(accdata1_low)
accdata2_low <- scale_by_country(accdata2_low)
accdata3_low <- scale_by_country(accdata3_low)
accdata4_low <- scale_by_country(accdata4_low)
accdata5_low <- scale_by_country(accdata5_low)
accdata6_low <- scale_by_country(accdata6_low)
accdata7_low <- scale_by_country(accdata7_low)
accdata8_low <- scale_by_country(accdata8_low)

# 2.  mixed effect model--------------------------------------------------------

data_plot = matrix(0,nrow=24,ncol=5,dimnames=list(rep(c(1:8),each=3),c('Countries','ACC','eta','eta_low','eta_upper')))
data_plot[,1] = rep(c('All','High-income','Low- to middle-income'),8)
data_plot[,2] = rep(c(1:8),each=3)

for (i in 1:8){
  fomula = paste0('inci_as~acc',i,'*year*gdp+(1+year|country)')
  
  data_all = get(paste0('accdata',i))
  model_all = lmer(fomula,data=data_all,control = lmerControl(optimizer ="Nelder_Mead"))
  eta = eta_squared(model_all, partial = TRUE, squared = TRUE, ci=0.95,alternative = "two.sided")
  data_plot[(i-1)*3+1,3] = eta$Eta2_partial[4]
  data_plot[(i-1)*3+1,4] = eta$CI_low[4]
  data_plot[(i-1)*3+1,5] = eta$CI_high[4]
  
  data_high = get(paste0('accdata',i,'_high'))
  model_high = lmer(fomula,data=data_high,control = lmerControl(optimizer ="Nelder_Mead"))
  eta = eta_squared(model_high, partial = TRUE, squared = TRUE, ci=0.95,alternative = "two.sided")
  data_plot[(i-1)*3+2,3] = eta$Eta2_partial[4]
  data_plot[(i-1)*3+2,4] = eta$CI_low[4]
  data_plot[(i-1)*3+2,5] = eta$CI_high[4]
  
  data_low = get(paste0('accdata',i,'_low'))
  model_low = lmer(fomula,data=data_low,control = lmerControl(optimizer ="Nelder_Mead"))
  eta = eta_squared(model_low, partial = TRUE, squared = TRUE, ci=0.95,alternative = "two.sided")
  data_plot[(i-1)*3+3,3] = eta$Eta2_partial[4]
  data_plot[(i-1)*3+3,4] = eta$CI_low[4]
  data_plot[(i-1)*3+3,5] = eta$CI_high[4]
}

data_plot = as.data.frame(data_plot)
data_plot[, 2:5] = lapply(data_plot[, 2:5], as.numeric)
data_plot$Countries = as.factor(data_plot$Countries)

p1 = ggplot(data=data_plot,aes(x=ACC,y=eta,group=Countries,color=Countries))+
  geom_point(size=3)+
  geom_line(linewidth=1.5)+
  geom_errorbar(aes(x=ACC,ymin=eta_low,ymax=eta_upper,group=Countries,color=Countries),size=1) +
  xlab('Accumulated year')+
  ylab('Impact of Accumulative Heatwaves')+
  scale_colour_manual(name="Countries",values = 
                        c('All'='#42B540','High-income'='#01468B',
                          'Low- to middle-income'='#EC0000'))+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),
        legend.position = "top",
        legend.key = element_rect(colour ="transparent",fill="transparent"))

ggsave(p1,filename = 'CI for different acc 1.pdf',
       width=5,height=5)

p2 = ggplot(data=data_plot,aes(x=ACC,y=eta,group=Countries,color=Countries))+
  geom_point(size=5)+
  geom_line(linewidth=2)+
  geom_ribbon(aes(ymin=eta_low,ymax=eta_upper,fill=Countries),alpha = 0.15,color=NA)+
  xlab('Cumulative year')+
  ylab('Effect size of heatwave')+
  scale_colour_manual(name="Countries",values = 
                        c('All'='#42B540','High-income'='#01468B',
                          'Low- to middle-income'='#EC0000'))+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),
        legend.position = "top",
        legend.key = element_rect(colour ="transparent",fill="transparent"))

ggsave(p2,filename = 'CI for different acc new.pdf',
       width=4,height=4)

