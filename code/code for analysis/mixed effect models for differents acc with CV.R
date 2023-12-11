# load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(tidymodels)
library(reshape)

################################################################################
# load data
################################################################################

setwd("d:/heatwave and dementia/data")
data_origin = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data_origin = data_origin[,-1]

################################################################################
# data reformation
################################################################################

# 0. split data-----------------------------------------------------------------

data_split = data_origin[which(data_origin$year==0),]
split = initial_split(data_split,prop=0.5,strata='gnigroup')
data_split1 = training(split) # to change into another group, use training/testing
data = data_origin[which(data_origin$country %in% data_split1$country),]

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

# all countries
accdata1$gdp = log(accdata1$gdp)
accdata2$gdp = log(accdata2$gdp)
accdata3$gdp = log(accdata3$gdp)
accdata4$gdp = log(accdata4$gdp)
accdata5$gdp = log(accdata5$gdp)
accdata6$gdp = log(accdata6$gdp)
accdata7$gdp = log(accdata7$gdp)
accdata8$gdp = log(accdata8$gdp)

accdata1[,c(1:3,7)] = scale(accdata1[,c(1:3,7)])
accdata2[,c(1:3,7)] = scale(accdata2[,c(1:3,7)])
accdata3[,c(1:3,7)] = scale(accdata3[,c(1:3,7)])
accdata4[,c(1:3,7)] = scale(accdata4[,c(1:3,7)])
accdata5[,c(1:3,7)] = scale(accdata5[,c(1:3,7)])
accdata6[,c(1:3,7)] = scale(accdata6[,c(1:3,7)])
accdata7[,c(1:3,7)] = scale(accdata7[,c(1:3,7)])
accdata8[,c(1:3,7)] = scale(accdata8[,c(1:3,7)])

# high income countries
accdata1_high$gdp = log(accdata1_high$gdp)
accdata2_high$gdp = log(accdata2_high$gdp)
accdata3_high$gdp = log(accdata3_high$gdp)
accdata4_high$gdp = log(accdata4_high$gdp)
accdata5_high$gdp = log(accdata5_high$gdp)
accdata6_high$gdp = log(accdata6_high$gdp)
accdata7_high$gdp = log(accdata7_high$gdp)
accdata8_high$gdp = log(accdata8_high$gdp)

accdata1_high[,c(1:3,7)] = scale(accdata1_high[,c(1:3,7)])
accdata2_high[,c(1:3,7)] = scale(accdata2_high[,c(1:3,7)])
accdata3_high[,c(1:3,7)] = scale(accdata3_high[,c(1:3,7)])
accdata4_high[,c(1:3,7)] = scale(accdata4_high[,c(1:3,7)])
accdata5_high[,c(1:3,7)] = scale(accdata5_high[,c(1:3,7)])
accdata6_high[,c(1:3,7)] = scale(accdata6_high[,c(1:3,7)])
accdata7_high[,c(1:3,7)] = scale(accdata7_high[,c(1:3,7)])
accdata8_high[,c(1:3,7)] = scale(accdata8_high[,c(1:3,7)])

# low- to middle income countries
accdata1_low$gdp = log(accdata1_low$gdp)
accdata2_low$gdp = log(accdata2_low$gdp)
accdata3_low$gdp = log(accdata3_low$gdp)
accdata4_low$gdp = log(accdata4_low$gdp)
accdata5_low$gdp = log(accdata5_low$gdp)
accdata6_low$gdp = log(accdata6_low$gdp)
accdata7_low$gdp = log(accdata7_low$gdp)
accdata8_low$gdp = log(accdata8_low$gdp)

accdata1_low[,c(1:3,7)] = scale(accdata1_low[,c(1:3,7)])
accdata2_low[,c(1:3,7)] = scale(accdata2_low[,c(1:3,7)])
accdata3_low[,c(1:3,7)] = scale(accdata3_low[,c(1:3,7)])
accdata4_low[,c(1:3,7)] = scale(accdata4_low[,c(1:3,7)])
accdata5_low[,c(1:3,7)] = scale(accdata5_low[,c(1:3,7)])
accdata6_low[,c(1:3,7)] = scale(accdata6_low[,c(1:3,7)])
accdata7_low[,c(1:3,7)] = scale(accdata7_low[,c(1:3,7)])
accdata8_low[,c(1:3,7)] = scale(accdata8_low[,c(1:3,7)])

# 2.  mixed effect model--------------------------------------------------------

data_plot = matrix(0,nrow=8,ncol=4,dimnames=list(1:8,c('ACC','All 137 countries','High-income','Low- to middle-income')))
data_plot[,1] = c(1:8)
for (i in 1:8){
  fomula = paste0('inci_as~acc',i,'*year*gdp+(1+year|country)')
  
  data_all = get(paste0('accdata',i))
  model_all = lmer(fomula,data=data_all,control = lmerControl(optimizer ="Nelder_Mead"))
  fit_all = summary(model_all)
  df = fit_all$coefficients[5,3]
  t = fit_all$coefficients[5,4]
  data_plot[i,2] = t*t/(t*t+df)
  
  data_high = get(paste0('accdata',i,'_high'))
  model_high = lmer(fomula,data=data_high,control = lmerControl(optimizer ="Nelder_Mead"))
  fit_high = summary(model_high)
  df = fit_high$coefficients[5,3]
  t = fit_high$coefficients[5,4]
  data_plot[i,3] = t*t/(t*t+df)
  
  data_low = get(paste0('accdata',i,'_low'))
  model_low = lmer(fomula,data=data_low,control = lmerControl(optimizer ="Nelder_Mead"))
  fit_low = summary(model_low)
  df = fit_low$coefficients[5,3]
  t = fit_low$coefficients[5,4]
  data_plot[i,4] = t*t/(t*t+df)
}


################################################################################
# visualization and comparison
################################################################################

data_plot = as.data.frame(data_plot)
colnames(data_plot) = c('acc','all','high','low')
p1 = ggplot(data=data_plot)+
  geom_point(aes(x=acc,y=all,color='All 137 countries'),size=4)+
  geom_line(aes(x=acc,y=all,color='All 137 countries'),size=2)+
  geom_point(aes(x=acc,y=high,color='High-income'),size=4)+
  geom_line(aes(x=acc,y=high,color='High-income'),size=2)+
  geom_point(aes(x=acc,y=low,color='Low- to middle-income'),size=4)+
  geom_line(aes(x=acc,y=low,color='Low- to middle-income'),size=2)+
  xlab('Accumulated year')+
  ylab('Partial-eta-squared')+
  scale_colour_manual(name="Countries",values = 
                        c('All 137 countries'='#42B540','High-income'='#01468B',
                          'Low- to middle-income'='#EC0000'))+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),
        legend.position = c(.7,.15),
        legend.key = element_rect(colour ="transparent",fill="transparent"))

ggsave(p1,filename = 'Partial-eta-squared for different acc in training5.pdf',
       width=4,height=4)

################################################################################
# permutation test with same year data
################################################################################

all_result = matrix(0,nrow=1000,ncol=8)
high_result = matrix(0,nrow=1000,ncol=8)
low_result = matrix(0,nrow=1000,ncol=8)

for (i in 1:1000){
  
  # split data
  data_split = data_origin[which(data_origin$year==0),]
  split = initial_split(data_split,prop=0.8,strata='gnigroup')
  data_split = training(split) 
  data = data_origin[which(data_origin$country %in% data_split$country),]
  
  # reform data
  cols = c('inci_as','hw','gdp','year','country','gnigroup')
  accdata = data[,colnames(data) %in% cols]
  accdata$acc1 = accdata$hw
  accdata$acc2 = accdata$hw
  accdata$acc3 = accdata$hw
  accdata$acc4 = accdata$hw
  accdata$acc5 = accdata$hw
  accdata$acc6 = accdata$hw
  accdata$acc7 = accdata$hw
  accdata$acc8 = accdata$hw
  for (j in 1997:2019){
    y = j-1990
    accdata[which(accdata$year==y),]$acc2 = accdata[which(accdata$year==y),]$hw+
      accdata[which(accdata$year==y-1),]$hw
    accdata[which(accdata$year==y),]$acc3 = accdata[which(accdata$year==y),]$hw+
      accdata[which(accdata$year==y-1),]$hw+
      accdata[which(accdata$year==y-2),]$hw
    accdata[which(accdata$year==y),]$acc4 = accdata[which(accdata$year==y),]$hw+
      accdata[which(accdata$year==y-1),]$hw+
      accdata[which(accdata$year==y-2),]$hw+
      accdata[which(accdata$year==y-3),]$hw
    accdata[which(accdata$year==y),]$acc5 = accdata[which(accdata$year==y),]$hw+
      accdata[which(accdata$year==y-1),]$hw+
      accdata[which(accdata$year==y-2),]$hw+
      accdata[which(accdata$year==y-3),]$hw+
      accdata[which(accdata$year==y-4),]$hw
    accdata[which(accdata$year==y),]$acc6 = accdata[which(accdata$year==y),]$hw+
      accdata[which(accdata$year==y-1),]$hw+
      accdata[which(accdata$year==y-2),]$hw+
      accdata[which(accdata$year==y-3),]$hw+
      accdata[which(accdata$year==y-4),]$hw+
      accdata[which(accdata$year==y-5),]$hw
    accdata[which(accdata$year==y),]$acc7 = accdata[which(accdata$year==y),]$hw+
      accdata[which(accdata$year==y-1),]$hw+
      accdata[which(accdata$year==y-2),]$hw+
      accdata[which(accdata$year==y-3),]$hw+
      accdata[which(accdata$year==y-4),]$hw+
      accdata[which(accdata$year==y-5),]$hw+
      accdata[which(accdata$year==y-6),]$hw
    accdata[which(accdata$year==y),]$acc8 = accdata[which(accdata$year==y),]$hw+
      accdata[which(accdata$year==y-1),]$hw+
      accdata[which(accdata$year==y-2),]$hw+
      accdata[which(accdata$year==y-3),]$hw+
      accdata[which(accdata$year==y-4),]$hw+
      accdata[which(accdata$year==y-5),]$hw+
      accdata[which(accdata$year==y-6),]$hw+
      accdata[which(accdata$year==y-7),]$hw
  }
  
  accdata = accdata[which(accdata$year>6),]
  accdata$year = accdata$year-7
  
  accdata_high = accdata[which(accdata$gnigroup=='high income'),]
  accdata_low = accdata[which(accdata$gnigroup=='mid and low income'),]
  
  # data preprocessing
  accdata$gdp = log(accdata$gdp)
  accdata[,c(1:3,7:14)] = scale(accdata[,c(1:3,7:14)])
  accdata_high$gdp = log(accdata_high$gdp)
  accdata_high[,c(1:3,7:14)] = scale(accdata_high[,c(1:3,7:14)])
  accdata_low$gdp = log(accdata_low$gdp)
  accdata_low[,c(1:3,7:14)] = scale(accdata_low[,c(1:3,7:14)])
  
  # mixed effect models
  eta_result = matrix(0,nrow=8,ncol=4,dimnames=list(1:8,c('ACC','All','High','Low')))
  eta_result[,1] = c(1:8)
  for (j in 1:8){
    fomula = paste0('inci_as~acc',j,'*year*gdp+(1+year|country)')
    
    model = lmer(fomula,data=accdata,control = lmerControl(optimizer ="Nelder_Mead"))
    fit = summary(model)
    df = fit$coefficients[5,3]
    t = fit$coefficients[5,4]
    eta_result[j,2] = t*t/(t*t+df)
    
    model = lmer(fomula,data=accdata_high,control = lmerControl(optimizer ="Nelder_Mead"))
    fit = summary(model)
    df = fit$coefficients[5,3]
    t = fit$coefficients[5,4]
    eta_result[j,3] = t*t/(t*t+df)
    
    model = lmer(fomula,data=accdata_low,control = lmerControl(optimizer ="Nelder_Mead"))
    fit = summary(model)
    df = fit$coefficients[5,3]
    t = fit$coefficients[5,4]
    eta_result[j,4] = t*t/(t*t+df)
  }
  
  # calculate deviations
  for (j in 1:8){
    all_result[i,j] = eta_result[j,2]-eta_result[5,2]
    high_result[i,j] = eta_result[7,3]-eta_result[j,3]
    low_result[i,j] = eta_result[3,4]-eta_result[j,4]
  }
}

# plot the deviation for each acc

#write.csv(all_result,'deviation of eta for acc in all 137 countries same year.csv')
write.csv(high_result,'deviation of eta for acc in high-income countries same year.csv')
write.csv(low_result,'deviation of eta for acc in low- to middle-income countries same year.csv')

high_result1 = t(high_result)
high_result1 = as.data.frame(high_result1)
high_result1$ID = c(1:8)
high_result_long = melt(high_result1,id.vars='ID',variable.name="permutation",value.name="deviation")
high_result_long$ID = as.factor(high_result_long$ID)
high_deviation_same = 
  ggplot(high_result_long, aes(x=ID, y=value,color=ID)) + 
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()+
  xlab('ACC')+
  ylab('Deviation')
ggsave(high_deviation_same,filename = 'deviation of eta for acc in high-income countries same year.pdf',
       width=4,height=4)

low_result1 = t(low_result)
low_result1 = as.data.frame(low_result1)
low_result1$ID = c(1:8)
low_result_long = melt(low_result1,id.vars='ID',variable.name="permutation",value.name="deviation")
low_result_long$ID = as.factor(low_result_long$ID)
low_deviation_same = 
  ggplot(low_result_long, aes(x=ID, y=value,color=ID)) + 
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()+
  xlab('ACC')+
  ylab('Deviation')
ggsave(low_deviation_same,filename = 'deviation of eta for acc in low- to middle-income countries same year.pdf',
       width=4,height=4)

################################################################################
# permutation test with different year data
################################################################################

all_result = matrix(0,nrow=1000,ncol=8)
high_result = matrix(0,nrow=1000,ncol=8)
low_result = matrix(0,nrow=1000,ncol=8)

for (i in 1:1000){
  
  # split data
  data_split = data_origin[which(data_origin$year==0),]
  split = initial_split(data_split,prop=0.8,strata='gnigroup')
  data_split = training(split) 
  data = data_origin[which(data_origin$country %in% data_split$country),]
  
  # reform data
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
  for (j in 1991:2019){
    y = j-1990
    accdata2[which(accdata2$year==y),]$acc2 = accdata2[which(accdata2$year==y),]$hw+
      accdata2[which(accdata2$year==y-1),]$hw
  }
  accdata2 = accdata2[which(accdata2$year>0),]
  accdata2$year = accdata2$year-1
  
  accdata2_high = accdata2[which(accdata2$gnigroup=='high income'),]
  accdata2_low = accdata2[which(accdata2$gnigroup=='mid and low income'),]
  
  #accdata3
  accdata3$acc3 = accdata3$hw
  for (j in 1992:2019){
    y = j-1990
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
  for (j in 1993:2019){
    y = j-1990
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
  for (j in 1994:2019){
    y = j-1990
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
  for (j in 1995:2019){
    y = j-1990
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
  for (j in 1996:2019){
    y = j-1990
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
  for (j in 1997:2019){
    y = j-1990
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
  
  # data preprocessing
  
  # all countries
  accdata1$gdp = log(accdata1$gdp)
  accdata2$gdp = log(accdata2$gdp)
  accdata3$gdp = log(accdata3$gdp)
  accdata4$gdp = log(accdata4$gdp)
  accdata5$gdp = log(accdata5$gdp)
  accdata6$gdp = log(accdata6$gdp)
  accdata7$gdp = log(accdata7$gdp)
  accdata8$gdp = log(accdata8$gdp)
  
  accdata1[,c(1:3,7)] = scale(accdata1[,c(1:3,7)])
  accdata2[,c(1:3,7)] = scale(accdata2[,c(1:3,7)])
  accdata3[,c(1:3,7)] = scale(accdata3[,c(1:3,7)])
  accdata4[,c(1:3,7)] = scale(accdata4[,c(1:3,7)])
  accdata5[,c(1:3,7)] = scale(accdata5[,c(1:3,7)])
  accdata6[,c(1:3,7)] = scale(accdata6[,c(1:3,7)])
  accdata7[,c(1:3,7)] = scale(accdata7[,c(1:3,7)])
  accdata8[,c(1:3,7)] = scale(accdata8[,c(1:3,7)])
  
  # high income countries
  accdata1_high$gdp = log(accdata1_high$gdp)
  accdata2_high$gdp = log(accdata2_high$gdp)
  accdata3_high$gdp = log(accdata3_high$gdp)
  accdata4_high$gdp = log(accdata4_high$gdp)
  accdata5_high$gdp = log(accdata5_high$gdp)
  accdata6_high$gdp = log(accdata6_high$gdp)
  accdata7_high$gdp = log(accdata7_high$gdp)
  accdata8_high$gdp = log(accdata8_high$gdp)
  
  accdata1_high[,c(1:3,7)] = scale(accdata1_high[,c(1:3,7)])
  accdata2_high[,c(1:3,7)] = scale(accdata2_high[,c(1:3,7)])
  accdata3_high[,c(1:3,7)] = scale(accdata3_high[,c(1:3,7)])
  accdata4_high[,c(1:3,7)] = scale(accdata4_high[,c(1:3,7)])
  accdata5_high[,c(1:3,7)] = scale(accdata5_high[,c(1:3,7)])
  accdata6_high[,c(1:3,7)] = scale(accdata6_high[,c(1:3,7)])
  accdata7_high[,c(1:3,7)] = scale(accdata7_high[,c(1:3,7)])
  accdata8_high[,c(1:3,7)] = scale(accdata8_high[,c(1:3,7)])
  
  # low- to middle income countries
  accdata1_low$gdp = log(accdata1_low$gdp)
  accdata2_low$gdp = log(accdata2_low$gdp)
  accdata3_low$gdp = log(accdata3_low$gdp)
  accdata4_low$gdp = log(accdata4_low$gdp)
  accdata5_low$gdp = log(accdata5_low$gdp)
  accdata6_low$gdp = log(accdata6_low$gdp)
  accdata7_low$gdp = log(accdata7_low$gdp)
  accdata8_low$gdp = log(accdata8_low$gdp)
  
  accdata1_low[,c(1:3,7)] = scale(accdata1_low[,c(1:3,7)])
  accdata2_low[,c(1:3,7)] = scale(accdata2_low[,c(1:3,7)])
  accdata3_low[,c(1:3,7)] = scale(accdata3_low[,c(1:3,7)])
  accdata4_low[,c(1:3,7)] = scale(accdata4_low[,c(1:3,7)])
  accdata5_low[,c(1:3,7)] = scale(accdata5_low[,c(1:3,7)])
  accdata6_low[,c(1:3,7)] = scale(accdata6_low[,c(1:3,7)])
  accdata7_low[,c(1:3,7)] = scale(accdata7_low[,c(1:3,7)])
  accdata8_low[,c(1:3,7)] = scale(accdata8_low[,c(1:3,7)])
  
  # mixed effect model
  
  data_plot = matrix(0,nrow=8,ncol=4,dimnames=list(1:8,c('ACC','All 137 countries','High-income','Low- to middle-income')))
  data_plot[,1] = c(1:8)
  for (j in 1:8){
    fomula = paste0('inci_as~acc',j,'*year*gdp+(1+year|country)')
    
    data_all = get(paste0('accdata',j))
    model_all = lmer(fomula,data=data_all,control = lmerControl(optimizer ="Nelder_Mead"))
    fit_all = summary(model_all)
    df = fit_all$coefficients[5,3]
    t = fit_all$coefficients[5,4]
    data_plot[j,2] = t*t/(t*t+df)
    
    data_high = get(paste0('accdata',j,'_high'))
    model_high = lmer(fomula,data=data_high,control = lmerControl(optimizer ="Nelder_Mead"))
    fit_high = summary(model_high)
    df = fit_high$coefficients[5,3]
    t = fit_high$coefficients[5,4]
    data_plot[j,3] = t*t/(t*t+df)
    
    data_low = get(paste0('accdata',j,'_low'))
    model_low = lmer(fomula,data=data_low,control = lmerControl(optimizer ="Nelder_Mead"))
    fit_low = summary(model_low)
    df = fit_low$coefficients[5,3]
    t = fit_low$coefficients[5,4]
    data_plot[j,4] = t*t/(t*t+df)
  }
  
  for (j in 1:8){
    all_result[i,j] = data_plot[3,2]-data_plot[j,2]
    high_result[i,j] = data_plot[7,3]-data_plot[j,3]
    low_result[i,j] = data_plot[3,4]-data_plot[j,4]
  }
  
}

# plot the deviation for each acc

write.csv(all_result,'deviation of eta for acc in all 137 countries dif year.csv')
write.csv(high_result,'deviation of eta for acc in high-income countries dif year.csv')
write.csv(low_result,'deviation of eta for acc in low- to middle-income countries dif year.csv')

all_result1 = t(all_result)
all_result1 = as.data.frame(all_result1)
all_result1$ID = c(1:8)
all_result_long = melt(all_result1,id.vars='ID',variable.name="permutation",value.name="deviation")
all_result_long$ID = as.factor(all_result_long$ID)
all_deviation_same = 
  ggplot(all_result_long, aes(x=ID, y=value,color=ID)) + 
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()+
  xlab('ACC')+
  ylab('Deviation')
ggsave(all_deviation_same,filename = 'deviation of eta for acc in all 137 countries dif year.pdf',
       width=4,height=4)

high_result1 = t(high_result)
high_result1 = as.data.frame(high_result1)
high_result1$ID = c(1:8)
high_result_long = melt(high_result1,id.vars='ID',variable.name="permutation",value.name="deviation")
high_result_long$ID = as.factor(high_result_long$ID)
high_deviation_same = 
  ggplot(high_result_long, aes(x=ID, y=value,color=ID)) + 
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()+
  xlab('ACC')+
  ylab('Deviation')
ggsave(high_deviation_same,filename = 'deviation of eta for acc in high-income countries dif year.pdf',
       width=4,height=4)

low_result1 = t(low_result)
low_result1 = as.data.frame(low_result1)
low_result1$ID = c(1:8)
low_result_long = melt(low_result1,id.vars='ID',variable.name="permutation",value.name="deviation")
low_result_long$ID = as.factor(low_result_long$ID)
low_deviation_same = 
  ggplot(low_result_long, aes(x=ID, y=value,color=ID)) + 
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()+
  xlab('ACC')+
  ylab('Deviation')
ggsave(low_deviation_same,filename = 'deviation of eta for acc in low- to middle-income countries dif year.pdf',
       width=4,height=4)


