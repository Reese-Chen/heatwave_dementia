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
library(lsr)

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
# form accdata and incrementdata
################################################################################

# 1. form accdata1--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata1 = data[,colnames(data) %in% cols]
accdata1$inci_as_delta = accdata1$inci_as
accdata1$acc1 = accdata1$hw
for (i in 1991:2019){
  y = i-1990
  accdata1[which(accdata1$year==y),]$inci_as_delta = accdata1[which(accdata1$year==y),]$inci_as_delta-
    accdata1[which(accdata1$year==y-1),]$inci_as
  accdata1[which(accdata1$year==y),]$acc1 = accdata1[which(accdata1$year==y),]$hw+
    accdata1[which(accdata1$year==y-1),]$hw
}
accdata1 = accdata1[which(accdata1$year>0),]
accdata1$year = accdata1$year-1

accdata1_high = accdata1[which(accdata1$gnigroup=='high income'),]
accdata1_low = accdata1[which(accdata1$gnigroup=='mid and low income'),]

# 2. form accdata2--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata2 = data[,colnames(data) %in% cols]
accdata2$inci_as_delta = accdata2$inci_as
accdata2$acc2 = accdata2$hw
for (i in 1992:2019){
  y = i-1990
  accdata2[which(accdata2$year==y),]$inci_as_delta = accdata2[which(accdata2$year==y),]$inci_as_delta-
    accdata2[which(accdata2$year==y-1),]$inci_as
  accdata2[which(accdata2$year==y),]$acc2 = accdata2[which(accdata2$year==y),]$hw+
    accdata2[which(accdata2$year==y-1),]$hw+accdata2[which(accdata2$year==y-2),]$hw
}
accdata2 = accdata2[which(accdata2$year>1),]
accdata2$year = accdata2$year-2

accdata2_high = accdata2[which(accdata2$gnigroup=='high income'),]
accdata2_low = accdata2[which(accdata2$gnigroup=='mid and low income'),]

# 3. form accdata3--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata3 = data[,colnames(data) %in% cols]
accdata3$inci_as_delta = accdata3$inci_as
accdata3$acc3 = accdata3$hw
for (i in 1993:2019){
  y = i-1990
  accdata3[which(accdata3$year==y),]$inci_as_delta = accdata3[which(accdata3$year==y),]$inci_as_delta-
    accdata3[which(accdata3$year==y-1),]$inci_as
  accdata3[which(accdata3$year==y),]$acc3 = accdata3[which(accdata3$year==y),]$hw+
    accdata3[which(accdata3$year==y-1),]$hw+accdata3[which(accdata3$year==y-2),]$hw+
    accdata3[which(accdata3$year==y-3),]$hw
}
accdata3 = accdata3[which(accdata3$year>2),]
accdata3$year = accdata3$year-3

accdata3_high = accdata3[which(accdata3$gnigroup=='high income'),]
accdata3_low = accdata3[which(accdata3$gnigroup=='mid and low income'),]

# 4. form accdata4--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata4 = data[,colnames(data) %in% cols]
accdata4$inci_as_delta = accdata4$inci_as
accdata4$acc4 = accdata4$hw
for (i in 1994:2019){
  y = i-1990
  accdata4[which(accdata4$year==y),]$inci_as_delta = accdata4[which(accdata4$year==y),]$inci_as_delta-
    accdata4[which(accdata4$year==y-1),]$inci_as
  accdata4[which(accdata4$year==y),]$acc4 = accdata4[which(accdata4$year==y),]$hw+
    accdata4[which(accdata4$year==y-1),]$hw+accdata4[which(accdata4$year==y-2),]$hw+
    accdata4[which(accdata4$year==y-3),]$hw+accdata4[which(accdata4$year==y-4),]$hw
}
accdata4 = accdata4[which(accdata4$year>3),]
accdata4$year = accdata4$year-4

accdata4_high = accdata4[which(accdata4$gnigroup=='high income'),]
accdata4_low = accdata4[which(accdata4$gnigroup=='mid and low income'),]


# 5. form accdata5--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata5 = data[,colnames(data) %in% cols]
accdata5$inci_as_delta = accdata5$inci_as
accdata5$acc5 = accdata5$hw
for (i in 1995:2019){
  y = i-1990
  accdata5[which(accdata5$year==y),]$inci_as_delta = accdata5[which(accdata5$year==y),]$inci_as_delta-
    accdata5[which(accdata5$year==y-1),]$inci_as
  accdata5[which(accdata5$year==y),]$acc5 = accdata5[which(accdata5$year==y),]$hw+
    accdata5[which(accdata5$year==y-1),]$hw+accdata5[which(accdata5$year==y-2),]$hw+
    accdata5[which(accdata5$year==y-3),]$hw+accdata5[which(accdata5$year==y-4),]$hw+
    accdata5[which(accdata5$year==y-5),]$hw
}
accdata5 = accdata5[which(accdata5$year>4),]
accdata5$year = accdata5$year-5

accdata5_high = accdata5[which(accdata5$gnigroup=='high income'),]
accdata5_low = accdata5[which(accdata5$gnigroup=='mid and low income'),]


# 6. form accdata6--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata6 = data[,colnames(data) %in% cols]
accdata6$inci_as_delta = accdata6$inci_as
accdata6$acc6 = accdata6$hw
for (i in 1996:2019){
  y = i-1990
  accdata6[which(accdata6$year==y),]$inci_as_delta = accdata6[which(accdata6$year==y),]$inci_as_delta-
    accdata6[which(accdata6$year==y-1),]$inci_as
  accdata6[which(accdata6$year==y),]$acc6 = accdata6[which(accdata6$year==y),]$hw+
    accdata6[which(accdata6$year==y-1),]$hw+accdata6[which(accdata6$year==y-2),]$hw+
    accdata6[which(accdata6$year==y-3),]$hw+accdata6[which(accdata6$year==y-4),]$hw+
    accdata6[which(accdata6$year==y-5),]$hw+accdata6[which(accdata6$year==y-6),]$hw
}
accdata6 = accdata6[which(accdata6$year>5),]
accdata6$year = accdata6$year-6

accdata6_high = accdata6[which(accdata6$gnigroup=='high income'),]
accdata6_low = accdata6[which(accdata6$gnigroup=='mid and low income'),]

# 7. form accdata7--------------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata7 = data[,colnames(data) %in% cols]
accdata7$inci_as_delta = accdata7$inci_as
accdata7$acc7 = accdata7$hw
for (i in 1997:2019){
  y = i-1990
  accdata7[which(accdata7$year==y),]$inci_as_delta = accdata7[which(accdata7$year==y),]$inci_as_delta-
    accdata7[which(accdata7$year==y-1),]$inci_as
  accdata7[which(accdata7$year==y),]$acc7 = accdata7[which(accdata7$year==y),]$hw+
    accdata7[which(accdata7$year==y-1),]$hw+accdata7[which(accdata7$year==y-2),]$hw+
    accdata7[which(accdata7$year==y-3),]$hw+accdata7[which(accdata7$year==y-4),]$hw+
    accdata7[which(accdata7$year==y-5),]$hw+accdata7[which(accdata7$year==y-6),]$hw+
    accdata7[which(accdata7$year==y-7),]$hw
}
accdata7 = accdata7[which(accdata7$year>6),]
accdata7$year = accdata7$year-7

accdata7_high = accdata7[which(accdata7$gnigroup=='high income'),]
accdata7_low = accdata7[which(accdata7$gnigroup=='mid and low income'),]

################################################################################
# mixed effect models with accdata1
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
accdata1$gdp = log(accdata1$gdp)

accdata1$inci_as = scale(accdata1$inci_as)
accdata1$inci_as_delta = scale(accdata1$inci_as_delta)
accdata1$hw = scale(accdata1$hw)
accdata1$gdp = scale(accdata1$gdp)
accdata1$acc1 = scale(accdata1$acc1)

# *1.2 high income--------------------------------------------------------------
accdata1_high$gdp = log(accdata1_high$gdp)

accdata1_high$inci_as = scale(accdata1_high$inci_as)
accdata1_high$inci_as_delta = scale(accdata1_high$inci_as_delta)
accdata1_high$hw = scale(accdata1_high$hw)
accdata1_high$gdp = scale(accdata1_high$gdp)
accdata1_high$acc1 = scale(accdata1_high$acc1)

# *1.3 mid and low income-------------------------------------------------------
accdata1_low$gdp = log(accdata1_low$gdp)

accdata1_low$inci_as = scale(accdata1_low$inci_as)
accdata1_low$inci_as_delta = scale(accdata1_low$inci_as_delta)
accdata1_low$hw = scale(accdata1_low$hw)
accdata1_low$gdp = scale(accdata1_low$gdp)
accdata1_low$acc1 = scale(accdata1_low$acc1)

# 2.  mixed effect model--------------------------------------------------------

model = lmer(inci_as~year*acc1*gdp+(1+year|country),
             data = accdata1,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc1*gdp+(1+year|country),
             data = accdata1_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc1*gdp+(1+year|country),
             data = accdata1_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

################################################################################
# mixed effect models with accdata2
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
accdata2$gdp = log(accdata2$gdp)

accdata2$inci_as = scale(accdata2$inci_as)
accdata2$inci_as_delta = scale(accdata2$inci_as_delta)
accdata2$hw = scale(accdata2$hw)
accdata2$gdp = scale(accdata2$gdp)
accdata2$acc2 = scale(accdata2$acc2)

# *1.2 high income--------------------------------------------------------------
accdata2_high$gdp = log(accdata2_high$gdp)

accdata2_high$inci_as = scale(accdata2_high$inci_as)
accdata2_high$inci_as_delta = scale(accdata2_high$inci_as_delta)
accdata2_high$hw = scale(accdata2_high$hw)
accdata2_high$gdp = scale(accdata2_high$gdp)
accdata2_high$acc2 = scale(accdata2_high$acc2)

# *1.3 mid and low income-------------------------------------------------------

accdata2_low$gdp = log(accdata2_low$gdp)

accdata2_low$inci_as = scale(accdata2_low$inci_as)
accdata2_low$inci_as_delta = scale(accdata2_low$inci_as_delta)
accdata2_low$hw = scale(accdata2_low$hw)
accdata2_low$gdp = scale(accdata2_low$gdp)
accdata2_low$acc2 = scale(accdata2_low$acc2)

# 2.  mixed effect model--------------------------------------------------------

model = lmer(inci_as~year*acc2*gdp+(1+year|country),
             data = accdata2,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc2*gdp+(1+year|country),
             data = accdata2_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc2*gdp+(1+year|country),
             data = accdata2_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

################################################################################
# mixed effect models with accdata3
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
accdata3$gdp = log(accdata3$gdp)
accdata3$inci_as = scale(accdata3$inci_as)
accdata3$inci_as_delta = scale(accdata3$inci_as_delta)
accdata3$hw = scale(accdata3$hw)
accdata3$gdp = scale(accdata3$gdp)
accdata3$acc3 = scale(accdata3$acc3)

# *1.2 high income--------------------------------------------------------------
accdata3_high$gdp = log(accdata3_high$gdp)
accdata3_high$inci_as = scale(accdata3_high$inci_as)
accdata3_high$inci_as_delta = scale(accdata3_high$inci_as_delta)
accdata3_high$hw = scale(accdata3_high$hw)
accdata3_high$gdp = scale(accdata3_high$gdp)
accdata3_high$acc3 = scale(accdata3_high$acc3)

# *1.3 mid and low income-------------------------------------------------------

accdata3_low$gdp = log(accdata3_low$gdp)
accdata3_low$inci_as = scale(accdata3_low$inci_as)
accdata3_low$inci_as_delta = scale(accdata3_low$inci_as_delta)
accdata3_low$hw = scale(accdata3_low$hw)
accdata3_low$gdp = scale(accdata3_low$gdp)
accdata3_low$acc3 = scale(accdata3_low$acc3)

# 2.  mixed effect model--------------------------------------------------------

model = lmer(inci_as~year*acc3*gdp+(1+year|country),
             data = accdata3,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc3*gdp+(1+year|country),
             data = accdata3_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc3*gdp+(1+year|country),
             data = accdata3_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)


################################################################################
# mixed effect models with accdata4
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
accdata4$gdp = log(accdata4$gdp)
accdata4$inci_as = scale(accdata4$inci_as)
accdata4$inci_as_delta = scale(accdata4$inci_as_delta)
accdata4$hw = scale(accdata4$hw)
accdata4$gdp = scale(accdata4$gdp)
accdata4$acc4 = scale(accdata4$acc4)

# *1.2 high income--------------------------------------------------------------
accdata4_high$gdp = log(accdata4_high$gdp)
accdata4_high$inci_as = scale(accdata4_high$inci_as)
accdata4_high$inci_as_delta = scale(accdata4_high$inci_as_delta)
accdata4_high$hw = scale(accdata4_high$hw)
accdata4_high$gdp = scale(accdata4_high$gdp)
accdata4_high$acc4 = scale(accdata4_high$acc4)

# *1.3 mid and low income-------------------------------------------------------

accdata4_low$gdp = log(accdata4_low$gdp)
accdata4_low$inci_as = scale(accdata4_low$inci_as)
accdata4_low$inci_as_delta = scale(accdata4_low$inci_as_delta)
accdata4_low$hw = scale(accdata4_low$hw)
accdata4_low$gdp = scale(accdata4_low$gdp)
accdata4_low$acc4 = scale(accdata4_low$acc4)

# 2.  mixed effect model--------------------------------------------------------

model = lmer(inci_as~year*acc4*gdp+(1+year|country),
             data = accdata4,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc4*gdp+(1+year|country),
             data = accdata4_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc4*gdp+(1+year|country),
             data = accdata4_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

################################################################################
# mixed effect models with accdata5
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
accdata5$gdp = log(accdata5$gdp)
accdata5$inci_as = scale(accdata5$inci_as)
accdata5$inci_as_delta = scale(accdata5$inci_as_delta)
accdata5$hw = scale(accdata5$hw)
accdata5$gdp = scale(accdata5$gdp)
accdata5$acc5 = scale(accdata5$acc5)

# *1.2 high income--------------------------------------------------------------
accdata5_high$gdp = log(accdata5_high$gdp)
accdata5_high$inci_as = scale(accdata5_high$inci_as)
accdata5_high$inci_as_delta = scale(accdata5_high$inci_as_delta)
accdata5_high$hw = scale(accdata5_high$hw)
accdata5_high$gdp = scale(accdata5_high$gdp)
accdata5_high$acc5 = scale(accdata5_high$acc5)

# *1.3 mid and low income-------------------------------------------------------

accdata5_low$gdp = log(accdata5_low$gdp)
accdata5_low$inci_as = scale(accdata5_low$inci_as)
accdata5_low$inci_as_delta = scale(accdata5_low$inci_as_delta)
accdata5_low$hw = scale(accdata5_low$hw)
accdata5_low$gdp = scale(accdata5_low$gdp)
accdata5_low$acc5 = scale(accdata5_low$acc5)

# 2.  mixed effect model--------------------------------------------------------

model = lmer(inci_as~year*acc5*gdp+(1+year|country),
             data = accdata5,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc5*gdp+(1+year|country),
             data = accdata5_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc5*gdp+(1+year|country),
             data = accdata5_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

################################################################################
# mixed effect models with accdata6
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
accdata6$gdp = log(accdata6$gdp)
accdata6$inci_as = scale(accdata6$inci_as)
accdata6$inci_as_delta = scale(accdata6$inci_as_delta)
accdata6$hw = scale(accdata6$hw)
accdata6$gdp = scale(accdata6$gdp)
accdata6$acc6 = scale(accdata6$acc6)

# *1.2 high income--------------------------------------------------------------
accdata6_high$gdp = log(accdata6_high$gdp)
accdata6_high$inci_as = scale(accdata6_high$inci_as)
accdata6_high$inci_as_delta = scale(accdata6_high$inci_as_delta)
accdata6_high$hw = scale(accdata6_high$hw)
accdata6_high$gdp = scale(accdata6_high$gdp)
accdata6_high$acc6 = scale(accdata6_high$acc6)

# *1.3 mid and low income-------------------------------------------------------

accdata6_low$gdp = log(accdata6_low$gdp)
accdata6_low$inci_as = scale(accdata6_low$inci_as)
accdata6_low$inci_as_delta = scale(accdata6_low$inci_as_delta)
accdata6_low$hw = scale(accdata6_low$hw)
accdata6_low$gdp = scale(accdata6_low$gdp)
accdata6_low$acc6 = scale(accdata6_low$acc6)

# 2.  mixed effect model--------------------------------------------------------

model = lmer(inci_as~year*acc6*gdp+(1+year|country),
             data = accdata6,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc6*gdp+(1+year|country),
             data = accdata6_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc6*gdp+(1+year|country),
             data = accdata6_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

################################################################################
# mixed effect models with accdata7
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
accdata7$gdp = log(accdata7$gdp)
accdata7$inci_as = scale(accdata7$inci_as)
accdata7$inci_as_delta = scale(accdata7$inci_as_delta)
accdata7$hw = scale(accdata7$hw)
accdata7$gdp = scale(accdata7$gdp)
accdata7$acc7 = scale(accdata7$acc7)

# *1.2 high income--------------------------------------------------------------
accdata7_high$gdp = log(accdata7_high$gdp)
accdata7_high$inci_as = scale(accdata7_high$inci_as)
accdata7_high$inci_as_delta = scale(accdata7_high$inci_as_delta)
accdata7_high$hw = scale(accdata7_high$hw)
accdata7_high$gdp = scale(accdata7_high$gdp)
accdata7_high$acc7 = scale(accdata7_high$acc7)

# *1.3 mid and low income-------------------------------------------------------

accdata7_low$gdp = log(accdata7_low$gdp)
accdata7_low$inci_as = scale(accdata7_low$inci_as)
accdata7_low$inci_as_delta = scale(accdata7_low$inci_as_delta)
accdata7_low$hw = scale(accdata7_low$hw)
accdata7_low$gdp = scale(accdata7_low$gdp)
accdata7_low$acc7 = scale(accdata7_low$acc7)

# 2.  mixed effect model--------------------------------------------------------

model = lmer(inci_as~year*acc7*gdp+(1+year|country),
             data = accdata7,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc7*gdp+(1+year|country),
             data = accdata7_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*acc7*gdp+(1+year|country),
             data = accdata7_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

################################################################################
# visualization and comparison
################################################################################

df = data.frame(c(1,2,3,4,5,6,7,8),
                c(0.0206,0.0367,0.0480,0.0445,0.0388,0.0360,0.0389,0.0410),
                c(0.0031,0.0122,0.0191,0.0224,0.0215,0.0249,0.0278,0.0232),
                c(0.0337,0.0525,0.0667,0.0596,0.0522,0.0479,0.0534,0.0641))
colnames(df) = c('acc','all','high','low')
p1 = ggplot(data=df)+
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

ggsave(p1,filename = 'Partial-eta-squared for different acc.pdf',
       width=4,height=4)
