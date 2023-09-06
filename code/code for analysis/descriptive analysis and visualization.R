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
library(ggrepel)
library(ppcor)
library(ggpubr)

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

# 2. form incrementdata---------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup')
incrementdata = data[,colnames(data) %in% cols]
incrementdata$inci_as_delta = incrementdata$inci_as
incrementdata$lag1 = incrementdata$hw
for (i in 1991:2019){
  y = i-1990
  incrementdata[which(incrementdata$year==y),]$inci_as_delta = incrementdata[which(incrementdata$year==y),]$inci_as_delta-
    incrementdata[which(incrementdata$year==y-1),]$inci_as
}
incrementdata = incrementdata[which(incrementdata$year>0),]
incrementdata$year = incrementdata$year-1


incrementdata_high = incrementdata[which(incrementdata$gnigroup=='high income'),]
incrementdata_low = incrementdata[which(incrementdata$gnigroup=='mid and low income'),]


################################################################################
# descriptive analysis
################################################################################

# 1. summary the full data------------------------------------------------------

desc_result = matrix(0,nrow=23,ncol=8,
                     dimnames=list(colnames(data)[1:23],c("mean","std","max","min","max_country","max_year","min_country","min_year")) )
for (i in 1:23){
  desc_result[i,1] = mean(data[,i])
  desc_result[i,2] = sd(data[,i])
  desc_result[i,3] = max(data[,i])
  desc_result[i,4] = min(data[,i])
  if (dim(data[which(data[,i]==max(data[,i])),])[1]>1){
    desc_result[i,5] = 'multiple countries'
    desc_result[i,6] = 'multiple years'
  }
  else{
    desc_result[i,5] = data[which(data[,i]==max(data[,i])),]$country
    desc_result[i,6] = data[which(data[,i]==max(data[,i])),]$year+1990
  }
  if (dim(data[which(data[,i]==min(data[,i])),])[1]>1){
    desc_result[i,7] = 'multiple countries'
    desc_result[i,8] = 'multiple years'
  }
  else{
    desc_result[i,7] = data[which(data[,i]==min(data[,i])),]$country
    desc_result[i,8] = data[which(data[,i]==min(data[,i])),]$year+1990
  }
}
write.csv(desc_result,'descriptive analysis result.csv')
stat.desc(data$sex_ratio)
stat.desc(data$age_ratio)

# 2. summary data for each country----------------------------------------------
country = unique(data$country)
result = matrix(0,nrow=137,ncol=19,
                dimnames=list(country,c('gnigroup',rep(c("lower","mean","upper","min","max"),3),'inci_as','hw','gdp')))

for (i in 1:137){
  
  data1 = data[which(data$country==country[i]),]
  result[i,1] = data1$gnigroup[1]
  
  result[i,2:4] = round(CI(data1$inci_as,ci = 0.95),2)
  result[i,5] = min(data1$inci_as)
  result[i,6] = max(data1$inci_as)
  
  result[i,7:9] = round(CI(data1$hw,ci = 0.95),2)
  result[i,10] = min(data1$hw)
  result[i,11] = max(data1$hw)
  
  result[i,12:14] = round(CI(data1$gdp,ci = 0.95),0)
  result[i,15] = min(data1$gdp)
  result[i,16] = max(data1$gdp)
  
  result[i,17] = paste(result[i,3],' (',result[i,4],' to ',result[i,2],')',sep="")
  result[i,18] = paste(result[i,8],' (',result[i,9],' to ',result[i,7],')',sep="")
  result[i,19] = paste(result[i,13],' (',result[i,14],' to ',result[i,12],')',sep="")
}

write.csv(result,'descriptive analysis of data for 137 countries incidence.csv')

# all countries
round(CI(data$inci_as,ci = 0.95),2)
round(CI(data$hw,ci = 0.95),2)
round(CI(data$gdp,ci = 0.95),0)

# high-income countries
round(CI(data[which(data$gnigroup=='high income'),]$inci_as,ci = 0.95),2)
round(CI(data[which(data$gnigroup=='high income'),]$hw,ci = 0.95),2)
round(CI(data[which(data$gnigroup=='high income'),]$gdp,ci = 0.95),0)

# middle- and low-income countries
round(CI(data[which(data$gnigroup=='mid and low income'),]$inci_as,ci = 0.95),2)
round(CI(data[which(data$gnigroup=='mid and low income'),]$hw,ci = 0.95),2)
round(CI(data[which(data$gnigroup=='mid and low income'),]$gdp,ci = 0.95),0)

# 3. analysis the trend of HW---------------------------------------------------

model = lmer(hw~year+(1+year|country),data=data)
summary(model)
confint(model,level=0.95)

model = lmer(hw~year+(1+year|country),data=data_low)
summary(model)
confint(model,level=0.95)

model = lmer(hw~year+(1+year|country),data=data_high)
summary(model)
confint(model,level=0.95)

# 4. analysis the inci_as_delta-------------------------------------------------

round(CI(accdata$inci_as_delta,ci = 0.95),4)

# 5. analysis the trend of inci-------------------------------------------------

model = lmer(inci_as~year+(1+year|country),data=data)
summary(model)
confint(model,level=0.95)

model = lmer(inci_as~year+(1+year|country),data=data_low)
summary(model)
confint(model,level=0.95)

model = lmer(inci_as~year+(1+year|country),data=data_high)
summary(model)
confint(model,level=0.95)

################################################################################
# visualization
################################################################################

# 1. trend of incidence---------------------------------------------------------

p = ggplot()
country = unique(data$country)
for (i in 1:137){
  data1 = data[which(data$country==country[i]),]
  p = p+geom_line(aes(x = year+1990, y = inci_as),data1,color='grey',alpha=0.8)
}

#  high income--------------------
beta = -0.002253
beta_upper = 0.03111131
beta_lower = -0.07616769

mean_year = 0:29
mean_incidence = mean_year*beta+mean(data_high$inci_as[which(data_high$year==0)])
upper_incidence = mean_year*beta_upper+CI(data_high$inci_as[which(data_high$year==0)])[1]
lower_incidence = mean_year*beta_lower+CI(data_high$inci_as[which(data_high$year==0)])[3]
mean_line1 = cbind(mean_year,mean_incidence,upper_incidence,lower_incidence)
mean_line1 = as.data.frame(mean_line1)

p = p+
  geom_line(aes(x = mean_year+1990, y = mean_incidence,color="High-income"),
            data= mean_line1,linewidth=1)+
  geom_ribbon(data=mean_line1,aes(x = mean_year+1990,y = mean_incidence,
                                  ymin = lower_incidence, ymax = upper_incidence),alpha = 0.2)+
  xlab("Year")+
  ylab("Age-standardized incidence of dementia")+
  scale_colour_manual(name="Countries",values = 
                        c('High-income'='#01468B','Low- to middle-income'='#EC0000'))

# mid and low income-------------
beta = 0.022134
beta_upper = 0.0008099656
beta_lower = 0.0361742

mean_year = 0:29
mean_incidence = mean_year*beta+mean(data_low$inci_as[which(data_low$year==0)])
upper_incidence = mean_year*beta_upper+CI(data_low$inci_as[which(data_low$year==0)])[1]
lower_incidence = mean_year*beta_lower+CI(data_low$inci_as[which(data_low$year==0)])[3]
mean_line2 = cbind(mean_year,mean_incidence,upper_incidence,lower_incidence)
mean_line2 = as.data.frame(mean_line2)

p = p+
  geom_line(aes(x = mean_year+1990, y = mean_incidence,color="Low- to middle-income"),
            linewidth=1,data=mean_line2)+
  geom_ribbon(aes(x = mean_year+1990, y = mean_incidence,
                  ymin = lower_incidence, ymax = upper_incidence),alpha = 0.2,data=mean_line2)+
  theme_minimal()+
  theme(text = element_text(size = 12,family="sans"),
        legend.position = c(.7,.1),
        legend.key = element_rect(colour ="transparent",fill="transparent"),
        axis.text.x= element_text(angle=60, hjust=1))

p0= p
ggsave(p,filename = 'trend of incidence.pdf',width=4,height=4)

# 2. trend of hw----------------------------------------------------------------

p = ggplot()
country = unique(data$country)
for (i in 1:137){
  data1 = data[which(data$country==country[i]),]
  p = p+geom_line(aes(x = year+1990, y = hw),data1,color='grey',alpha=0.8)
}

# high income
mean_year = 0:29
mean_hw = mean_year*2.4841+178.0372
upper_hw = mean_year*2.8457522+182.603166
lower_hw = mean_year*2.1224844+173.471314
mean_line2 = cbind(mean_year,mean_hw,upper_hw,lower_hw)
mean_line2 = as.data.frame(mean_line2)

p = p+
  geom_line(aes(x = mean_year+1990, y = mean_hw,color="High-income"),
            linewidth=1,data=mean_line2)+
  geom_ribbon(aes(x = mean_year+1990, y = mean_hw,
                  ymin = lower_hw, ymax = upper_hw),alpha = 0.2,data=mean_line2)

# middle and low income
mean_year = 0:29
mean_hw = mean_year*3.5178+167.4
upper_hw = mean_year*3.8990709+170.642530
lower_hw = mean_year*3.136503+164.075013
mean_line2 = cbind(mean_year,mean_hw,upper_hw,lower_hw)
mean_line2 = as.data.frame(mean_line2)

p = p+
  geom_line(aes(x = mean_year+1990, y = mean_hw,color="Low- to middle-income"),
            linewidth=1,data=mean_line2)+
  geom_ribbon(aes(x = mean_year+1990, y = mean_hw,
                  ymin = lower_hw, ymax = upper_hw),alpha = 0.2,data=mean_line2)+
  theme_minimal()+
  theme(text = element_text(size = 12,family="sans"),
        legend.position = c(.7,.1),
        legend.key = element_rect(colour ="transparent",fill="transparent"),
        axis.text.x= element_text(angle=60, hjust=1))+
  xlab('Year')+
  ylab('Heatwave')+
  scale_colour_manual(name="Countries",values = 
                        c('High-income'='#01468B','Low- to middle-income'='#EC0000'))

ggsave(p,filename = 'trend of hw.pdf',width=4,height=4)

p_arrange = ggarrange(p0, p, common.legend = TRUE, legend="top",
                      labels = c('C','D'),
                      label.x = .1, label.y = 1,
                      font.label = list(size = 12, face = "bold"))
ggsave(p_arrange,filename = 'trend.pdf',width=8,height=4)

# 4. relationship between change of incidence and acc2--------------------------

# *4.1 scatter plot for all countries-------------------------------------------
accdata_plot = accdata
accdata_plot$gnigroup[which(accdata_plot$gnigroup=='high income')] = 'High-income'
accdata_plot$gnigroup[which(accdata_plot$gnigroup=='mid and low income')] = 'Low- to middle-income'
yrng <- range(accdata_plot$inci_as_delta)
xrng <- range(accdata_plot$acc3)
caption1 <- "0.00216 (0.00121 to 0.00134), p<0.0001"
caption2 <- "0.00181 (0.00154 to 0.00208), p<0.0001"

p =ggplot(accdata_plot,aes(x=acc3,y=inci_as_delta,color=gnigroup))+
  geom_point()+
  geom_smooth(method='lm')+
  xlab('ACC3')+
  ylab('Change of incidence')+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),legend.position = c(.3,.15))+
  scale_colour_manual(name="Countries",values = 
                        c('High-income'='#01468B','Low- to middle-income'='#EC0000'))+
  annotate(
    geom = "text", x = xrng[1], y = yrng[2], 
    label = caption1, hjust = 0, vjust = 1, size = 3,color='#01468B'
  )+
  annotate(
    geom = "text", x = xrng[1], y = yrng[2]-0.3, 
    label = caption2, hjust = 0, vjust = 1, size = 3,color='#EC0000'
  )
p1 = ggMarginal(p,
                type = "histogram",
                groupColour = TRUE,
                groupFill = TRUE,
                size = 3)
ggsave(p1,filename = 'relationship between inci_as_delta and acc3 grouped by gni.pdf',width=4,height=4)


