# load packages
library(dplyr)
library(metafor)
library(lme4)
library(lmerTest)
library(corrplot)
library(caret)
library(ggplot2)
library(tidyr)
library(psych)
library(ggpubr)
library(outliers)
library(meta)
library(dmetar)
library(effectsize)
library(splines)

p = ggplot(data,aes(x=year+1990,y=cw,color=country))+
  geom_line()
ggsave(p,filename = 'trend of cw.pdf',width=20,height=4)


################################################################################
# load data
################################################################################
rm(list=ls())
setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]

data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]

country = unique(data$country)
highcountry = unique(data[which(data$gnigroup=='high income'),]$country)
lowcountry = unique(data[which(data$gnigroup=='mid and low income'),]$country)

################################################################################
# basic meta regression
################################################################################

# 1. calculate beta-----------------------------------------------------------

### regression in each country
result1 = matrix(0,nrow=length(country),ncol=30,
                 dimnames=list(country,c("beta","SE","p","beta_origin","SE_origin",
                                         "cw_mean","cw_sd","cw_slope",
                                         "gdp_mean","gdp_sd","gdp_slope",
                                         "temp_mean","temp_sd","temp_slope",
                                         "pop_mean","pop_sd","pop_slope",
                                         "mort_as_mean","mort_as_sd","mort_as_slope",
                                         "sex_ratio","sex_ratio_sd","sex_ratio_slope",
                                         "Estimate","Estimate_orgin","reformed_p","eta",
                                         "beta_hwgdp","SE_hwgdp","Estimate_hwgdp")))
t = 0:29

for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  sd_inci = sd(data1$inci_as)
  sd_hw = sd(data1$hw)
  
  count = 0
  for (j in c(16:19,22,24)){
    count = count+1
    if (!(is.na(data1[1,j]))){
      result1[i,(count*3+3)] = mean(data1[,j])
      result1[i,(count*3+4)] = sd(data1[,j])
      linear_fit = summary(lm(data1[,j] ~ t))
      result1[i,(count*3+5)] = linear_fit$coefficients[2,1]
    }
  }
  
  data1$hw = scale(data1$hw)
  data1$gdp = scale(data1$gdp)
  
  model = lm(inci_as~year+hw+gdp+hw:gdp,data=data1)
  fit = summary(model)
  eta = eta_squared(model, partial = TRUE,ci=0.95,alternative = "two.sided")
  result1[i,1:2] = fit$coefficients[3,1:2]
  result1[i,3] = fit$coefficients[3,4]
  result1[i,4:5] = fit$coefficients[3,1:2]*sd_hw
  result1[i,27] = eta$Eta2_partial[2]*sign(fit$coefficients[3,1])
  
  result1[i,28:29] = fit$coefficients[5,1:2]
  result1[i,30] = paste(signif(fit$coefficients[5,1],3),' (',signif(confint(model)[5,1],3),' to ',signif(confint(model)[5,2],3),')',sep="")
  
  beta = signif(fit$coefficients[3,1],3)
  p = signif(fit$coefficients[5,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[3,1],3)
  CI_high = signif(confint(model)[3,2],3)
  
  beta_origin = signif(fit$coefficients[5,1]*sd_inci/sd_hw,3) 
  CI_low_origin = signif(confint(model)[3,1]*sd_inci/sd_hw,3) 
  CI_high_origin = signif(confint(model)[3,2]*sd_inci/sd_hw,3) 
  
  result1[i,24] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result1[i,26] = p
  result1[i,25] = paste(beta_origin,' (',CI_low_origin,' to ',CI_high_origin,')',sep="")
  
}

write.csv(result1,'first stage regression result for hw.csv')

result1 = result1[,c(1:21,27:29)]
result1 = apply(result1, 2, as.numeric)

result1 = as.data.frame(result1)
result1$country = country
result_high = result1[which(result1$country %in% highcountry),]
result_low = result1[which(result1$country %in% lowcountry),]

# boxplot of the beta at first stage

plot_data = result1[,c(3,22)]
plot_data$gni_group = "high-income"
plot_data$gni_group[which(country %in% lowcountry)] = "low- to middle-income"
plot_data$gni_group = as.factor(plot_data$gni_group)
head(plot_data)


t.test(plot_data$eta[which(plot_data$gni_group=='high-income')],
       plot_data$eta[which(plot_data$gni_group=='low- to middle-income')])

p1 = ggplot(plot_data,aes(x=gni_group,y=eta,color=gni_group))+
  geom_boxplot(linewidth=1,fill='white')+
  geom_point(shape=20,size=2,
             position = position_jitterdodge(jitter.width = 0.1, seed = 202), show.legend = FALSE)+
  stat_boxplot(geom='errorbar',size=1,show.legend = FALSE)+
  stat_compare_means(label="p.signif",
                     method = "t.test",paired = F,show.legend = FALSE)+
  xlab('GNI group')+
  ylab("beta for hw at first stage regression")+
  scale_color_manual(values=c('#EC0000','#01468B'))+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),
        legend.position = "none",
        legend.key = element_rect(colour ="transparent",fill="transparent"))

ggsave(p1,filename='boxplot of comparision of beta at first stage regression.pdf',
       width=4,height=4)

# 2. two-stage meta in all 153 countries----------------------------------------

plot(result1$beta,result1$cw_mean)


second_stage_res = matrix(0,nrow=17,ncol=16,
                          dimnames = list(c("cw_mean","cw_sd","cw_slope",
                                            "gdp_mean","gdp_sd","gdp_slope",
                                            "temp_mean","temp_sd","temp_slope",
                                            "pop_mean","pop_sd","pop_slope",
                                            "mort_as_mean","mort_as_sd","mort_as_slope",
                                            "sex_ratio","none"),
                                          rep(c('beta','se','zval','pval','ci.lb','ci.ub','estimate','p_reformed'),2)))

for (i in 1:16){
  
  meta_analysis = rma(yi = beta, sei = SE, mods = ~result1[,i+5], data = result1)
  fit = summary(meta_analysis)
  second_stage_res[i,1] = fit$beta[2]
  second_stage_res[i,2] = fit$se[2]
  second_stage_res[i,3] = fit$zval[2]
  second_stage_res[i,4] = fit$pval[2]
  second_stage_res[i,5] = fit$ci.lb[2]
  second_stage_res[i,6] = fit$ci.ub[2]
  
  beta = signif(fit$beta[2],3)
  p = signif(fit$pval[2],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[2],3)
  CI_high = signif(fit$ci.ub[2],3)
  second_stage_res[i,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  second_stage_res[i,8] = p
  
  meta_analysis = rma(yi = beta_origin, sei = SE_origin, mods = ~result1[,i+5], data = result1)
  fit = summary(meta_analysis)
  second_stage_res[i,9] = fit$beta[2]
  second_stage_res[i,10] = fit$se[2]
  second_stage_res[i,11] = fit$zval[2]
  second_stage_res[i,12] = fit$pval[2]
  second_stage_res[i,13] = fit$ci.lb[2]
  second_stage_res[i,14] = fit$ci.ub[2]
  
  beta = signif(fit$beta[2],3)
  p = signif(fit$pval[2],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[2],3)
  CI_high = signif(fit$ci.ub[2],3)
  second_stage_res[i,15] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  second_stage_res[i,16] = p
  
}


# without coveriates

meta_analysis = rma(yi = beta, sei = SE, data = result1)
find.outliers(meta_analysis)
fit = summary(meta_analysis)
second_stage_res[17,1] = fit$beta[1]
second_stage_res[17,2] = fit$se[1]
second_stage_res[17,3] = fit$zval[1]
second_stage_res[17,4] = fit$pval[1]
second_stage_res[17,5] = fit$ci.lb[1]
second_stage_res[17,6] = fit$ci.ub[1]

beta = signif(fit$beta[1],3)
p = signif(fit$pval[1],2)
if (p<0.0001){
  p = "<0.0001"
}
CI_low = signif(fit$ci.lb[1],3)
CI_high = signif(fit$ci.ub[1],3)
second_stage_res[17,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
second_stage_res[17,8] = p

meta_analysis = rma(yi = beta_origin, sei = SE_origin, data = result1)
fit = summary(meta_analysis)
second_stage_res[17,9] = fit$beta[1]
second_stage_res[17,10] = fit$se[1]
second_stage_res[17,11] = fit$zval[1]
second_stage_res[17,12] = fit$pval[1]
second_stage_res[17,13] = fit$ci.lb[1]
second_stage_res[17,14] = fit$ci.ub[1]

beta = signif(fit$beta[1],3)
p = signif(fit$pval[1],2)
if (p<0.0001){
  p = "<0.0001"
}
CI_low = signif(fit$ci.lb[1],3)
CI_high = signif(fit$ci.ub[1],3)
second_stage_res[17,15] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
second_stage_res[17,16] = p

write.csv(second_stage_res,'second stage regression result for hw in all 153 countries.csv')

meta_analysis = rma(yi = beta_hwgdp, sei = SE_hwgdp, data = result1)
summary(meta_analysis)
find.outliers(meta_analysis)


# 3. two-stage meta in high-income countries------------------------------------

second_stage_res = matrix(0,nrow=17,ncol=16,
                          dimnames = list(c("cw_mean","cw_sd","cw_slope",
                                            "gdp_mean","gdp_sd","gdp_slope",
                                            "temp_mean","temp_sd","temp_slope",
                                            "pop_mean","pop_sd","pop_slope",
                                            "mort_as_mean","mort_as_sd","mort_as_slope",
                                            "sex_ratio","none"),
                                          rep(c('beta','se','zval','pval','ci.lb','ci.ub','estimate','p_reformed'),2)))

for (i in 1:16){
  
  meta_analysis = rma(yi = beta, sei = SE, mods = ~result_high[,i+5], data = result_high)
  fit = summary(meta_analysis)
  second_stage_res[i,1] = fit$beta[2]
  second_stage_res[i,2] = fit$se[2]
  second_stage_res[i,3] = fit$zval[2]
  second_stage_res[i,4] = fit$pval[2]
  second_stage_res[i,5] = fit$ci.lb[2]
  second_stage_res[i,6] = fit$ci.ub[2]
  
  beta = signif(fit$beta[2],3)
  p = signif(fit$pval[2],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[2],3)
  CI_high = signif(fit$ci.ub[2],3)
  second_stage_res[i,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  second_stage_res[i,8] = p
  
  meta_analysis = rma(yi = beta_origin, sei = SE_origin, mods = ~result_high[,i+5], data = result_high)
  fit = summary(meta_analysis)
  second_stage_res[i,9] = fit$beta[2]
  second_stage_res[i,10] = fit$se[2]
  second_stage_res[i,11] = fit$zval[2]
  second_stage_res[i,12] = fit$pval[2]
  second_stage_res[i,13] = fit$ci.lb[2]
  second_stage_res[i,14] = fit$ci.ub[2]
  
  beta = signif(fit$beta[2],3)
  p = signif(fit$pval[2],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[2],3)
  CI_high = signif(fit$ci.ub[2],3)
  second_stage_res[i,15] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  second_stage_res[i,16] = p
  
}


# without coveriates

meta_analysis = rma(yi = beta, sei = SE, data = result_high)
find.outliers(meta_analysis)
fit = summary(meta_analysis)
second_stage_res[17,1] = fit$beta[1]
second_stage_res[17,2] = fit$se[1]
second_stage_res[17,3] = fit$zval[1]
second_stage_res[17,4] = fit$pval[1]
second_stage_res[17,5] = fit$ci.lb[1]
second_stage_res[17,6] = fit$ci.ub[1]

beta = signif(fit$beta[1],3)
p = signif(fit$pval[1],2)
if (p<0.0001){
  p = "<0.0001"
}
CI_low = signif(fit$ci.lb[1],3)
CI_high = signif(fit$ci.ub[1],3)
second_stage_res[17,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
second_stage_res[17,8] = p

meta_analysis = rma(yi = beta_origin, sei = SE_origin, data = result_high)
fit = summary(meta_analysis)
second_stage_res[17,9] = fit$beta[1]
second_stage_res[17,10] = fit$se[1]
second_stage_res[17,11] = fit$zval[1]
second_stage_res[17,12] = fit$pval[1]
second_stage_res[17,13] = fit$ci.lb[1]
second_stage_res[17,14] = fit$ci.ub[1]

beta = signif(fit$beta[1],3)
p = signif(fit$pval[1],2)
if (p<0.0001){
  p = "<0.0001"
}
CI_low = signif(fit$ci.lb[1],3)
CI_high = signif(fit$ci.ub[1],3)
second_stage_res[17,15] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
second_stage_res[17,16] = p

write.csv(second_stage_res,'second stage regression result for hw in high-income countries.csv')

meta_analysis = rma(yi = beta_hwgdp, sei = SE_hwgdp, data = result_high)
summary(meta_analysis)
find.outliers(meta_analysis)


# 3. two-stage meta in low- to middle-income countries--------------------------

second_stage_res = matrix(0,nrow=17,ncol=16,
                          dimnames = list(c("cw_mean","cw_sd","cw_slope",
                                            "gdp_mean","gdp_sd","gdp_slope",
                                            "temp_mean","temp_sd","temp_slope",
                                            "pop_mean","pop_sd","pop_slope",
                                            "mort_as_mean","mort_as_sd","mort_as_slope",
                                            "sex_ratio","none"),
                                          rep(c('beta','se','zval','pval','ci.lb','ci.ub','estimate','p_reformed'),2)))

for (i in 1:16){
  
  meta_analysis = rma(yi = beta, sei = SE, mods = ~result_low[,i+5], data = result_low)
  fit = summary(meta_analysis)
  second_stage_res[i,1] = fit$beta[2]
  second_stage_res[i,2] = fit$se[2]
  second_stage_res[i,3] = fit$zval[2]
  second_stage_res[i,4] = fit$pval[2]
  second_stage_res[i,5] = fit$ci.lb[2]
  second_stage_res[i,6] = fit$ci.ub[2]
  
  beta = signif(fit$beta[2],3)
  p = signif(fit$pval[2],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[2],3)
  CI_high = signif(fit$ci.ub[2],3)
  second_stage_res[i,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  second_stage_res[i,8] = p
  
  meta_analysis = rma(yi = beta_origin, sei = SE_origin, mods = ~result_low[,i+5], data = result_low)
  fit = summary(meta_analysis)
  second_stage_res[i,9] = fit$beta[2]
  second_stage_res[i,10] = fit$se[2]
  second_stage_res[i,11] = fit$zval[2]
  second_stage_res[i,12] = fit$pval[2]
  second_stage_res[i,13] = fit$ci.lb[2]
  second_stage_res[i,14] = fit$ci.ub[2]
  
  beta = signif(fit$beta[2],3)
  p = signif(fit$pval[2],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[2],3)
  CI_high = signif(fit$ci.ub[2],3)
  second_stage_res[i,15] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  second_stage_res[i,16] = p
  
}


# without coveriates

meta_analysis = rma(yi = beta, sei = SE, data = result_low)
find.outliers(meta_analysis)
fit = summary(meta_analysis)
second_stage_res[17,1] = fit$beta[1]
second_stage_res[17,2] = fit$se[1]
second_stage_res[17,3] = fit$zval[1]
second_stage_res[17,4] = fit$pval[1]
second_stage_res[17,5] = fit$ci.lb[1]
second_stage_res[17,6] = fit$ci.ub[1]

beta = signif(fit$beta[1],3)
p = signif(fit$pval[1],2)
if (p<0.0001){
  p = "<0.0001"
}
CI_low = signif(fit$ci.lb[1],3)
CI_high = signif(fit$ci.ub[1],3)
second_stage_res[17,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
second_stage_res[17,8] = p

meta_analysis = rma(yi = beta_origin, sei = SE_origin, data = result_low)
fit = summary(meta_analysis)
second_stage_res[17,9] = fit$beta[1]
second_stage_res[17,10] = fit$se[1]
second_stage_res[17,11] = fit$zval[1]
second_stage_res[17,12] = fit$pval[1]
second_stage_res[17,13] = fit$ci.lb[1]
second_stage_res[17,14] = fit$ci.ub[1]

beta = signif(fit$beta[1],3)
p = signif(fit$pval[1],2)
if (p<0.0001){
  p = "<0.0001"
}
CI_low = signif(fit$ci.lb[1],3)
CI_high = signif(fit$ci.ub[1],3)
second_stage_res[17,15] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
second_stage_res[17,16] = p

write.csv(second_stage_res,'second stage regression result for hw in low- to middle-income countries.csv')

meta_analysis = rma(yi = beta_hwgdp, sei = SE_hwgdp, data = result_low)
summary(meta_analysis)
find.outliers(meta_analysis)

################################################################################
# sensitivity analysis
################################################################################

# 1. controlling for temp-------------------------------------------------------

# 1.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=8,
                 dimnames = list(country,c("hw_beta","hw_SE","hw_p","hw_estimate",
                              "temp_beta","temp_SE","temp_p","temp_estimate")))
for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1[,c(9,17,18)] = scale(data1[,c(9,17,18)])
  model = lm(inci_as~year+hw+gdp+temp+gdp:hw,data = data1)
  fit = summary(model)
  # hw
  result1[i,1:2] = fit$coefficients[3,1:2]
  beta = signif(fit$coefficients[3,1],3)
  p = signif(fit$coefficients[3,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[3,1],3)
  CI_high = signif(confint(model)[3,2],3)
  result1[i,3] = p
  result1[i,4] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  # temp
  result1[i,5:6] = fit$coefficients[5,1:2]
  beta = signif(fit$coefficients[5,1],3)
  p = signif(fit$coefficients[5,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[5,1],3)
  CI_high = signif(confint(model)[5,2],3)
  result1[i,7] = p
  result1[i,8] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  
}

write.csv(result1,'first stage regression controlling temp.csv')

result1 = result1[,c(1:2,5:6)]
result1 = apply(result1, 2, as.numeric)

# 1.2 second stage analysis-----------------------------------------------------

meta_analysis = rma(yi = hw_beta, sei = hw_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = temp_beta, sei = temp_SE, data = result1)
summary(meta_analysis)

# 2. controlling for other coveriates-------------------------------------------

# 2.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=12,
                 dimnames = list(country,c("hw_beta","hw_SE","hw_p","hw_estimate",
                                           "hw_mort_beta","hw_mort_SE","hw_mort_p","hw_mort_estimate",
                                           "hw_cw_beta","hw_cw_SE","hw_cw_p","hw_cw_estimate")))
for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1[,c(2,9,16,17,22)] = scale(data1[,c(2,9,16,17,22)])
  
  # hw
  model = lm(inci_as~year+hw+gdp+gdp:hw,data = data1)
  fit = summary(model)
  result1[i,1:2] = fit$coefficients[3,1:2]
  beta = signif(fit$coefficients[3,1],3)
  p = signif(fit$coefficients[3,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[3,1],3)
  CI_high = signif(confint(model)[3,2],3)
  result1[i,3] = p
  result1[i,4] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  
  # hw controlling for mort_as
  model = lm(inci_as~year+hw+gdp+mort_as+gdp:hw,data = data1)
  fit = summary(model)
  result1[i,5:6] = fit$coefficients[3,1:2]
  beta = signif(fit$coefficients[3,1],3)
  p = signif(fit$coefficients[3,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[3,1],3)
  CI_high = signif(confint(model)[3,2],3)
  result1[i,7] = p
  result1[i,8] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  
  # hw controlling for cw
  model = lm(inci_as~year+hw+gdp+cw+gdp:hw,data = data1)
  fit = summary(model)
  result1[i,9:10] = fit$coefficients[3,1:2]
  beta = signif(fit$coefficients[3,1],3)
  p = signif(fit$coefficients[3,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[3,1],3)
  CI_high = signif(confint(model)[3,2],3)
  result1[i,11] = p
  result1[i,12] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  
}

write.csv(result1,'first stage regression controlling other coveriates.csv')

result1 = result1[,c(1:2,5:6,9:10)]
result1 = apply(result1, 2, as.numeric)

# 2.2 second stage analysis-----------------------------------------------------

meta_analysis = rma(yi = hw_beta, sei = hw_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = hw_mort_beta, sei = hw_mort_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = hw_cw_beta, sei = hw_cw_SE, data = result1)
summary(meta_analysis)

# 3. different heatwave indexes-------------------------------------------------

# 3.1 first stage analysis------------------------------------------------------

### regression in each country
result1 = matrix(0,nrow=length(country),ncol=28,
                 dimnames=list(country,c("hw_beta","hw_SE","hw_p","hw_estimate",
                                         "hw_count_beta","hw_count_SE","hw_count_p","hw_count_estimate",
                                         "hw_days_beta","hw_days_SE","hw_days_p","hw_days_estimate",
                                         "hw_degree_beta","hw_degree_SE","hw_degree_p","hw_degree_estimate",
                                         "hw_mean_degree_beta","hw_mean_degree_SE","hw_mean_degree_p","hw_mean_degree_estimate",
                                         "hw_max_degree_beta","hw_max_degree_SE","hw_max_degree_p","hw_max_degree_estimate",
                                         "hw_mean_length_beta","hw_mean_length_SE","hw_mean_length_p","hw_mean_length_estimate"
                 )))

for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1[,c(2,9:15,17)] = scale(data1[,c(2,9:15,17)])
  for (j in 9:15){
    model = lm(inci_as~year+data1[,j]+gdp+data1[,j]:gdp,data=data1)
    fit = summary(model)
    result1[i,((j-9)*4+1):((j-9)*4+2)] = fit$coefficients[3,1:2]
    beta = signif(fit$coefficients[3,1],3)
    p = signif(fit$coefficients[3,4],2)
    if (p<0.0001){
      p = "<0.0001"
    }
    CI_low = signif(confint(model)[3,1],3)
    CI_high = signif(confint(model)[3,2],3)
    result1[i,((j-9)*4+3)] = p
    result1[i,((j-9)*4+4)] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  }
}

write.csv(result1,'first stage regression different hw indexes.csv')

result1 = result1[,c(1:2,5:6,9:10,13:14,17:18,21:22,25:26)]
result1 = apply(result1, 2, as.numeric)

# 3.2 second stage analysis-----------------------------------------------------

result2 = matrix(0,nrow=7,ncol=8,
                 dimnames=list(c("hw","hw_count","hw_days","hw_degree","hw_mean_degree",
                                 "hw_max_degree","hw_mean_length",
                                 'beta','se','zval','pval','ci.lb','ci.ub','estimate','p_reformed')))

for (i in 1:7){
  
  meta_analysis = rma(yi = result1[,((i-1)*2+1)], sei = result1[,((i-1)*2+2)], data = result1)
  fit = summary(meta_analysis)
  result2[i,1] = fit$beta[1]
  result2[i,2] = fit$se[1]
  result2[i,3] = fit$zval[1]
  result2[i,4] = fit$pval[1]
  result2[i,5] = fit$ci.lb[1]
  result2[i,6] = fit$ci.ub[1]
  
  beta = signif(fit$beta[1],3)
  p = signif(fit$pval[1],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[1],3)
  CI_high = signif(fit$ci.ub[1],3)
  result2[i,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result2[i,8] = p
  
}
write.csv(result2,'second stage regression different hw indexes.csv')

# 4. cubic effect of hw---------------------------------------------------------

# 4.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=20,
                 dimnames=list(country,c("2hw_beta","2hw_SE","2hw_t","2hw_p",
                                         "2hw2_beta","2hw2_SE","2hw2_t","2hw2_p",
                                         "3hw_beta","3hw_SE","3hw_t","3hw_p",
                                         "3hw2_beta","3hw2_SE","3hw2_t","3hw2_p",
                                         "3hw3_beta","3hw3_SE","3hw3_t","3hw3_p"
                 )))

for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1$hw2 = data1$hw**2
  data1$hw3 = data1$hw**3
  data1[,c(2,9,17,28,29)] = scale(data1[,c(2,9,17,28,29)])
  
  model = lm(inci_as~year+(hw+hw2)*gdp,data = data1)
  fit = summary(model)
  result1[i,1:4] = fit$coefficients[3,]
  result1[i,5:8] = fit$coefficients[4,]
  
  model = lm(inci_as~year+(hw+hw2+hw3)*gdp,data = data1)
  fit = summary(model)
  result1[i,9:12] = fit$coefficients[3,]
  result1[i,13:16] = fit$coefficients[4,]
  result1[i,17:20] = fit$coefficients[5,]
  
}

write.csv(result1,'first stage regression nonlinear hw.csv')


# 4.2 second stage analysis-----------------------------------------------------

result2 = matrix(0,nrow=5,ncol=8,
                 dimnames=list(c("2hw","2hw2","3hw","3hw2","3hw3"),
                               c('beta','se','zval','pval','ci.lb','ci.ub','estimate','p_reformed')))

for (i in 1:5){
  
  meta_analysis = rma(yi = result1[,((i-1)*4+1)], sei = result1[,((i-1)*4+2)], data = result1)
  fit = summary(meta_analysis)
  result2[i,1] = fit$beta[1]
  result2[i,2] = fit$se[1]
  result2[i,3] = fit$zval[1]
  result2[i,4] = fit$pval[1]
  result2[i,5] = fit$ci.lb[1]
  result2[i,6] = fit$ci.ub[1]
  
  beta = signif(fit$beta[1],3)
  p = signif(fit$pval[1],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(fit$ci.lb[1],3)
  CI_high = signif(fit$ci.ub[1],3)
  result2[i,7] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result2[i,8] = p
  
}
write.csv(result2,'second stage regression different nonlinear hw.csv')

# 5. without special years------------------------------------------------------

# 5.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=4,
                 dimnames=list(country,c("hw_beta","hw_SE","hw_t","hw_p")))

for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i] & data$year!=7),]
  data1[,c(2,9,17)] = scale(data1[,c(2,9,17)])
  
  model = lm(inci_as~year+hw*gdp,data = data1)
  fit = summary(model)
  result1[i,1:4] = fit$coefficients[3,]
  
}

write.csv(result1,'first stage regression without special year.csv')


# 5.2 second stage analysis-----------------------------------------------------

meta_analysis = rma(yi = hw_beta, sei = hw_SE, data = result1)
summary(meta_analysis)

# 6. with prevalence------------------------------------------------------------

# 6.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=8,
                 dimnames=list(country,c("prev_all_beta","prev_all_SE","prev_all__t","prev_all_p",
                                         "prev_as_beta","prev_as_SE","prev_as__t","prev_as_p")))

for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1[,c(3,4,9,17)] = scale(data1[,c(3,4,9,17)])
  
  model = lm(prev_all~year+hw*gdp,data = data1)
  fit = summary(model)
  result1[i,1:4] = fit$coefficients[3,]
  
  model = lm(prev_as~year+hw*gdp,data = data1)
  fit = summary(model)
  result1[i,5:8] = fit$coefficients[3,]
  
}

write.csv(result1,'first stage regression prevalence.csv')


# 6.2 second stage analysis-----------------------------------------------------

meta_analysis = rma(yi = prev_all_beta, sei = prev_all_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = prev_as_beta, sei = prev_as_SE, data = result1)
summary(meta_analysis)

# 7. using gdp per capita------------------------------------------------------

# 7.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=4,
                 dimnames=list(country,c("hw_beta","hw_SE","hw_t","hw_p")))

for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i] & data$year!=7),]
  data1[,c(2,9,17)] = scale(data1[,c(2,9,17)])
  
  model = lm(inci_as~year+hw*gdp,data = data1)
  fit = summary(model)
  result1[i,1:4] = fit$coefficients[3,]
  
}

write.csv(result1,'first stage regression without special year.csv')


# 7.2 second stage analysis-----------------------------------------------------

meta_analysis = rma(yi = hw_beta, sei = hw_SE, data = result1)
summary(meta_analysis)

# 8. higher order effect of years-----------------------------------------------

# 8.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=16,
                 dimnames = list(country,c("hw1_beta","hw1_SE","hw1_p","hw1_estimate",
                                           "hw2_beta","hw2_SE","hw2_p","hw2_estimate",
                                           "year1_beta","year1_SE","year1_p","year1_estimate",
                                           "year2_beta","year2_SE","year2_p","year2_estimate")))
for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1[,c(2,9,17,18)] = scale(data1[,c(2,9,17,18)])
  model = lm(inci_as~ns(hw,knots=3)+gdp+ns(year,knots=3),data = data1)
  fit = summary(model)
  # hw
  result1[i,1:2] = fit$coefficients[2,1:2]
  beta = signif(fit$coefficients[2,1],3)
  p = signif(fit$coefficients[2,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[2,1],3)
  CI_high = signif(confint(model)[2,2],3)
  result1[i,3] = p
  result1[i,4] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  # year1
  result1[i,5:6] = fit$coefficients[4,1:2]
  beta = signif(fit$coefficients[4,1],3)
  p = signif(fit$coefficients[4,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[4,1],3)
  CI_high = signif(confint(model)[4,2],3)
  result1[i,7] = p
  result1[i,8] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  # year2
  result1[i,9:10] = fit$coefficients[5,1:2]
  beta = signif(fit$coefficients[5,1],3)
  p = signif(fit$coefficients[5,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[5,1],3)
  CI_high = signif(confint(model)[5,2],3)
  result1[i,11] = p
  result1[i,12] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  
}

write.csv(result1,'first stage regression with higher order year effects.csv')

result1 = result1[,c(1:2,5:6,9:10)]
result1 = apply(result1, 2, as.numeric)

# 8.2 second stage analysis-----------------------------------------------------

meta_analysis = rma(yi = hw_beta, sei = hw_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = year1_beta, sei = year1_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = year2_beta, sei = year2_SE, data = result1)
summary(meta_analysis)

# 8. higher order effect of years-----------------------------------------------

# 8.1 first stage analysis------------------------------------------------------

result1 = matrix(0,nrow=length(country),ncol=16,
                 dimnames = list(country,c("hw1_beta","hw1_SE","hw1_p","hw1_estimate",
                                           "hw2_beta","hw2_SE","hw2_p","hw2_estimate",
                                           "year1_beta","year1_SE","year1_p","year1_estimate",
                                           "year2_beta","year2_SE","year2_p","year2_estimate")))
for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1[,c(2,9,17,18)] = scale(data1[,c(2,9,17,18)])
  model = lm(inci_as~ns(hw,knots=3)+gdp+ns(year,knots=3),data = data1)
  fit = summary(model)
  # hw
  result1[i,1:2] = fit$coefficients[2,1:2]
  beta = signif(fit$coefficients[2,1],3)
  p = signif(fit$coefficients[2,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[2,1],3)
  CI_high = signif(confint(model)[2,2],3)
  result1[i,3] = p
  result1[i,4] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  # year1
  result1[i,5:6] = fit$coefficients[4,1:2]
  beta = signif(fit$coefficients[4,1],3)
  p = signif(fit$coefficients[4,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[4,1],3)
  CI_high = signif(confint(model)[4,2],3)
  result1[i,7] = p
  result1[i,8] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  # year2
  result1[i,9:10] = fit$coefficients[5,1:2]
  beta = signif(fit$coefficients[5,1],3)
  p = signif(fit$coefficients[5,4],2)
  if (p<0.0001){
    p = "<0.0001"
  }
  CI_low = signif(confint(model)[5,1],3)
  CI_high = signif(confint(model)[5,2],3)
  result1[i,11] = p
  result1[i,12] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  
}

write.csv(result1,'first stage regression with higher order year effects.csv')

result1 = result1[,c(1:2,5:6,9:10)]
result1 = apply(result1, 2, as.numeric)

# 8.2 second stage analysis-----------------------------------------------------

meta_analysis = rma(yi = hw_beta, sei = hw_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = year1_beta, sei = year1_SE, data = result1)
summary(meta_analysis)

meta_analysis = rma(yi = year2_beta, sei = year2_SE, data = result1)
summary(meta_analysis)

################################################################################
# test the residuals in first-stage analysis
################################################################################

result1 = matrix(0,nrow=length(country),ncol=1,
                 dimnames = list(country,c("p_normal")))
for (i in seq(length(country))){
  
  data1 = data[which(data$country==country[i]),]
  data1[,c(2,9,17,18)] = scale(data1[,c(2,9,17,18)])
  model = lm(inci_as~hw*gdp+ns(year,knots=3),data = data1)
  result1[i,1] = shapiro.test(residuals(model))$p.value
}

write.csv(result1,'residuals in first-stage analysis.csv')

data1 = data[which(data$country=="Azerbaijan"),]
plot(data1$year+1990,data1$inci_as)
data1[,c(2,9,17,18)] = scale(data1[,c(2,9,17,18)])
model = lm(inci_as~hw*gdp+year,data = data1)
plot(residuals(model))
result1[i,1] = shapiro.test(residuals(model))$p.value

################################################################################
# visualization of significant countries at first level
################################################################################

# 创建数据框
data = tibble::tribble(
  ~Country, ~Coefficient, ~p_value,
  "Malawi", 0.574037391, 0.000243601,
  "New Zealand", 0.667565545, 0.00096734,
  "Indonesia", 0.300034711, 0.005114734,
  "Nepal", 0.157930096, 0.0052008,
  "El Salvador", 0.395583599, 0.007036436,
  "Cote d'Ivoire", 0.439922869, 0.00801407,
  "Guyana", 0.329423076, 0.010173165,
  "South Africa", 0.537999869, 0.011535824,
  "Yemen", -0.363696398, 0.012548702,
  "Sierra Leone", 0.401520298, 0.013058568,
  "Angola", 0.463192081, 0.019287163,
  "Papua New Guinea", 0.273411596, 0.020023857,
  "Zambia", 0.262282299, 0.02717257,
  "United States", 0.383169387, 0.027572729,
  "Libyan Arab Jamahiriya", -0.384187177, 0.029040664,
  "Albania", 0.30677021, 0.033731512,
  "Philippines", 0.208448788, 0.048245322
)

# 按系数绝对值排序
data = data %>% mutate(abs_Coefficient = abs(Coefficient)) %>% arrange(desc(abs_Coefficient))

# 创建角度和颜色
num_wedges <- nrow(data)
angles <- seq(0, 2 * pi, length.out = num_wedges + 1)
data$colors <- ifelse(data$Coefficient > 0, 
                      scales::rescale(data$abs_Coefficient, to = c(0.5, 1), from = c(0, max(data$abs_Coefficient))), 
                      scales::rescale(data$abs_Coefficient, to = c(0.5, 1), from = c(0, max(data$abs_Coefficient))) * -1)
data$angle <- angles[-length(angles)] + (angles[2] - angles[1]) / 2
data$hjust <- ifelse(data$angle < pi, 0, 1)
data$angle <- ifelse(data$angle < pi, data$angle * 180 / pi, (data$angle + pi) * 180 / pi)

# 绘制图表
p = ggplot(data, aes(x = factor(Country, levels = Country), y = abs_Coefficient)) +
  geom_bar(stat = "identity", aes(fill = colors), width = 1) +
  coord_polar(theta = "x") +
  scale_fill_gradient2(low = "#01468B", mid = "white", high = "#EC0000", midpoint = 0, space = "Lab", 
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  theme_void() +
  theme(
    text = element_text(size = 10,family="sans"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  #labs(title = "Significantly affected countries at first stage") +
  geom_text(aes(x = factor(Country, levels = Country), y = abs_Coefficient + 0.05, 
                label = Country, angle = 90-data$angle, hjust = hjust), size = 3, 
            position = position_stack(vjust = 0.5))

ggsave(p,filename = 'Significantly affected countries at first stage.pdf',width=4,height=4)





