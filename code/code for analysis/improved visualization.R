# load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(grid)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(foreign)
library(ggExtra)
library(bstfun)
library(gtsummary)
library(forester)
library(forestploter)
library(interactions)
library(ggeffects)
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
# form accdata and increment data
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
# visualization for mixed-effect models
################################################################################

# 1. improved plot for Figure 2 B-----------------------------------------------

accdata_plot_low = matrix(NA,nrow=10,ncol=6)
accdata_plot_high = matrix(NA,nrow=10,ncol=6)

cutoff = c(min(accdata$acc3),seq(from=450,to=850,by=50),max(accdata$acc3))

for (i in 1:10){
  
  data_plot_low = accdata[which(accdata$acc3<cutoff[i+1] & accdata$acc3>=cutoff[i] & accdata$gnigroup=='mid and low income'),]
  if (dim(data_plot_low)[1]==1){
    accdata_plot_low[i,1] = data_plot_low$acc3
    accdata_plot_low[i,2:4] = data_plot_low$inci_as_delta
  }
  else if (dim(data_plot_low)[1]>1){
    accdata_plot_low[i,1] = mean(data_plot_low$acc3)
    accdata_plot_low[i,2:4] = CI(data_plot_low$inci_as_delta,ci=0.95)
  }
  accdata_plot_low[i,5] = dim(data_plot_low)[1]
  
  data_plot_high = accdata[which(accdata$acc3<cutoff[i+1] & accdata$acc3>=cutoff[i] & accdata$gnigroup=='high income'),]
  if (dim(data_plot_high)[1]==1){
    accdata_plot_high[i,1] = data_plot_high$acc3
    accdata_plot_high[i,2:4] = data_plot_high$inci_as_delta
  }
  else if (dim(data_plot_high)[1]>1){
    accdata_plot_high[i,1] = mean(data_plot_high$acc3)
    accdata_plot_high[i,2:4] = CI(data_plot_high$inci_as_delta,ci=0.95)
  }
  accdata_plot_high[i,5] = dim(data_plot_high)[1]
  
}

accdata_plot_low[,6] = rep('Low- to middle-income',10)
accdata_plot_high[,6] = rep('High-income',10)

accdata_plot = rbind(accdata_plot_low,accdata_plot_high)
accdata_plot = as.data.frame(accdata_plot)
colnames(accdata_plot) = c('ACC3','up','incidence_delta','low','size','GNIgroup')
accdata_plot$ACC3 = as.numeric(accdata_plot$ACC3)
accdata_plot$incidence_delta = as.numeric(accdata_plot$incidence_delta)
accdata_plot$up = as.numeric(accdata_plot$up)
accdata_plot$low = as.numeric(accdata_plot$low)
accdata_plot$size = as.numeric(accdata_plot$size)
accdata_plot=accdata_plot[complete.cases(accdata_plot),]

caption1 <- "0.00216 (0.00121 to 0.00134), p<0.0001"
caption2 <- "0.00181 (0.00154 to 0.00208), p<0.0001"


p =ggplot()+
  geom_point(aes(x=ACC3,y=incidence_delta,color=GNIgroup),accdata_plot)+
  geom_errorbar(aes(x=ACC3,y=incidence_delta,ymin = low, ymax = up),accdata_plot,width=0.1)+
  geom_smooth(aes(x=ACC3,y=incidence_delta,color=GNIgroup),accdata_plot,method='lm')+
  xlab('ACC3')+
  ylab('Change of incidence')+
  scale_x_continuous(limits = c(350, 950))+
  scale_y_continuous(limits = c(-0.27, 0.3))+
  theme_minimal()+
  theme(text = element_text(size = 12,family='sans'),legend.position = "none")+
  scale_colour_manual(name="Countries",values = 
                        c('High-income'='#01468B','Low- to middle-income'='#EC0000'))+
  scale_size_continuous(range = c(2, 10)) +
  annotate(
    geom = "text", x = 360, y = 0.29, 
    label = caption1, hjust = 0, vjust = 1, size = 3,color='#01468B'
  )+
  annotate(
    geom = "text", x = 360, y = 0.25, 
    label = caption2, hjust = 0, vjust = 1, size = 3,color='#EC0000'
  )
ggsave(p,filename = 'relationship between inci_as_delta and acc3 grouped by gni8.pdf',width=4,height=4)


# 2. visualization of Table1----------------------------------------------------

df =  read.table(file = "plot data for table1.csv", header = T ,
                      check.names=F,sep = "," , fill = TRUE , encoding = "UTF-8")
colnames(df) = c('Moderater','Estimate','CI_low','CI_high','p')


p1 = forester(
  left_side_data= df[,c(1,5)],
  estimate = df$Estimate,
  ci_low = df$CI_low,
  ci_high = df$CI_high,
  ci_sep = " to ",
  #right_side_data = df[,5],
  estimate_precision = 3,
  ggplot_width = 18,
  null_line_at = 0,
  file_path = file.path("d:/heatwave and dementia/data", paste0("forester_plot.pdf")),
  dpi = 600,
  display = TRUE,
  font_family = "sans",
  estimate_col_name = "Estimate",
  stripe_colour = "white",
  background_colour = "white",
  x_scale_linear = TRUE,
  xlim = c(-0.001,0.003),
  xbreaks = c(-0.001,0,0.001,0.002,0.003),
  nudge_y = 0,
  nudge_x = 1,
  nudge_height = 0,
  nudge_width = 0,
  justify = 0,
  arrows = FALSE,
  arrow_labels = c("Lower", "Higher"),
  add_plot = NULL,
  add_plot_width = 1,
  add_plot_gap = FALSE,
  point_sizes = 3,
  point_shapes = 18,
  center_ggplot = NULL,
  lower_header_row = FALSE,
  render_as = "pdf"
)

# 3. visualization of interaction term------------------------------------------

plot_data1 = data
plot_data1$gdp = log(plot_data1$gdp)
plot_data1[,c(2,9,17)] = scale(plot_data1[,c(2,9,17)])

model = lmer(inci_as~year*hw*gdp+(1+year|country),data = plot_data1,
             control = lmerControl(optimizer ="Nelder_Mead"))

plot_data2 = accdata
plot_data2$gdp = log(plot_data2$gdp)
plot_data2[,c(1,3,9)] = scale(plot_data2[,c(1,3,9)])

model = lmer(inci_as~year*acc3*gdp+(1+year|country),data = plot_data2,
             control = lmerControl(optimizer='Nelder_Mead'))

# 3.1 interaction of acc3*year--------------------------------------------------

ybreaks = c(-0.2,-0.1,0,0.1)
inci_mean = mean(accdata$inci_as)
inci_sd = sd(accdata$inci_as)
ybreaks_label = round(ybreaks*inci_sd+inci_mean,digits = 0)

interact_p1 =
  interact_plot(model, pred = year, modx = acc3, interval = TRUE,line.thickness=2,colors='CUD Bright')+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")+
  scale_y_continuous(breaks=ybreaks, labels =ybreaks_label)+
  xlab('Year')+
  ylab('Age_standardized incidence')
ggsave(interact_p1,filename = 'interaction of year acc3.pdf',width=4,height=4)


# 3.4 interaction of hw:year:gdp------------------------------------------------

gdp_median = median(plot_data2$gdp) 

# 生成交互作用预测
preds <- ggpredict(model, terms = c("year", "hw"), condition=c(gdp=gdp_median))

# 绘制交互作用图
interact_p2=ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  labs(x = "Year", y = "Inci_as", color = "HW") +
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")+
  ggtitle(paste("Interaction of year and hw at gdp =", gdp_median))
ggsave(interact_p2,filename = 'interaction of year hw with constant gdp.pdf',width=4,height=4)

