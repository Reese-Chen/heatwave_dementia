#%% load packages

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random

from pyecharts import options as opts
from pyecharts.charts import Map
from pyecharts.charts import Geo

#%% load data

max_data = pd.read_csv('d:\\heatwave and dementia\\data\\daily max temperature.csv',sep=',',header=None)
pop = np.loadtxt('d:\\heatwave and dementia\\data\\各坐标点2000-2020年均人口密度.csv',delimiter = ',')
hw_cn = np.loadtxt("d:\\heatwave and dementia\\data\\hw_english_countrynames.csv",
                delimiter=',',encoding='utf_8',dtype=str)
countryname = pd.read_csv('d:\\heatwave and dementia\\data\\热浪国家列表1.csv',
                         sep=',',header=None)
pop = pop[4406:,2]

#%% define functions on date

#判断润年
def nian(n):
    if ((n%4==0 and n%100!=0)or(n%400==0)):
        return 1
    else:
        return 0

# 获取doy
def doy(xxday):
    days = 0
    for k in range(1990,2020):
        delta = 365
        if nian(k)==1:
            delta = delta+1
        if (xxday>=days and xxday<days+delta):
            day = xxday-days
            break
        days = days+delta
    return day

#%%

max_data1 = max_data
max_data = max_data1.values

#%% calculate the 25th and 75th temperature threshold

T30y25p = np.zeros((15456,366))
T30y75p = np.zeros((15456,366))
plusminus = 15

for x in range(15456):

    for i in range(365):
        day = i
        valid_data = max_data[max(0,day-plusminus):day+plusminus,x]
        
        for j in range(1990,2019,1): 
            if nian(j)==1:
                day = day+366
            else:
                day = day+365
            valid_data = np.append(valid_data,max_data[day-plusminus:min(day+plusminus,10956),x])
            
        T30y25p[x,i] = np.percentile(valid_data,25)
        T30y75p[x,i] = np.percentile(valid_data,75)
    
    day = 1096
    valid_data = max_data[day-plusminus:day+plusminus,x]
    for j in range(1992,2016,4):
        day = day+365*3+366
        valid_data = np.append(valid_data,max_data[day-plusminus:day+plusminus,x])
    T30y25p[x,365] = np.percentile(valid_data,25)
    T30y75p[x,365] = np.percentile(valid_data,75)
    
#%% calculate cw for each day

cw = np.zeros((15456,10957))

for x in range(15456):
    for i in range(10957): 
        day_of_year = doy(i)
        cw[x,i] =  (max_data[i,x]-T30y75p[x,day_of_year])/(T30y75p[x,day_of_year]-T30y25p[x,day_of_year])
        if cw[x,i]>0:
            cw[x,i] = 0

#%% save cw

cw = pd.DataFrame(cw)
cw.to_csv('d:\\heatwave and dementia\\data\\cw.csv',sep=',',index=False,header=None)

#%% 整合到年份上

cw_year = np.zeros((15456,30))
   
for i in range(15456):
    day = 0 
    for j in range(30):
        this_year = j+1990
        days = 365
        if nian(this_year)==1:
            days = days+1
        cw_year[i,j] = sum(cw.iloc[i,day:day+days])
        day = days+day

#%% 提取所有在陆地上的经纬度点

from global_land_mask import globe
k = 0
for i in range(-85,86,1):
    for j in range(-179,180,1):
        is_on_land = globe.is_land(i,j)
        if (is_on_land):
            k = k+1
            new = [j,i]
            if k==1:
                location = new
            else:
                location = np.column_stack((location,new))
              
location = location[:,4406:]  

location = np.transpose(location)
indx = np.arange(15456)

#%% 寒潮可视化

# 绘图数据提取
cw1 = cw_year.mean(axis=1)
cw0 = cw_year.mean(axis=0)

# 年的变化趋势
x = np.arange(1990,2020)
plt.plot(x,cw0)
plt.xlabel('year')
plt.ylabel('mean_cw') #
plt.title('trend of mean_cw in year')#
plt.show()

# 全球分布
maxx = max(cw1)
minx = min(cw1)

data= np.c_[indx,location,cw1]
data = pd.DataFrame(data,columns=['indx', 'lon', 'lat', 'mean_cw'])#

geo_sight_coord={data['indx'][i]: [data['lon'][i], data['lat'][i]] for i in range(len(data))} #构造位置字典数据
data_pair=[(data['indx'][i], data['mean_cw'][i]) for i in range(len(data))] #构造数据

g=Geo() #地理初始化
g.add_schema(maptype="world") #限定范围
for key, value in geo_sight_coord.items(): #对地理点循环
    g.add_coordinate(key, value[0], value[1]) #追加点位置
g.add("", data_pair, symbol_size=2) 
g.set_series_opts(label_opts=opts.LabelOpts(is_show=False), type='scatter')  #星散点图scatter

g.set_global_opts(
        visualmap_opts=opts.VisualMapOpts(type_="color",max_=maxx,min_=minx), 
        title_opts=opts.TitleOpts(title="distribution of sum of mean_cw")#
    )
g.render("d:\\heatwave and dementia\\pictures\\distribution of mean_cw.html")  

#%% calculate populational weighted heatwave indexs for each country    
        
country = np.unique(hw_cn) #获取所有坐标点对应的全部不重复国家名称
n_country = np.shape(country)[0]
cw_country = np.zeros((n_country,30))
pop_country = np.zeros(n_country)

for i,cname in enumerate(country):
    for j in range(15456):
        if hw_cn[j]==cname:
            
            pop_country[i] = pop_country[i]+pop[j]
            cw_country[i,:] = cw_country[i,:] + pop[j]*cw_year[j,:]

    if pop_country[i]!=0:
        cw_country[i,:] = cw_country[i,:]/pop_country[i]

#%% save populational weighted hwmid

cw_country = pd.DataFrame(cw_country)
cw_country['country'] = country
cw_country.to_csv('d:\\heatwave and dementia\\data\\cw_country.csv',sep=',',index=False,header=None)

#%% get colors
def randomcolor():
    colorArr = ['1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
    color = ""
    for i in range(6):
        color += colorArr[random.randint(0,14)]
    return "#"+color

#%% visualization

xarea = list(cw_country.country)
ynum = list(cw_country.mean(axis=1).values)
maxy = max(ynum)
miny = min(ynum)

c = (
    Map()
    .add("cw_country", [list(z) for z in zip(xarea, ynum)], "world")
    .set_series_opts(label_opts=opts.LabelOpts(is_show=False))
    .set_global_opts(
        title_opts=opts.TitleOpts(title="distribution of cw_country"),
        visualmap_opts=opts.VisualMapOpts(min_=miny,max_=maxy,is_piecewise=True),
    )
    .render("d:\\heatwave and dementia\\pictures\\distribution of cw_country.html")
) 

#%%
year = np.arange(1990,2020)
labels = []
for i in range(166):
    xhw = np.array(pd.to_numeric(cw_country.iloc[i,:30])) #
    xcountry = cw_country.iloc[i,30]                      #
    plt.plot(year,xhw,color=randomcolor(),alpha=0.8)
    labels.append(xcountry)

plt.legend(labels,ncol=10,bbox_to_anchor=(1.05, 0), loc=3, borderaxespad=0)
plt.xlabel('year')
plt.ylabel('cw')
plt.title('population weighted cw in each country')
plt.show()
