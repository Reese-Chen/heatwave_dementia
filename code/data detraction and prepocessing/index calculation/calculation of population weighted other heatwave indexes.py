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
hw = np.loadtxt('d:\\heatwave and dementia\\data\\hw(第一列是编号).csv',delimiter = ',')
pop = np.loadtxt('d:\\heatwave and dementia\\data\\各坐标点2000-2020年均人口密度.csv',delimiter = ',')
hw_cn = np.loadtxt("d:\\heatwave and dementia\\data\\hw_english_countrynames.csv",
                delimiter=',',encoding='utf_8',dtype=str)
countryname = pd.read_csv('d:\\heatwave and dementia\\data\\热浪国家列表1.csv',
                         sep=',',header=None)
hw = hw[:,1:]
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

#%% calculate the 90th temperature threshold

T30y90p = np.zeros((15456,366))
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
            
        T30y90p[x,i] = np.percentile(valid_data,90)
    
    day = 1096
    valid_data = max_data[day-plusminus:day+plusminus,x]
    for j in range(1992,2016,4):
        day = day+365*3+366
        valid_data = np.append(valid_data,max_data[day-plusminus:day+plusminus,x])
    T30y90p[x,365] = np.percentile(valid_data,90)
        
#%% define days that exceed temperature threshold

EE = np.zeros((15456,10957))

for x in range(15456):
    for i in range(10957): 
        day_of_year = doy(i)
        if max_data[i,x]>T30y90p[x,day_of_year]:
            EE[x,i]=1

#%% calculate heatwave index for each point

HW_count = np.zeros((15456,30))
HW_days = np.zeros((15456,30))
HW_degree = np.zeros((15456,30))
HW_mean_length = np.zeros((15456,30))
HW_mean_degree = np.zeros((15456,30))
HW_max_degree = np.zeros((15456,30))

for x in range(15456):
    days = 0
    for i in range(1990,2020):
        HW_count[x,i-1990] = 0
        p1 = days
        hwmidm = np.zeros(100)
        if nian(i)==1:
            delta = 366
        else:
            delta = 365
            
        while (p1<days+delta) and (EE[x,p1]==0):
            p1 = p1+1
        p2 = p1+1
        
        while p2<days+delta:
            if EE[x,p2]==0:
                if p2-p1>=3:
                   HW_count[x,i-1990] = HW_count[x,i-1990]+1
                   HW_days[x,i-1990] = HW_days[x,i-1990]+p2-p1
                   HW_degree[x,i-1990] = HW_degree[x,i-1990]+np.sum(hw[p1:p2,x])
                   hwmidm[int(HW_count[x,i-1990])] = np.sum(hw[p1:p2,x])
                while (p2<days+delta) and (EE[x,p2]==0):
                    p2 = p2+1
                p1 = p2
            p2 = p2+1
 
        days = days+delta
        HW_max_degree[x,i-1990] = np.max(hwmidm)
        if HW_count[x,i-1990]>0:
            HW_mean_degree[x,i-1990] = HW_degree[x,i-1990]/HW_count[x,i-1990]
            HW_mean_length[x,i-1990] = HW_days[x,i-1990]/HW_count[x,i-1990]

#%% save heatwave index for a year 

HW_count = pd.DataFrame(HW_count)
HW_days = pd.DataFrame(HW_days)
HW_degree = pd.DataFrame(HW_degree)
HW_mean_length = pd.DataFrame(HW_mean_length)
HW_mean_degree = pd.DataFrame(HW_mean_degree)
HW_max_degree = pd.DataFrame(HW_max_degree)

HW_count.to_csv('d:\\heatwave and dementia\\data\\HW_count.csv',sep=',',index=False,header=None)
HW_days.to_csv('d:\\heatwave and dementia\\data\\HW_days.csv',sep=',',index=False,header=None)
HW_degree.to_csv('d:\\heatwave and dementia\\data\\HW_degree.csv',sep=',',index=False,header=None)
HW_mean_length.to_csv('d:\\heatwave and dementia\\data\\HW_mean_length.csv',sep=',',index=False,header=None)
HW_mean_degree.to_csv('d:\\heatwave and dementia\\data\\HW_mean_degree.csv',sep=',',index=False,header=None)
HW_max_degree.to_csv('d:\\heatwave and dementia\\data\\HW_max_degree.csv',sep=',',index=False,header=None)

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

#%% 热浪程度可视化

# 绘图数据提取
HW_index1 = HW_mean_length.mean(axis=1).values
HW_index0 = HW_mean_length.mean(axis=0).values

# 年的变化趋势
x = np.arange(1990,2020)
plt.plot(x,HW_index0)
plt.xlabel('year')
plt.ylabel('mean_HW_mean_length') #
plt.title('trend of mean_HW_mean_length in year')#
plt.show()

# 全球分布
maxx = max(HW_index1)
minx = min(HW_index1)

data= np.c_[indx,location,HW_index1]
data = pd.DataFrame(data,columns=['indx', 'lon', 'lat', 'mean_HW_mean_length'])#

geo_sight_coord={data['indx'][i]: [data['lon'][i], data['lat'][i]] for i in range(len(data))} #构造位置字典数据
data_pair=[(data['indx'][i], data['mean_HW_mean_length'][i]) for i in range(len(data))] #构造数据

g=Geo() #地理初始化
g.add_schema(maptype="world") #限定范围
for key, value in geo_sight_coord.items(): #对地理点循环
    g.add_coordinate(key, value[0], value[1]) #追加点位置
g.add("", data_pair, symbol_size=2) 
g.set_series_opts(label_opts=opts.LabelOpts(is_show=False), type='scatter')  #星散点图scatter

g.set_global_opts(
        visualmap_opts=opts.VisualMapOpts(type_="color",max_=maxx,min_=minx), 
        title_opts=opts.TitleOpts(title="distribution of sum of mean_HW_mean_length")#
    )
g.render("d:\\heatwave and dementia\\pictures\\heatwave indexs\\distribution of mean_HW_mean_length.html")  

#%% calculate populational weighted heatwave indexs for each country    
        
country = np.unique(hw_cn) #获取所有坐标点对应的全部不重复国家名称
n_country = np.shape(country)[0]

HW_count_country = np.zeros((n_country,30))
HW_days_country = np.zeros((n_country,30))
HW_degree_country = np.zeros((n_country,30))
HW_mean_length_country = np.zeros((n_country,30))
HW_mean_degree_country = np.zeros((n_country,30))
HW_max_degree_country = np.zeros((n_country,30))

pop_country = np.zeros(n_country)

for i,cname in enumerate(country):
    for j in range(15456):
        if hw_cn[j]==cname:
            
            pop_country[i] = pop_country[i]+pop[j]
            
            HW_count_country[i,:] = HW_count_country[i,:] + pop[j]*HW_count.loc[j].values
            HW_days_country[i,:] = HW_days_country[i,:] + pop[j]*HW_days.loc[j].values
            HW_degree_country[i,:] = HW_degree_country[i,:] + pop[j]*HW_degree.loc[j].values
            HW_mean_length_country[i,:] = HW_mean_length_country[i,:] + pop[j]*HW_mean_length.loc[j].values
            HW_mean_degree_country[i,:] = HW_mean_degree_country[i,:] + pop[j]*HW_mean_degree.loc[j].values
            HW_max_degree_country[i,:] = HW_max_degree_country[i,:] + pop[j]*HW_max_degree.loc[j].values
    
    if pop_country[i]!=0:
        HW_count_country[i,:] = HW_count_country[i,:]/pop_country[i]
        HW_days_country[i,:] = HW_days_country[i,:]/pop_country[i]
        HW_degree_country[i,:] = HW_degree_country[i,:]/pop_country[i]
        HW_mean_length_country[i,:] = HW_mean_length_country[i,:]/pop_country[i]
        HW_mean_degree_country[i,:] = HW_mean_degree_country[i,:]/pop_country[i]
        HW_max_degree_country[i,:] = HW_max_degree_country[i,:]/pop_country[i]
        

#%% save populational weighted hwmid

HW_count_country = pd.DataFrame(HW_count_country)
HW_days_country = pd.DataFrame(HW_days_country)
HW_degree_country = pd.DataFrame(HW_degree_country)
HW_mean_length_country= pd.DataFrame(HW_mean_length_country)
HW_mean_degree_country = pd.DataFrame(HW_mean_degree_country)
HW_max_degree_country = pd.DataFrame(HW_max_degree_country)

HW_count_country['country'] = country
HW_days_country['country'] = country
HW_degree_country['country'] = country
HW_mean_length_country['country'] = country
HW_mean_degree_country['country'] = country
HW_max_degree_country['country'] = country

HW_count_country.to_csv('d:\\heatwave and dementia\\data\\HW_count_country.csv',sep=',',index=False,header=None)
HW_days_country.to_csv('d:\\heatwave and dementia\\data\\HW_days_country.csv',sep=',',index=False,header=None)
HW_degree_country.to_csv('d:\\heatwave and dementia\\data\\HW_degree_country.csv',sep=',',index=False,header=None)
HW_mean_length_country.to_csv('d:\\heatwave and dementia\\data\\HW_mean_length_country.csv',sep=',',index=False,header=None)
HW_mean_degree_country.to_csv('d:\\heatwave and dementia\\data\\HW_mean_degree_country.csv',sep=',',index=False,header=None)
HW_max_degree_country.to_csv('d:\\heatwave and dementia\\data\\HW_max_degree_country.csv',sep=',',index=False,header=None)

#%% get colors
def randomcolor():
    colorArr = ['1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
    color = ""
    for i in range(6):
        color += colorArr[random.randint(0,14)]
    return "#"+color

#%% visualization

xarea = list(HW_mean_length_country.country)
ynum = list(HW_mean_length_country.mean(axis=1).values)
maxy = max(ynum)
miny = min(ynum)

c = (
    Map()
    .add("(HW_mean_length_country", [list(z) for z in zip(xarea, ynum)], "world")
    .set_series_opts(label_opts=opts.LabelOpts(is_show=False))
    .set_global_opts(
        title_opts=opts.TitleOpts(title="distribution of (HW_mean_length_country"),
        visualmap_opts=opts.VisualMapOpts(min_=miny,max_=maxy,is_piecewise=True),
    )
    .render("d:\\heatwave and dementia\\pictures\\heatwave indexs\\distribution of (HW_mean_length_country.html")
) 

#%%
year = np.arange(1990,2020)
labels = []
for i in range(166):
    xhw = np.array(pd.to_numeric(HW_mean_length_country.iloc[i,:30])) #
    xcountry = HW_mean_length_country.iloc[i,30]                      #
    plt.plot(year,xhw,color=randomcolor(),alpha=0.8)
    labels.append(xcountry)

plt.legend(labels,ncol=10,bbox_to_anchor=(1.05, 0), loc=3, borderaxespad=0)
plt.xlabel('year')
plt.ylabel('HW_mean_length')
plt.title('population weighted HW_mean_length in each country')
plt.show()

            





