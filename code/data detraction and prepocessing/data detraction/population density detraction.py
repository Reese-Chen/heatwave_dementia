#%%
import ee
import os
ee.Authenticate()
if __name__ == '__main__':
    os.environ['HTTP_PROXY'] = 'http://127.0.0.1:10809'
    os.environ['HTTPS_PROXY'] = 'http://127.0.0.1:10809'
ee.Initialize()

#%%
#today = ee.Date('2020-01-01')
#date_range = ee.DateRange(today.advance(-20, 'years'), today)
#pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Basic_Demographic_Characteristics").select('basic_demographic_characteristics').filter(ee.Filter.calendarRange(2000,2020,'year'))

#pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Basic_Demographic_Characteristics").select('basic_demographic_characteristics').filterDate('2000-01-01','2020-01-01')
pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Basic_Demographic_Characteristics").select('basic_demographic_characteristics');

#%%
full_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Basic_Demographic_Characteristics")\
             .filter(ee.Filter.eq('Sex', 'b'))\
             .filter(ee.Filter.eq('Age_Group','Total Population'))\
             .select('basic_demographic_characteristics')

#%%
def getdata(lon,lat): 
    poi = ee.Geometry.Point(lon, lat)
    scale = 5000
    return full_pop.getRegion(poi, scale).getInfo()

import pandas as pd
#定义函数将某个坐标点数据转化为时间序列
def ee_array_to_df(arr, list_of_bands):
    """Transforms client-side ee.Image.getRegion array to pandas.DataFrame."""
    df = pd.DataFrame(arr)

    # Rearrange the header.
    headers = df.iloc[0]
    df = pd.DataFrame(df.values[1:], columns=headers)

    # Remove rows without data inside.
    df = df[['longitude', 'latitude', 'time', *list_of_bands]].dropna()

    # Convert the data to numeric values.
    for band in list_of_bands:
        df[band] = pd.to_numeric(df[band], errors='coerce')

    # Convert the time field into a datetime.
    df['datetime'] = pd.to_datetime(df['time'], unit='ms')
    df['year'] = df['datetime'].dt.year

    # Keep the columns of interest.
    #df = df[['longitude', 'latitude','time','datetime',  *list_of_bands]]
    df = df[['datetime', 'year', *list_of_bands]]

    return df

#%%
#提取所有在陆地上的经纬度点
import numpy as np
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
#%% 实验

lon = int(location[0,10000])
lat = int(location[1,10000])
pop_poi = getdata(lon,lat)
pop_series = ee_array_to_df(pop_poi,['basic_demographic_characteristics'])
           

#%%
#ans = np.zeros(15456)
ans1 = np.zeros(19862)
for i in range(15456,19862):
    lon = int(location[0,i])
    lat = int(location[1,i])
    pop_poi = getdata(lon,lat)
    pop_series = ee_array_to_df(pop_poi,['basic_demographic_characteristics'])
    if len(np.array(pop_series.basic_demographic_characteristics)):
        ans1[i] = max(np.array(pop_series.basic_demographic_characteristics))

#%%

pop_matrix = np.c_[location.T,ans1]

#%%
np.savetxt('d:\\heatwave and dementia\\data\\各坐标点2000-2020年均人口密度new.csv', pop_matrix, delimiter = ',')

#%% 

pop_matrix = np.loadtxt('d:\\heatwave and dementia\\data\\各坐标点2000-2020年均人口密度.csv', delimiter = ',')

#%% 绘图展示
from pyecharts import options as opts
from pyecharts.charts import Geo
from pyecharts.render import make_snapshot
from snapshot_phantomjs import snapshot
import pandas as pd

data = pop_matrix[4406:,:]
indx = np.arange(15456)
data= np.c_[indx,data]
data = pd.DataFrame(data,columns=['indx', 'lon', 'lat', 'pop'])
geo_sight_coord={data['indx'][i]: [data['lon'][i], data['lat'][i]] for i in range(len(data))} #构造位置字典数据
data_pair=[(data['indx'][i], data['pop'][i]) for i in range(len(data))] #构造项目租金数据
pieces = [
        {'min': 0,'max': 10, 'label': '0-10', 'color': '#3700A4'},
        {'min': 10,'max': 100, 'label': '10-100', 'color': '#50A3BA'},
        {'min': 100,'max': 1000, 'label': '100-1000', 'color': '#81AE9F'},
        {'min': 1000,'max': 10000, 'label': '1000-10000', 'color': '#DD675E'},
        {'min': 10000, 'max': 14900, 'label': '10000+', 'color': '#D94E5D'}
]
g=Geo() #地理初始化
g.add_schema(maptype="world") #限定范围
for key, value in geo_sight_coord.items(): #对地理点循环
    g.add_coordinate(key, value[0], value[1]) #追加点位置
g.add("population density", data_pair, symbol_size=2) 
g.set_series_opts(label_opts=opts.LabelOpts(is_show=False), type='scatter')  #星散点图scatter
g.set_global_opts(
    visualmap_opts=opts.VisualMapOpts(is_piecewise=True,pieces=pieces)
) 
#g.render("d:\\heatwave and dementia\\data\\population distribution1.html")
make_snapshot(snapshot,g.render(),"d:\\heatwave and dementia\\pictures\\126国家结果可视化\\population distribution.pdf",delay=10.0)
