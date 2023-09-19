#%%

###############################################################################
### from 1990-01-01 to 2019-12-31 min/mean/max temperature on each grid cell
###############################################################################

import numpy as np
import pandas as pd

#%%
import ee
import os
ee.Authenticate()
if __name__ == '__main__':
    os.environ['HTTP_PROXY'] = 'http://127.0.0.1:10809'
    os.environ['HTTPS_PROXY'] = 'http://127.0.0.1:10809'
ee.Initialize()

#%%
era5_2mt_max = ee.ImageCollection('ECMWF/ERA5/DAILY').select('maximum_2m_air_temperature').filter(ee.Filter.date('1990-01-01', '2020-01-01'));
era5_2mt_mean = ee.ImageCollection('ECMWF/ERA5/DAILY').select('mean_2m_air_temperature').filter(ee.Filter.date('1990-01-01', '2020-01-01'));
era5_2mt_min = ee.ImageCollection('ECMWF/ERA5/DAILY').select('minimum_2m_air_temperature').filter(ee.Filter.date('1990-01-01', '2020-01-01'));

#%%

# extract data around a grid point
def getdata_max(lon,lat): 
    poi = ee.Geometry.Point(lon, lat)
    scale = 1000
    return era5_2mt_max.getRegion(poi, scale).getInfo()

def getdata_min(lon,lat): 
    poi = ee.Geometry.Point(lon, lat)
    scale = 1000
    return era5_2mt_min.getRegion(poi, scale).getInfo()

def getdata_mean(lon,lat): 
    poi = ee.Geometry.Point(lon, lat)
    scale = 1000
    return era5_2mt_mean.getRegion(poi, scale).getInfo()


#convert data to time series
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

    # Keep the columns of interest.
    #df = df[['longitude', 'latitude','time','datetime',  *list_of_bands]]
    df = df[['datetime',  *list_of_bands]]

    return df


#%%

# extract all grid cells
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
                  
#%%

# get average min/mean/max temperature on each grid cell
#df = pd.DataFrame(data=None,columns=['lon','lat','max','min','mean'])

for i in range(2644,3336):
    
    lon = int(location[0,i])
    lat = int(location[1,i])
    
    era5_2mt_max_poi = getdata_max(lon,lat)
    era5_2mt_min_poi = getdata_min(lon,lat)
    era5_2mt_mean_poi = getdata_mean(lon,lat)
    
    maxt = ee_array_to_df(era5_2mt_max_poi,['maximum_2m_air_temperature'])['maximum_2m_air_temperature']-273
    mint = ee_array_to_df(era5_2mt_min_poi,['minimum_2m_air_temperature'])['minimum_2m_air_temperature']-273
    meant = ee_array_to_df(era5_2mt_mean_poi,['mean_2m_air_temperature'])['mean_2m_air_temperature']-273
    
    if i==0:
        max_series = maxt.values
        min_series = mint.values
        mean_series = meant.values
    else:
        max_series = np.c_[max_series,maxt.values]
        min_series = np.c_[min_series,mint.values]
        mean_series = np.c_[mean_series,meant.values]
    

#%%

df1 = pd.DataFrame(max_series)
df1.to_csv('d:\\heatwave and dementia\\data\\daily max temperature3336.csv',sep=',',index=False,header=None)
df2 = pd.DataFrame(min_series)
df2.to_csv('d:\\heatwave and dementia\\data\\daily min temperature3336.csv',sep=',',index=False,header=None)
df3 = pd.DataFrame(mean_series)
df3.to_csv('d:\\heatwave and dementia\\data\\daily mean temperature3336.csv',sep=',',index=False,header=None)

#%%

max_series = pd.read_csv(r'd:\\heatwave and dementia\\data\\daily max temperature1112.csv',
                        sep=',',header=None)
#max_series = max_series.iloc[1:10958,1:]
min_series = pd.read_csv(r'd:\\heatwave and dementia\\data\\daily min temperature1112.csv',
                        sep=',',header=None)
#min_series = min_series.iloc[1:10958,1:]
mean_series = pd.read_csv(r'd:\\heatwave and dementia\\data\\daily mean temperature1112.csv',
                        sep=',',header=None)
#mean_series = mean_series.iloc[1:10958,1:]
