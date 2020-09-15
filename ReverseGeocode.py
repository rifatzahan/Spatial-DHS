import time
import numpy as np
import pandas as pd
from pygeocoder import Geocoder
import geocoder

# import data for latitude and longitude
df = pd.DataFrame(pd.read_csv('gps_cluster.csv'))


# Create empty array for District, SubDistrict and Route name
District = []
SubDistrict = []
Route = []

# create a for loop to append the values of District, SubDistrict and Route
for i in range(0, len(df)):
    # Try to,
    try:
        District.append((Geocoder.reverse_geocode(df.lat[i], df.lon[i])).administrative_area_level_2)
        SubDistrict.append((Geocoder.reverse_geocode(df.lat[i], df.lon[i])).administrative_area_level_3)
        Route.append((Geocoder.reverse_geocode(df.lat[i], df.lon[i])).route)
    # But if you get an error
    except:
        # append a missing value
        District.append(np.NaN)
        SubDistrict.append(np.NaN)
        Route.append(np.NaN)
    time.sleep(1) # this is for giving Python a pause so that we do not download the exceeded amount of data

# Create columns for these data in data frame
df['District'] = District
df['SubDistrict'] = SubDistrict
df['Route'] = Route

# export the data to a csv
df.to_csv('gps_address.csv')

#########################################
#### Assign Lat/Lon to the Districts ####
#########################################

# import data of unique districts
df = pd.DataFrame(pd.read_csv('district_death_64.csv'))

DistLoc = []
Division = []

for i in range(0, len(df)):
    DistLoc.append((geocoder.google(df.District[i])).latlng)
    Division.append((geocoder.google(df.District[i])).state)

DistLoc = pd.DataFrame(DistLoc, columns=['lat', 'lon'])

df['lat'] = DistLoc.lat
df['lon'] = DistLoc.lon
df['Division'] = Division

df.to_csv('district_death_count_with_location.csv')


