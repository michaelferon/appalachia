#Code to perform a basic shifting of data points based on their wind vector at the time of measurement
#Pseudo-equation : X_wind = lat/long point + (wind vector)(amount of time in hours)

#FIXME Implement as function to allow for easy calling of different time shifts

#Number of hours forward to shift data
time_shift = 24



#Initialize X_wind
X_wind = X

for (row in 1:nrow(X)){
  #69 (roughly) represents number of miles in 1 degree latitude.
  #Difficult to calculate exactly like for longitude, but 69 should be an accurate enough measurement for our purposes
  X_wind[row,'latitude'] = X[row,'latitude'] + (1/69) *time_shift * X[row,'eastward_wind']
  
  #Length of 1 degree of Longitude in miles = cosine (latitude in decimal degrees) * length of degree (miles) at equator.
  #Could change this to use an integral to exactly calculate time shift and remove some bias
  X_wind[row,'longitude'] = X[row,'longitude'] + (1/69.172) * cos(X[row,'latitude']) * time_shift *X[row,'northward_wind']
  
  #Times must be shifted forward by time_shift hours
  #Date data type allows for addition of seconds
  X_wind[row,'time'] = X[row,'time'] + 60*60*time_shift
  X_wind[row,'time_utc'] = X[row,'time_utc'] + 60*60*time_shift
}  

#FIXME  
#X_wind should probably be cleaned here; wind speed, surface altitude, quadrant no longer meaningful
#I'll wait to make changes until we know how we want to use the data