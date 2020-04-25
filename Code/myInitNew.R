## sample_netcdf_files.R 
## Lewis Blake
## based of sample_conus_v2.py by Sean Crowell
## Last updated 1/16/2020


#####################################################
# DO NOT RUN -- SEE process.R
#####################################################


# some libraries we'll be using. install if not already available
rm( list = ls() )
library(ncdf4)
library(lubridate)

file_path = "/Volumes/SSD/Appalachia/Data/data-full/data-new"
outfile_name = "/Volumes/SSD/Appalachia/Data/data-full/varDictNew.Rdata"
file_names = list.files(file_path)[1:97]
".DS_Store" %in% file_names
any(is.na(file_names))
length(file_names)


setwd(file_path) # set the working directory so to be file where data is located

# use same region as Sean
region_name = 'haynesville'
lat_lb = 29.498 - 0.001
lat_ub = 35.518 + 0.001
lon_lb = -98.679 - 0.001
lon_ub = -88.792 + 0.001
vars_to_sample = c()

#lists of the variable names we want to copy over, separated by the netCDF group they reside in
product_vars_to_sample = c('latitude','longitude','time','time_utc','qa_value','methane_mixing_ratio','methane_mixing_ratio_precision','methane_mixing_ratio_bias_corrected')
geo_vars_to_sample = c('solar_zenith_angle','viewing_zenith_angle','latitude_bounds','longitude_bounds')
detailed_vars_to_sample = c('water_total_column','surface_albedo_SWIR','surface_albedo_NIR','aerosol_optical_thickness_SWIR','aerosol_optical_thickness_NIR')
input_vars_to_sample = c('surface_altitude','surface_altitude_precision','surface_classification','surface_pressure','apparent_scene_pressure','methane_weak_twoband_total_column','methane_strong_twoband_total_column', 'eastward_wind', 'northward_wind')


#creating a universal list of variables that has the netcdf group included for ease of reading
# R doesn't do inline for-loops like python
for(i in 1:length(product_vars_to_sample)){
  vars_to_sample = c(vars_to_sample, paste("PRODUCT/", product_vars_to_sample[i], sep="") )
}
for(j in 1:length(geo_vars_to_sample)){
  vars_to_sample = c(vars_to_sample, paste("GEOLOCATIONS/", geo_vars_to_sample[j], sep="") )
}
for(k in 1:length(detailed_vars_to_sample)){
  vars_to_sample = c(vars_to_sample, paste("DETAILED_RESULTS/", detailed_vars_to_sample[k], sep="") )
}
for(l in 1:length(input_vars_to_sample)){
  vars_to_sample = c(vars_to_sample, paste("INPUT_DATA/", input_vars_to_sample[l], sep="") )
}
# R also doesn't have dictionaries, but lists can be a good approximation. we'll call it a dictionary for consistency.
# initialize the variables
var_dict_new = vector(mode="list", length = 34)
additional_var_names = c("latitude_ll", "longitude_ll", "latitude_lr", "longitude_lr", "latitude_ur", "longitude_ur", "latitude_ul", "longitude_ul")
names(var_dict_new) = c(product_vars_to_sample, geo_vars_to_sample, detailed_vars_to_sample, input_vars_to_sample, additional_var_names)


OBS_EXIST = FALSE
# main for-loop
counter <- 1
for(file_name in file_names){
  print(counter)
  print(file_name)
  nc_data = nc_open(file_name, write = FALSE)
  lat = c(ncvar_get(nc_data, "PRODUCT/latitude") ) # putting into a vector flattens the matrix
  lon = c(ncvar_get(nc_data, "PRODUCT/longitude") )
  start_time = ymd_hms("2010-01-01 00:00:00") # create an "inital time". Sean starts time here.
  sd = update(start_time, seconds = ncvar_get(nc_data, "PRODUCT/time") ) # According to Panoply array is single-valued (has one entry)
  loc_inds = which((lon > lon_lb) & (lon < lon_ub) & (lat > lat_lb) & (lat < lat_ub))
  if(length(loc_inds) > 0){
    print(paste0(file_name, " has valid observations"))
    OBS_EXIST = TRUE
    num_locs = length(loc_inds)
    
    # This is my hack around the embedded for-loop Sean has
    # Specify the size of tm in the begining otherwise ludicrously expensive - even worse!
    # use a dummy_index to access the correct entry of tm, iterate it at end of outter loop
    tm = rep(NA, length.out = length(lat))
    end_range = dim(ncvar_get(nc_data, "PRODUCT/latitude"))[1]
    dummy_index = 0
    for(t in ncvar_get(nc_data, "PRODUCT/delta_time")){
      for(i in 1:end_range){
        tm[dummy_index*end_range + i] = update(sd, seconds = as.integer(t)/1e3) # stored as integers. access with as_datetime()
      }
      dummy_index = dummy_index + 1
    }
    
    
    # prepare parts for naming the file. Although not sure if this is legacy from Sean's v1 because it doesn't seem to do much
    data_inital_time = as_datetime(tm[1])
    data_ending_time = as_datetime(tm[length(tm)])
    initial_time_string = paste(year(data_inital_time), month(data_inital_time), mday(data_inital_time), sep="_")
    ending_time_string = paste(year(data_ending_time), month(data_ending_time), mday(data_ending_time), sep="_")
    fname = paste0("tropomi_samples_", region_name, "_", initial_time_string, "_", ending_time_string, ".Rdata") # save as .Rdata for now.
    
    # if fname is already a file delete it
    if(file.exists(fname)){unlink(fname)}
    # Doesn't like it when I include PRODUCT/.../ GEOLOCATIONS
    lat_e = ncvar_get(nc_data, "GEOLOCATIONS/latitude_bounds")
    lon_e = ncvar_get(nc_data, "GEOLOCATIONS/longitude_bounds")
    
    # LB: reading in variables names in. python reverses the order of the dimensions compared to R. R goes smallest to largest.
    # So here, we read in slices of 3d tensors (lat_e, lon_e), flatten them, and select the locations that have valid entries w/ loc_inds
    # loc_inds should work here b/c lat etc. are falttened same way with c() in column-major format.
    # var_dict_new$latitude_ll = c(lat_e[1,,])[loc_inds]
    # var_dict_new$longitude_ll = c(lon_e[1,,])[loc_inds]
    # var_dict_new$latitude_lr = c(lat_e[2,,])[loc_inds]
    # var_dict_new$longitude_lr = c(lon_e[2,,])[loc_inds]
    # var_dict_new$latitude_ur = c(lat_e[3,,])[loc_inds]
    # var_dict_new$longitude_ur = c(lon_e[3,,])[loc_inds]
    # var_dict_new$latitude_ul = c(lat_e[4,,])[loc_inds]
    # var_dict_new$longitude_ul = c(lon_e[4,,])[loc_inds]
    
    for(v in vars_to_sample){
      split_v_string = strsplit(v, "/")
      v_trunc = split_v_string[[1]][length(split_v_string[[1]][] ) ] # access the word after late / in v. aka which variable name
      if(v %in% c("PRODUCT/time_utc","GEOLOCATIONS/latitude_bounds", "GEOLOCATIONS/longitude_bounds") ){ next }
      if(v == "PRODUCT/time"){
        # following replicates behavior of in-line python for-loop
        tmp_time_in_seconds_vec = rep(NA, length.out = num_locs)
        tm_loc_inds = na.omit(tm[loc_inds])
        for(s in 1:num_locs){
          t = as_datetime(tm_loc_inds[s])
          time_interval_obj = interval(start_time, t)
          tmp_time_in_seconds_vec[s] = time_interval_obj@.Data
        }
        var_dict_new[[v_trunc]] = append(var_dict_new[[v_trunc]], tmp_time_in_seconds_vec)
      } else if (v %in% c("INPUT_DATA/northward_wind", "INPUT_DATA/eastward_wind")) {
        check_wind <- tryCatch(
          { wind <- c(ncvar_get(nc_data, v));
          var_dict_new[[v_trunc]] <- append(var_dict_new[[v_trunc]], wind[loc_inds]); },
          error = function(e) {
            print("No wind data found.");
            return(NULL)
          }
        )
        if (is.null(check_wind)) {
          z <- rep(999, num_locs);
          var_dict_new[[v_trunc]] <- append(var_dict_new[[v_trunc]], z);
        }
      } else {
        sampled_var = c(ncvar_get(nc_data, v))
        var_dict_new[[v_trunc]] = append(var_dict_new[[v_trunc]], sampled_var[loc_inds])
      }
    }
  }
  counter <- counter + 1
} # End outter for-loop

if(!OBS_EXIST){
  print("No valid observations founds!")
}

## write var_dict_new to .Rdata object.
save(var_dict_new, file = outfile_name)
