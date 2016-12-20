# THIS ROUTINE GENERATES MAPS OF DAMAGING ROAD CONDITIONS
# It takes historical GMS data
# Extracts damaging conditions per year
# And plots it to a map
# Allowing you to lookup damaging conditions per year for coordinates
library(rgdal)
library(dplyr)

## 1 Load the featurematrix that was created from the GMS collection 
print("1. Loading the preconstructed feature matrix")
load("data/input/GMS/gms_measurement_features_1501_20161106_781593.RData")

## 2 Now we clean the data
print("2. Cleaning the data")
gms_measurement_features_cleaned <- gms_measurement_features
distribution_per_year <- aggregate(gms_measurement_features_cleaned[,0:1], by = list(period = gms_measurement_features_cleaned$period), FUN = length)
distribution_per_year_and_sensor <- aggregate(date~ period+loc_nr,gms_measurement_features_cleaned, FUN = length)

# first we toss out all years with less than 95000 measurements, because they are most likely broken years
gms_measurement_features_cleaned =  gms_measurement_features_cleaned[ gms_measurement_features_cleaned$period %in% distribution_per_year[distribution_per_year[,2]>95000,]$period, ]
#next we toss out all sensors that didn't record alot of the time in a year, due to being unreliable
#NOTE we relaxed this a bit to save 2015: from 2015-06-17 to 16 - 08 -2015 no recording has been found
gms_measurement_features_cleaned = anti_join(gms_measurement_features_cleaned,distribution_per_year_and_sensor[distribution_per_year_and_sensor[,3] < 290,], by = c("period", "loc_nr") )

# Now we aggregate the measurements to years
class(gms_measurement_features_cleaned$loc_nr)<- class(gms_measurement_features_cleaned$freeze_count_1)<- class(gms_measurement_features_cleaned$freeze_count_2)<- class(gms_measurement_features_cleaned$freeze_count_3)<- class(gms_measurement_features_cleaned$melt_minutes)<- class(gms_measurement_features_cleaned$freeze_pattern_count) <- "numeric"
gms_measurement_features_aggregated = aggregate(cbind(freeze_count_1, freeze_count_2, freeze_count_3, melt_minutes, freeze_pattern_count) ~ period+loc_nr, gms_measurement_features_cleaned, FUN = sum)

#now we prune outliers in freeze_count_1 : more than 3 standard deviations are discarded
for(cur_period in unique(gms_measurement_features_aggregated[,1])){
  sub_set = subset(gms_measurement_features_aggregated, period = cur_period)
  prune_set = subset(sub_set, abs(freeze_count_1 - mean(freeze_count_1)) > 3 * sd(freeze_count_1) ) 
  gms_measurement_features_aggregated = anti_join(gms_measurement_features_aggregated, prune_set, by = c("period", "loc_nr"))
}

#now we prune outliers in melt_minutes : more than 3 standard deviations are discarded
for(cur_period in unique(gms_measurement_features_aggregated[,1])){
  sub_set = subset(gms_measurement_features_aggregated, period = cur_period)
  prune_set = subset(sub_set, abs(melt_minutes - mean(melt_minutes)) > 6 * sd(melt_minutes) ) 
  gms_measurement_features_aggregated = anti_join(gms_measurement_features_aggregated, prune_set, by = c("period", "loc_nr"))
}
rm(sub_set, prune_set, cur_period)

## 3 We paste the features to a dataframe of known locations per sensor station to create a map
print("3. Constructing the map")
# First we build a matrix of stations with their locations
gms_locations <- read.csv(file = "data/input/GMS/LocatiesGMSstations.csv")
gms_locations <- gms_locations[complete.cases(gms_locations),]

#next we project the locations to RD format
coordinates(gms_locations)<- ~loc_lon + loc_lat
proj4string(gms_locations)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"
rd<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs" 
gms_locations<-spTransform(gms_locations,rd)
gms_locations<-as.data.frame(gms_locations)
gms_locations_measurement_features = merge(gms_locations, gms_measurement_features_aggregated, by.gms_measurement_features_aggregated = "loc_nr", by.gms_locations = "loc_nr")

## 4 Now we clean the workspace
print("4. Cleaning the workspace")
rm(rd)
rm(distribution_per_year_and_sensor, distribution_per_year)
rm(gms_locations)
rm(gms_measurement_features_aggregated, gms_measurement_features_cleaned, gms_measurement_features)

#and save the matrix
save(gms_locations_measurement_features, file="data/output/gms_locations_measurement_features.Rdata")