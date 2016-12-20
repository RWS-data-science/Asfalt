# THIS ROUTINE GENERATES MAPS OF DAMAGING ROAD CONDITIONS
# WARNING THIS IS A -- BIG -- OPERATION

# It takes historical GMS data
# Extracts damaging conditions per year
# And plots it to a map
# Allowing you to lookup damaging conditions per year for coordinates
library(plyr)
library(XML)

## 0 Parameters
MIN_CONSECUTIVE_MEASUREMENTS_FOR_FREEZING = 3 #Set the minium period that road surface should be below a temprature to be frozen (one measurement is 5 minutes)
FREEZE_THRESHOLD_1 = 0  #pure water freezes
FREEZE_THRESHOLD_2 = -2 #water in salty environment freezes
FREEZE_THRESHOLD_3 = -10#water freezes even if the road has been salted
MELTING_THRESHOLD  = 50 #Tarmac liquifies
YEAR_START_MONTH = 11 #Inspections are usually in oktober, first freezes are in december
FILE_COUNTER_PARAMETER = 10000 # Save every 10000 iterations
 
#Then we setup the data frame to accept the new projections
gms_measurement_features = data.frame(date = c(0), period = c(0), loc_nr = c(0), freeze_count_1 = c(0), freeze_count_2 = c(0), freeze_count_3 = c(0), melt_minutes = c(0), freeze_pattern_count = c(0))

### 2 Load the GMS sensor data
# #first we start at the seed-dirs and we extract all the dirs recursively
dirs = list.dirs('data/input/GMS/sensordata') 
# then we extract all XML files from these dirs
file_counter = 0
for (dir in dirs) {
  print(dir)
  files <- list.files(dir, pattern = "\\.zip$") 
  for( file in files) {
    file_counter = file_counter + 1
    # START MEASUREMENT SERIES ANALYSIS
    # Takes a day worth of measurements and reconstructs it as a feature vector
    #TODO: Improve serie measurements1
    #NOTE: Days are stored per file, we measure series of freezes per day. This might lead to inconsistencies in series
    xml_file = unzip(paste(c(dir, file), collapse='/'), exdir="data/temp")
    xml = xmlParse(xml_file)
    cur_measurement_date = as.character.Date(xpathApply(xml, "//GMS_METINGEN/@stop_date"))
    cur_period = as.character(as.numeric(strftime(cur_measurement_date, "%Y")) + if(as.numeric(strftime(cur_measurement_date, "%m")) > 10) 1 else 0)
    road_tempratures = as.numeric(xpathApply(xml, "//GMS_METING[@type='road_temp']/@val"))
    road_conductivities = as.numeric(xpathApply(xml, "//GMS_METING[@type='road_cond']/@val"))
    if(length(road_tempratures)>0 && length(road_conductivities)>0){
      cur_measurement_dates = as.POSIXct(as.character.Date(xpathApply(xml, "//GMS_METING[@type='road_temp']/@date")))
      cur_measure_station = as.numeric(xpathApply(xml, "//GMS_METINGEN/@ms_id"))
      road_temprature_measurements = data.frame(road_tempratures, cur_measurement_dates)
      road_temprature_measurements = aggregate(road_tempratures ~ cur_measurement_dates, FUN = median, data= road_temprature_measurements)
      
      cur_measurement_dates = as.POSIXct(as.character.Date(xpathApply(xml, "//GMS_METING[@type='road_cond']/@date")))
      road_conductivity_measurements = data.frame(road_conductivities, cur_measurement_dates)
      road_conductivity_measurements = aggregate(road_conductivities ~ cur_measurement_dates, FUN = median, data= road_conductivity_measurements)
      
      freeze_threshold_1_recorder = freeze_threshold_2_recorder = freeze_threshold_3_recorder = 0
      cur_freeze_1_count = cur_freeze_2_count = cur_freeze_3_count = cur_melt_minutes =  freeze_pattern_count = 0
      for(row in 1:nrow(road_temprature_measurements)){
        if(row+2 <= nrow(road_temprature_measurements)){
        if((road_temprature_measurements[row + 2, 2] < 0)){
          if((road_conductivity_measurements[row + 2,2] < 150)){
            #If temp under 0 quickly rises
            if((road_temprature_measurements[row, 2] + 0.2 < road_temprature_measurements[row + 2, 2]) ){
             #and conductivity quickly falls
              if((road_conductivity_measurements[row,2] > road_conductivity_measurements[row + 2,2] * 1.5)){
                freeze_pattern_count = freeze_pattern_count + 1       
              }
            }
          }
        }}
          
        
        cur_mean_road_temp = road_temprature_measurements[row, 2]

        if(cur_mean_road_temp >= MELTING_THRESHOLD) {
          cur_melt_minutes = cur_melt_minutes + 5
          print(paste("LOG: ", MELTING_THRESHOLD, cur_measure_station, cur_measurement_date ))
        }
        if(cur_mean_road_temp < FREEZE_THRESHOLD_1) {

          freeze_threshold_1_recorder = freeze_threshold_1_recorder + 1
        } else {
          if (freeze_threshold_1_recorder >= MIN_CONSECUTIVE_MEASUREMENTS_FOR_FREEZING){ # The end of a freezing period longer than threshold
            cur_freeze_1_count = cur_freeze_1_count + 1
          }
          freeze_threshold_1_recorder = 0
        }
        if(cur_mean_road_temp < FREEZE_THRESHOLD_2) {
          freeze_threshold_2_recorder = freeze_threshold_2_recorder + 1
        } else {
          if (freeze_threshold_2_recorder >= MIN_CONSECUTIVE_MEASUREMENTS_FOR_FREEZING){ # The end of a freezing period longer than threshold
            cur_freeze_2_count = cur_freeze_2_count + 1
          }
          freeze_threshold_2_recorder = 0
        }
        if(cur_mean_road_temp < FREEZE_THRESHOLD_3) {
          freeze_threshold_3_recorder = freeze_threshold_3_recorder + 1
        } else {
          if (freeze_threshold_3_recorder >= MIN_CONSECUTIVE_MEASUREMENTS_FOR_FREEZING){ # The end of a freezing period longer than threshold
            cur_freeze_3_count = cur_freeze_3_count + 1
            print(paste("LOG: ", FREEZE_THRESHOLD_3 , cur_measure_station, cur_measurement_date ))
          }
          freeze_threshold_3_recorder = 0
        }
      }#END OF MEASUREMENT SERIES LOOP
      gms_measurement_features[nrow(gms_measurement_features) + 1,]<-c(cur_measurement_date, as.numeric(cur_period),cur_measure_station, cur_freeze_1_count, cur_freeze_2_count, cur_freeze_3_count, cur_melt_minutes, freeze_pattern_count)
    }
    unlink(xml_file)
    if(file_counter %% (FILE_COUNTER_PARAMETER / 10)==0){
      print(file_counter)
    }
    if( file_counter %% FILE_COUNTER_PARAMETER == 0){
      print(paste("LOG: @ file ", file_counter))
      #saving a temporary backup copy
      save(gms_measurement_features,file=paste("data/temp/gms_measurement_features", "_",cur_measure_station, "_", strftime(cur_measurement_date, "%Y%m%d"), "_", file_counter,".RData",sep=""))
    }
    
  } #END OF FILE LOOP
  
}#END OF DIRECTORY LOOP
print(file_counter)
save(gms_measurement_features,file=paste("data/input/GMS/gms_measurement_features", "_",cur_measure_station, "_", strftime(cur_measurement_date, "%Y%m%d"), "_", file_counter,".RData",sep=""))
