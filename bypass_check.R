#All of the events used for the GI analysis as of PWEA conference in june 2024
eventfile <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/2024-06-10/gi_metrics_5min.csv"

##### 0.1 packages #####
library(tidyverse)
library(odbc)
library(DBI)
library(pwdgsi)
library(lubridate)
library(openxlsx)
library(RPostgreSQL)
library(RPostgres)
library(sf)
library(mapsf)


# not in operator
`%!in%` <- Negate(`%in%`)

mars_con <- odbc::dbConnect(odbc::odbc(), "mars14_datav2")

localfolder <- "C:/Users/mars_db/Documents/github/green-inlet-monitoring-main/plots/"
dayfolder <- paste0(localfolder, today())
dir.create(dayfolder, recursive = TRUE, showWarnings = FALSE)

#All of the events for green inlets
events <- read_csv(eventfile) %>%
  select(-bypass_length) %>% #This is Brian's old bypass length calculation that we need to verify/replace with new methods
  arrange(ow_uid, radar_event_uid)

# Grab Overtopping elevations for each inlet
#fieldwork.viw_ow_plus_measurements.deployment_depth_ft is what brian uses for GI overflow elev in his calculations
overtopping_elev <- dbGetQuery(mars_con, paste0("SELECT ow_uid, well_depth_ft, deployment_depth_ft
                                 FROM fieldwork.viw_ow_plus_measurements WHERE ow_uid in (",
                                                paste(unique(events$ow_uid), collapse= ", "),") AND
                                                 end_dtime_est IS NULL"))

#Join all the things
db_events <- dbGetQuery(mars_con, "select * from data.tbl_radar_event") %>% 
  filter(radar_event_uid %in% events$radar_event_uid)

rain_events <- left_join(events, db_events, by = "radar_event_uid") %>%
  left_join(overtopping_elev) %>%
  arrange(ow_uid, radar_event_uid)

#calculating bypass duration in the loop below (time that level > overflow elevation)
bypass_estimates <- head(rain_events, 0) %>%
                    transmute(ow_uid, radar_event_uid, smp_id, ow_suffix, 
                           bypass_bool = NA,
                           bypass_steps = NA_integer_, #timesteps where level > overflow
                           step_duration = NA_character_, #length of timestep from padr::get_interval (should equal 5 mins or 15 mins, trust no one)
                           bypass_mins = NA_integer_) #Length of bypass, in minutes, via lubridate::as_duration 

#create output file
if(!file.exists(paste0(dayfolder, "/bypass_lengths.csv"))){
  write_csv(bypass_estimates, file = paste0(dayfolder, "/bypass_lengths.csv"))
}

for(i in 1:nrow(rain_events)){
#for(i in 34){
  print(paste(rain_events$smp_id[i], 
              rain_events$ow_suffix[i],
              "radar grid cell:", rain_events$radar_uid[i],
              "radar event UID:", rain_events$radar_event_uid[i],
              "start:", as.Date(rain_events$eventdatastart_edt)[i],
              "end:", as.Date(rain_events$eventdataend_edt)[i]))
  
  #something is wrong with the rainfall plotting. Some data appears missing from the database?
    #eg radar_uid       100445
    #   radar_event_uid 265264
    #   start date      2021-02-26
    #   end date        2021-02-27
  #The record exists in tbl_radar_event and exists in Brian's previous records
  #But the data is not there now. TODO to investigate this
  #For now, plot the level series without the rainfall series for spot checking of overtopping values.
  
  level_data <- marsFetchLevelData(con = mars_con, 
                                        target_id = rain_events$smp_id[i], 
                                        ow_suffix = rain_events$ow_suffix[i], 
                                        start_date = as.Date(rain_events$eventdatastart_edt)[i], 
                                        end_date = as.Date(rain_events$eventdataend_edt)[i], 
                                        sump_correct = TRUE)
  
  event_bool <- marsOvertoppingCheck_bool(level_data$level_ft, rain_events$deployment_depth_ft[i])
  
  #Calculate the number of steps where sensor level > overflow elevation
  event_steps <- sum(level_data$level_ft >= rain_events$deployment_depth_ft[i])
  
  #how long were those steps?
  event_interval = padr::get_interval(level_data$dtime_est)

  #How many minutes of bypass?
  event_bypass_mins <- lubridate::as.duration(event_interval) %>%
    as.numeric("minutes") %>% #lubridate extension to as.numeric. See ?lubridate::as.duration for more info.
    {. * event_steps} #Multiply by the count of overflow timesteps. {} are an anonymous function.
  
  bypass_check <- rain_events[i, ] %>%
    transmute(ow_uid, radar_event_uid, smp_id, ow_suffix, 
              bypass_bool = event_bool,
              bypass_steps = event_steps, 
              step_duration = event_interval,
              bypass_mins = event_bypass_mins)
              
  # eventplot <- marsWaterLevelPlot(event = rain_events$radar_event_uid[i],
  #                               structure_name = paste(rain_events$smp_id[i], rain_events$ow_suffix[i]),
  #                               obs_datetime = level_data$dtime_est,
  #                               obs_level_ft = level_data$level_ft,
  #                               storage_depth_ft = rain_events$deployment_depth_ft[i])
  
  output_basename <- paste(bypass_estimates$smp_id[i], 
                           bypass_estimates$ow_suffix[i], 
                           "bypass", 
                           paste0(sprintf("%05d", event_bypass_mins), "mins"), #print the bypass times in sortable format,
                           "event",
                           rain_events$radar_event_uid[i],
                           sep = "_")
                    
  output_filename <- paste0(dayfolder, "/", output_basename, ".png")
  # ggsave(filename = output_filename,
  #        plot = eventplot,
  #        width = 10,
  #        height = 8,
  #        units = "in")
  
  #write_csv(bypass_check, file = paste0(dayfolder, "/bypass_lengths.csv"), append = TRUE)
  
  bypass_estimates <- rbind(bypass_estimates, bypass_check)
}

#Spot checking my results vs Brian's results
bypass_06_10 <- select(events, ow_uid, radar_event_uid, smp_id, ow_suffix, bypass_06_10 = overtop)

#Running ths same script from 6/10, but doing so today by me
bypass_12_30 <- read_csv("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/2024-12-30/gi_metrics_5min.csv") %>%
  mutate(bypass_12_30 = overtop) %>%
  select(-overtop)

spot_check <- left_join(bypass_06_10, bypass_12_30) %>%
  left_join(bypass_estimates)

disagreements <- filter(spot_check, bypass_12_30 != bypass_bool | bypass_06_10 != bypass_12_30 | bypass_bool != bypass_06_10)
