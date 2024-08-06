#### 0.0 Headings and basic script set up ####
#### Green Inlet Monitoring
#### Data gathering
#### Written by : Brian Cruice
#### Written on: 12/29/2022
#### Last Run: 04/03/2024

##### 0.1 packages #####
library(tidyverse)
library(odbc)
library(DBI)
library(pwdgsi)
library(lubridate)
library(xlsx)
library(RPostgreSQL)
library(RPostgres)
library(sf)
library(mapsf)


# not in operator
`%!in%` <- Negate(`%in%`)

#smp to system fx
smp_2_sys <- function(smp_id){
  x <- str_split(smp_id, pattern = "-") %>% unlist()
  sys_id <- paste0(x[1],"-",x[2])
  return(sys_id)
}


##### 0.2 connect #####
cw_con <- odbc::dbConnect(odbc::odbc(),"CityWorks",uid = "cwread", pwd = "readcw", database = "PWD_Cityworks")
mars_con <- odbc::dbConnect(odbc::odbc(), "mars14_datav2")

####  1.0 Basic Data Set UP ####
##### 1.1 Load Spreadsheet data from project folder #####

# Systems within the study
systems <- c('171-1', '171-2', '1-3', '1-1', '1006-1', '179-5','488-5', '439-1')



# Manual list of pipe runs immediately connected to green inlets
folderpath <-  "//pwdoows//OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis"
piperuns <- read.csv(paste0(folderpath,"/","pipe_run.csv"))

# System characteristics
sys_char_file <- paste0(folderpath,"/SystemCharacteristics.xlsx")
sys_char <- xlsx::read.xlsx(file = sys_char_file,
                            sheetName = "Characteristics")


# As-built elevations
asbuilt_file <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/relative_elevations.xlsx"
asbuilt_elev <- xlsx::read.xlsx(file = asbuilt_file, sheetName = "elev")

##### 1.2 Query MARS DB data #####
ow_query <- paste0("WITH sys as (SELECT *, admin.fun_smp_to_system(smp_id) as system_id  FROM fieldwork.tbl_ow)
             SELECT * FROM sys WHERE system_id IN ('",paste(systems, collapse = "', '"),"')")
ow <- dbGetQuery(mars_con, ow_query)


# Grab componet ID's associated with these systems

asset_query <- paste0("SELECT * FROM external.mat_assets WHERE system_id IN ('",paste(systems, collapse = "', '"),"')")

assets <- dbGetQuery(mars_con, asset_query)
  
# subset of GI ow_uid's
gi_ow <- ow[grepl("GI", ow$ow_suffix),]

# add assets to gi_ow
ow <- ow %>% left_join(dplyr::select(assets, facility_id, component_id, asset_type), by = "facility_id")
ow <- ow %>% mutate(temp = paste0(system_id, ow_suffix))

well_list <- c(paste0(sys_char$system_id,sys_char$OW), paste0(sys_char$system_id,sys_char$GI))

ow <- ow %>% filter(temp %in% well_list) %>% dplyr::select(-temp)


# Find a beginning date and cut-off date for monitoring/work order
begin_date <- dbGetQuery(mars_con,
                         paste0("SELECT MIN(dtime_est) FROM data.tbl_ow_leveldata_raw WHERE ow_uid IN (",
                                paste(gi_ow$ow_uid, collapse = ", "),")"))[1,1] %>% as.Date()
end_date <- dbGetQuery(mars_con,
                       paste0("SELECT MAX(dtime_est) FROM data.tbl_ow_leveldata_raw WHERE ow_uid IN (",
                              paste(gi_ow$ow_uid, collapse = ", "),")"))[1,1] %>% as.Date()

#### 2.0 CITYWORKS ####
# Add (american) brackets to the facility ID's from MARS database
facility_ids <- paste("('{",paste(ow$facility_id, collapse = "}', '{"),"}')", sep = "")

# Repeat for all assets, including fittings
facility_asset_ids <- paste("('{",paste(assets$facility_id, collapse = "}', '{"),"}')", sep = "")

# For inlet/ow only
wo_query <- paste0("SELECT * FROM Azteca.WORKORDER wo LEFT JOIN Azteca.WORKORDERENTITY woe ON
    wo.WORKORDERID = woe.WORKORDERID WHERE
    woe.ENTITYUID IN ", facility_ids)

wo_asset_query <- paste0("SELECT * FROM Azteca.WORKORDER wo LEFT JOIN Azteca.WORKORDERENTITY woe ON
    wo.WORKORDERID = woe.WORKORDERID WHERE
    woe.ENTITYUID IN ", facility_asset_ids)

# double check for subsurf inspection/maint wo's
subsurf_maint_query <- paste0("SELECT * FROM Azteca.WORKORDER wo LEFT JOIN Azteca.WORKORDERENTITY woe ON
    wo.WORKORDERID = woe.WORKORDERID WHERE DESCRIPTION = 'SUB-SURFACE MAINTENANCE' OR DESCRIPTION = 'SUBSURFACE INSPECTION'" )


subsurf_workorders <- dbGetQuery(cw_con, subsurf_maint_query)
# Remove duplicates
subsurf_workorders <- subsurf_workorders[!duplicated(colnames(subsurf_workorders))]


##### 2.1 Grab filter bag data #####

# Thank you for the code, Farshad
#get the smps
dataconv <- dbConnect(odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = "pwdgissql",
                      Database = "DataConv",
                      uid = "gisread",
                      pwd = "gisread")

# Inlet data
gi_inlets <- dbGetQuery(dataconv, "select * from gisad.GSWIINLET")
gi_obs_wells <- dbGetQuery(dataconv, "select * from gisad.GSWIOBSERVATIONWELL")


gi_inlets %<>% dplyr::filter(FACILITYID %in% assets$facility_id)

# check if locations match pipe fittings
feature_ids <- c(piperuns$Pipe.Run.Asset.ID, piperuns$Attached.Asset)
subsurf_workorders <- subsurf_workorders %>% dplyr::filter(ENTITYUID %in% feature_ids | FEATUREUID %in% feature_ids)


gi_workorders <- dbGetQuery(cw_con, wo_query)
gi_asset_workorders <- dbGetQuery(cw_con, wo_asset_query)

# Remove duplicates
gi_workorders <- gi_workorders[!duplicated(colnames(gi_workorders))]
gi_asset_workorders <- gi_asset_workorders[!duplicated(colnames(gi_asset_workorders))]

# Limit to monitoring period
gi_workorders <- gi_workorders %>% dplyr::filter(ACTUALSTARTDATE >= begin_date) %>%
                  dplyr::filter(ACTUALSTARTDATE <= end_date)

gi_asset_workorders <- gi_asset_workorders %>% dplyr::filter(ACTUALSTARTDATE >= begin_date) %>%
  dplyr::filter(ACTUALSTARTDATE <= end_date)


# Frequency of "SURFACE INLET PROTECTION MAINTENANCE" for each inlet



in_prot_maint <- gi_workorders %>% dplyr::filter(DESCRIPTION == "SURFACE INLET PROTECTION MAINTENANCE") %>%
                 # strip curly brackets to associate facility id
                 dplyr::mutate(facility_id = toupper(as.character(gsub("\\{|\\}","",FEATUREUID)))) %>%
                 # associate with facility id
                 dplyr::left_join(assets, by = "facility_id")

wo_in_prot_plot <- ggplot(data = in_prot_maint, aes(x = component_id)) + geom_bar() + ggtitle('Count of "Surface Inlet Protection Maintenance" Work Orders') +
  ylab("Count of Work Orders") + xlab("Inlet Componenet ID") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1))

ggsave(plot = wo_in_prot_plot, file = paste0(folderpath,"/Preliminary Visualizations/Inlet_Protection_WO_frequency.png"), width = 8, height = 4.5)
write.csv(in_prot_maint,
          file = paste0(folderpath, "/inlet_prot_wos.csv"))

##### 2.2 Work order types of interests #####
# (created by Johanna on 1/11/2023)
descriptions <- c("SUBSURFACE INSPECTION",
                  "SUB-SURFACE MAINTENANCE",
                  "DRAINAGE MODIFICATION",
                  "SUB-SURFACE INLET CLEANING",
                  "SUB-SURFACE INLET PROTECTION MAINTENANCE",
                  "IC - INLET EXAM",
                  "INLET MAINTENANCE",
                  "INLET EXAM",
                  "SURFACE INLET PROTECTION MAINTENANCE")

gi_workorders <- gi_workorders %>% dplyr::filter(DESCRIPTION %in% descriptions)
gi_asset_workorders <- gi_asset_workorders %>% dplyr::filter(DESCRIPTION %in% descriptions)


##### 2.3 Write workorder results and plots #####

# Write .csv of both sets of work orders
write.csv(gi_asset_workorders,
          file = paste0(folderpath, "/gi_workorders.csv"))
write.csv(subsurf_workorders,
          file = paste0(folderpath, "/subsurface_workorders.csv"))

# bind the two together for plotting
binded <- rbind(gi_asset_workorders,subsurf_workorders) #%>%
          # turn feature id format into facility id format
          # dplyr::mutate(facility_id = toupper(as.character(gsub("\\{|\\}","",FEATUREUID)))) %>%
          # associate with facility id
          # dplyr::inner_join(assets, by = "facility_id")


##### 2.4 Plot results #####

gi_wo_plot <- ggplot(data = binded, aes(x = DESCRIPTION)) + geom_bar() + ggtitle('Count of Reviewed Work Orders') +
  ylab("Count of Work Orders") + xlab("WO Description") +
  theme(axis.text.x = element_text(angle = 50, vjust = 1.0, hjust=1))

gi_wo_plot
ggsave(plot = gi_wo_plot, file = paste0(folderpath,"/Preliminary Visualizations/WO_reviewed_plot.png"), width = 8, height = 4.5)

#### 3.0 Calculate storm event, overtopping Summary Metrics ####

# Complete log of storms for each ow

##### 3.1 Read latest gi_metrics from last run of the script ####
folders <- list.files(folderpath)
folders <- folders[!grepl(pattern = "\\.",folders)]
# read the data from the most recent run of this script
last_run_date <- NA
for(i in 1:length(folders)){
  if(try(as.Date(folders[i]), silent = TRUE) %>% is.Date()){
    # set new date
    new_date <- as.Date(folders[i])
    
    # usurp if latest date
    last_run_date <- max(c(new_date, last_run_date), na.rm = TRUE)
  }
}

latest_data <- read.csv(paste0(folderpath,"/",last_run_date,"/gi_metrics_5min.csv"), row.names = NULL)


# grab list of storm events
  # modified code chunk from SC analysis
  # Get the SC data
  sc_batch_newcat <- dbGetQuery(mars_con, "SELECT * from metrics.tbl_smp_sc_category") 
  rain_radar_event <- dbGetQuery(mars_con, "SELECT * FROM data.tbl_radar_event")
  smp_radar <- dbGetQuery(mars_con, "SELECT * FROM admin.tbl_smp_radar")
  ow_leveldata_raw <- dbGetQuery(mars_con,
                                 paste0("SELECT * FROM data.tbl_ow_leveldata_raw WHERE ow_uid IN (",paste(ow$ow_uid,collapse = ", "),")"))
  smp_resolution_date <- dbGetQuery(mars_con, "SELECT * from metrics.tbl_sc_addressed_date") 
  
  
  
  #Kludge: ow_leveldata_raw coerced to 00 seconds instead of 59 seconds for the inner_join
  ow_leveldata_raw$dtime_est  <- ow_leveldata_raw$dtime_est %>% lubridate::round_date(unit = "minute")
  
  
  # Create a table with smp_id, ow_suffix, and rainevent_uid
  ow_radar_events <- sc_batch_newcat %>%
    inner_join(ow, by="smp_id") %>%
    inner_join(smp_radar, by="smp_id") %>% 
    inner_join(rain_radar_event, by= "radar_uid") %>%
    inner_join(ow_leveldata_raw, by=c("ow_uid"="ow_uid","eventdatastart_edt"="dtime_est"))%>%
    select(smp_id,ow_uid, ow_suffix,radar_event_uid, batch_uid,eventdatastart_edt) 
  
  
  # Create summary table of rainfall events for Green Inlets
  ow_event_vals <- ow_radar_events %>% dplyr::filter(ow_uid %in% gi_ow$ow_uid) %>%
                                       dplyr::left_join(rain_radar_event, by = "radar_event_uid")
  
  event_summary_table <- ow_event_vals %>% group_by(ow_uid) %>% dplyr::summarize(avg_eventdepth_in = round(mean(eventdepth_in),4),
                                                          max_eventdepth_in = round(max(eventdepth_in),4),
                                                          max_eventavgintensity_inhr = round(max(eventavgintensity_inhr),4),
                                                          total_events = n()) %>%
                                          dplyr::left_join(gi_ow, by = "ow_uid")
  
  # Write summary table
  write.csv(event_summary_table, paste0(folderpath,"/green_inlet_event_summary.csv"))
  
  # Grab Overtopping elevations for each inlet
  overtopping_elev <- dbGetQuery(mars_con, paste0("SELECT ow_uid, smp_id, ow_suffix, well_depth_ft, deployment_depth_ft
                                 FROM fieldwork.viw_ow_plus_measurements WHERE ow_uid in (",
                                                 paste(gi_ow$ow_uid, collapse= ", "),") AND
                                                 end_dtime_est IS NULL"))

  # Grab measurements for all ows
  well_meas <- dbGetQuery(mars_con, paste0("SELECT * FROM fieldwork.viw_ow_plus_measurements WHERE ow_uid in (",
                                           paste(ow$ow_uid, collapse= ", "),")")) %>%
               dplyr::filter(ow_suffix == "OW1" | ow_suffix == "GI1" | ow_suffix == "GI2") %>%
               dplyr::filter(start_dtime_est < date('2023-01-01'))
  

  
  #Create error log
  error_log <- data.frame(ow_event_row = NA, error_message = NA, stringsAsFactors=FALSE)

  # Split OW and GI radar events
  ow_events <- ow_radar_events[grepl(ow_radar_events$ow_suffix,pattern = "OW"),]
  gi_events <- ow_radar_events[grepl(ow_radar_events$ow_suffix,pattern = "GI"),]

## write green inlet data
  
  #Create error log
  error_log <- data.frame(ow_event_row = NA, error_message = NA, stringsAsFactors=FALSE)
  
    
  #check for only new GI data
  
  #create unique identifier for ow-events
  ow_event_vals <- ow_event_vals %>% mutate(ow_event = paste0(ow_uid,"-",radar_event_uid))
  
  gi_events <- gi_events %>% mutate(ow_event = paste0(ow_uid,"-",radar_event_uid))
  
  latest_data <- latest_data %>% mutate(ow_event = paste0(ow_uid,"-",radar_event_uid))

  # Limit calculation of GI metrics to those not yet done 
  new_gi_events <- gi_events %>% dplyr::filter(ow_event %!in% latest_data$ow_event)
  
  ## Create directory if needed
  if(dir.exists(paste0(folderpath, "/", Sys.Date())) == FALSE){
     dir.create(paste0(folderpath, "/", Sys.Date())) 
  }
   
  
# Copy latest data over if necessary
  if(file.exists(paste0(folderpath, "/", Sys.Date(),"/gi_metrics_5min.csv")) == FALSE){
  file.copy(from = paste0(folderpath,"/",last_run_date,"/gi_metrics_5min.csv"),
            to = paste0(folderpath, "/", Sys.Date()))
  }
  

for(i in 1:nrow(new_gi_events)){
  
  tryCatch({
    
    ##### 3.2 Query rain, ow, and gi data from the database for select event ####
    temp_df <- new_gi_events[i,]
    
    rain_start_date <- rain_radar_event %>%
      filter(radar_event_uid == temp_df[1, "radar_event_uid"]) %>%
      select(eventdatastart_edt) %>%
      format("%Y-%m-%d") %>%
      pull %>% ymd
    
    rain_end_date <- rain_radar_event %>%
      filter(radar_event_uid == temp_df[1, "radar_event_uid"]) %>%
      select(eventdataend_edt) %>%
      format("%Y-%m-%d") %>%
      pull %>% ymd
    
    event <- temp_df %>%
      select(radar_event_uid) %>%
      pull
    
    structure_name <- paste(temp_df$smp_id, temp_df$ow_suffix)
    storage_depth <- overtopping_elev$deployment_depth_ft[overtopping_elev$ow_uid == temp_df$ow_uid]
    
    snapshot_date <- "today"
    snapshot <- marsFetchSMPSnapshot(con = mars_con, 
                                     smp_id = temp_df$smp_id, 
                                     ow_suffix = temp_df$ow_suffix, 
                                     request_date = snapshot_date)
    
    # storage_depth <- snapshot$storage_depth_ft
    orifice_height_ft <- snapshot$orificedepth_ft
    
    
    monitoringdata <- marsFetchMonitoringData(con = mars_con, 
                                              target_id = temp_df$smp_id, 
                                              ow_suffix = temp_df$ow_suffix, 
                                              source = "radar",
                                              start_date = as.character(rain_start_date), 
                                              end_date = as.character(rain_end_date), 
                                              sump_correct = TRUE,
                                              level = TRUE)
    
    ow_data <- marsFetchMonitoringData(con = mars_con, 
                                       target_id = temp_df$smp_id, 
                                       ow_suffix = "OW1", 
                                       source = "radar",
                                       start_date = as.character(rain_start_date), 
                                       end_date = as.character(rain_end_date), 
                                       sump_correct = TRUE,
                                       debug = TRUE,
                                       level = TRUE)
    
    ##### 3.3 Rearrange Data for Calculations #####
    
    rain_event_data <- monitoringdata[["Rain Event Data"]]
    rain_data <- monitoringdata[["Rainfall Data"]]
    level_data <- monitoringdata[["Level Data"]]
    ow_level_data <- ow_data[["Level Data"]]
    
    rain_plot_data <- monitoringdata[["Rainfall Data"]] %>%
      dplyr::filter(radar_event_uid == temp_df$radar_event_uid)
    
    rainfall_datetime	<- rain_plot_data$dtime_est
    rainfall_in <- rain_plot_data$rainfall_in
    
    obs_data <- dplyr::full_join(level_data, rain_data, 
                                 by = c("dtime_est", "radar_uid", "radar_event_uid")) %>% 
      dplyr::arrange(dtime_est) %>%
      dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
      dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., fromLast = TRUE))) # %>%
      # dplyr::mutate(orifice_outflow_ft3 = marsUnderdrainOutflow_cf(dtime_est = dtime_est,
      #                                                              waterlevel_ft = level_ft,
      #                                                              orifice_height_ft = snapshot$assumption_orificeheight_ft,
      #                                                              orifice_diam_in = snapshot$orifice_diam_in))
    selected_event <- obs_data %>%
      dplyr::filter(radar_event_uid == temp_df$radar_event_uid)
    
    ##### 3.4 Calculate Metrics #####
    
    #Draindown time
    draindown_hr <- marsDraindown_hr(dtime_est = selected_event$dtime_est,
                                     rainfall_in = selected_event$rainfall_in,
                                     waterlevel_ft = selected_event$level_ft)
    
    #Observed relative storage utilization
    percentstorageused_relative <- marsPeakStorage_percent(waterlevel_ft = selected_event$level_ft - dplyr::first(selected_event$level_ft), storage_depth_ft = snapshot$storage_depth_ft) %>% round(4) 
    
    # overtopping; Bypass Time Counting 
    overtop <- marsOvertoppingCheck_bool(selected_event$level_ft, storage_depth)
    
    if(overtop == TRUE){
      overtop_length <- max(max(selected_event$dtime_est[selected_event$level_ft > storage_depth]) -
                            min(selected_event$dtime_est[selected_event$level_ft > storage_depth]),
                            selected_event$dtime_est[2] - selected_event$dtime_est[1]
                            )
    } else {
      overtop_length <- 0
    }
     
    
    ##### 3.5 Head Differential Calculations #####
    
    #Head Differential
    peak_time <- dplyr::filter(level_data, level_ft == max(level_ft, na.rm = TRUE)) %>% dplyr::select(dtime_est) %>% pull
    peak_time <- peak_time[length(peak_time)]
    peak_gi_head <- dplyr::filter(level_data, level_ft == max(level_ft, na.rm = TRUE)) %>% dplyr::select(level_ft) %>% pull
    peak_gi_head <- peak_gi_head[length(peak_gi_head)]
    peak_ow_head <- dplyr::filter(ow_level_data, dtime_est == peak_time) %>% dplyr::select(level_ft) %>% pull
    peak_ow_head <- peak_ow_head[length(peak_ow_head)]
    head_dif_x <- peak_gi_head - peak_ow_head
    
    owpeak_time <- dplyr::filter(ow_level_data, level_ft == max(level_ft, na.rm = TRUE)) %>% dplyr::select(dtime_est) %>% pull
    owpeak_time <- owpeak_time[length(owpeak_time)]
    
    peak_lag <- owpeak_time - peak_time
    
    owpeak_gi_head <- dplyr::filter(level_data, dtime_est == owpeak_time) %>% dplyr::select(level_ft) %>% pull 
    owpeak_gi_head <- owpeak_gi_head[length(owpeak_gi_head)]
    owpeak_ow_head <- dplyr::filter(ow_level_data, level_ft == max(level_ft, na.rm = TRUE)) %>% dplyr::select(level_ft) %>% pull 
    owpeak_ow_head <- owpeak_ow_head[length(owpeak_ow_head)]
    owhead_dif_x <- owpeak_gi_head - owpeak_ow_head
    
    #Relative head differential
    # sensor heights
    ow_sensor_height <- asbuilt_elev$as_built_elev[asbuilt_elev$system_id == smp_2_sys(temp_df$smp_id) & asbuilt_elev$ow_suffix == "OW1"] -
                 well_meas$cap_to_hook_ft[well_meas$smp_id == temp_df$smp_id & well_meas$ow_suffix == "OW1"] -
                 well_meas$hook_to_sensor_ft[well_meas$smp_id == temp_df$smp_id & well_meas$ow_suffix == "OW1"]
    
    
    gi_sensor_height <- asbuilt_elev$as_built_elev[asbuilt_elev$system_id == smp_2_sys(temp_df$smp_id) & (asbuilt_elev$ow_suffix == "GI1"| asbuilt_elev$ow_suffix == "GI2")] -
                 well_meas$cap_to_hook_ft[well_meas$smp_id == temp_df$smp_id & (well_meas$ow_suffix == "GI1" | well_meas$ow_suffix == "GI2")] -
                 well_meas$hook_to_sensor_ft[well_meas$smp_id == temp_df$smp_id & (well_meas$ow_suffix == "GI1" | well_meas$ow_suffix == "GI2")]
    
    rel_head_dif_x <- (peak_gi_head + gi_sensor_height) - (peak_ow_head + ow_sensor_height)
    owrel_head_dif_x <- (owpeak_gi_head + gi_sensor_height) - (owpeak_ow_head + ow_sensor_height)
    
    if(length(head_dif_x) > 1)
    {
      head_dif_x <- mean(head_dif_x)
      rel_head_dif_x <- mean(rel_head_dif_x)
    }
    
    if(length(owhead_dif_x) > 1)
    {
     owhead_dif_x <- mean(owhead_dif_x)
     owrel_head_dif_x <- mean(owrel_head_dif_x)
    }
    
    #write metrics as CSV
    
    # populating ddown_error data
    if (draindown_hr >= 0){
      
      draindown_hr <- draindown_hr
      ddown_error <- NA
      
      
    } else {
      draindown_hr <- NA 
      ddown_error <- draindown_hr
      
    }
    
    metrics_output <- temp_df %>%
      select(ow_uid, radar_event_uid, smp_id, ow_suffix) %>%
      mutate(draindown_hr=draindown_hr,
             draindown_error=ddown_error,
             percentstorageused_relative=percentstorageused_relative,
             overtop= overtop,
             head_dif = head_dif_x,
             rel_head_dif = rel_head_dif_x,
             peak_gi_head = peak_gi_head,
             ow_head_gipeak = peak_ow_head,
             peak_ow_head = owpeak_ow_head,
             gi_head_owpeak = owpeak_gi_head,
             head_dif_owpeak = owhead_dif_x,
             rel_head_dif_owpeak = owrel_head_dif_x,
             ow_sensor_elev = ow_sensor_height,
             gi_sensor_elev = gi_sensor_height,
             peak_lag = peak_lag,
             bypass_length = overtop_length)

    write.table(metrics_output, file = paste0(folderpath, "/", Sys.Date(), "/gi_metrics_5min.csv"), sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
    
    #Manually managing memory just in case
    rm(obs_data)
    rm(level_data)
    
  }, error=function(e){
    error_log[1,1] <<- i
    error_log[1,2] <<- toString(conditionMessage(e))
    write.table(error_log, file = paste0(folderpath, "/", Sys.Date(), "/error_log.csv"), sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
    
  })
}
  
  
# read gi and ow metrics
  
gi_metrics <-  read.csv(paste0(folderpath, "/", Sys.Date(), "/gi_metrics_5min.csv"))


#### Compare to latest ow metrics ####

# This folder needs to be updated with the latest metrics

ow_folder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/48 Short-Circuiting GSI Data Analysis/Calculation Phase/Metrics Calculations/"

ow_folders <- list.files(ow_folder)
ow_folders <- ow_folders[!grepl(pattern = "\\.",ow_folders)]
# read the data from the most recent run of this script
# this should me moved into a fx at some point... migrate into SC analysis as well
last_run_date <- NA
for(i in 1:length(ow_folders)){
  if(try(as.Date(ow_folders[i]), silent = TRUE) %>% is.Date()){
    # set new date
    new_date <- as.Date(ow_folders[i])
    
    # usurp if latest date
    last_run_date <- max(c(new_date, last_run_date), na.rm = TRUE)
  }
}



ow_file <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/48 Short-Circuiting GSI Data Analysis/Calculation Phase/Metrics Calculations/2023-01-03/metrics.csv"
ow_metrics <-  read.csv(ow_file)


# filter ow_metrics
ow_metrics <- ow_metrics %>% dplyr::mutate(smp_radar_uid = paste(radar_event_uid,smp_id))
gi_metrics <- gi_metrics %>% dplyr::mutate(smp_radar_uid = paste(radar_event_uid,smp_id))

ow_metrics <- ow_metrics %>% dplyr::filter(smp_radar_uid %in% gi_metrics$smp_radar_uid)




# summarize

ow_summary <- ow_metrics %>% group_by(ow_uid) %>% summarize(n = n(),
                                                            overtopping_count = sum(overtop),
                                                            avg_RPSU = mean(percentstorageused_relative, na.rm = TRUE)) %>%
                             dplyr::left_join(ow_metrics, by = "ow_uid") %>%
                            dplyr::select(ow_uid,smp_id,ow_suffix,n, overtopping_count, avg_RPSU) %>%
                            unique() %>% dplyr::filter(!is.na(smp_id))

ow_summary <- ow_summary %>% dplyr::mutate(overtop_perc = overtopping_count/n)

gi_summary <- gi_metrics %>% group_by(ow_uid) %>% summarize(n = n(),
                                                            overtopping_count = sum(overtop),
                                                            avg_RPSU = mean(percentstorageused_relative, na.rm = TRUE)) %>%
  dplyr::left_join(gi_metrics, by = "ow_uid") %>%
  dplyr::select(ow_uid,smp_id,ow_suffix,n,overtopping_count, avg_RPSU) %>%
   unique() %>% dplyr::filter(!is.na(smp_id))

gi_summary <- gi_summary %>% dplyr::mutate(overtop_perc = overtopping_count/n)


write.csv(gi_summary, file = paste0(folderpath, "/", Sys.Date(), "/gi_summary.csv"))
write.csv(ow_summary, file = paste0(folderpath, "/", Sys.Date(), "/ow_summary.csv"))

## Join metrics with storm information

ot_data <- gi_metrics %>% left_join(rain_radar_event, by = "radar_event_uid")
write.csv(ot_data, paste0(folderpath, "/", Sys.Date(), "/overtopping_data.csv"))

# design depths
sysbdv <- dbGetQuery(mars_con,
                      paste0("SELECT * FROM external.tbl_systembdv where system_id IN ('",
                             paste(assets$system_id, collapse = "', '"),
                             "')")
                     )

sys_dsgn_strm <- sysbdv %>% dplyr::select(system_id, sys_creditedstormsizemanaged_in)

# join to ot_data
ot_data$system_id <- smp_2_sys(ot_data$smp_id)
dsgn_storm_data <- ot_data %>%
                   dplyr::left_join(sys_dsgn_strm, by = 'system_id') %>%
                   dplyr::filter(eventdepth_in <= sys_creditedstormsizemanaged_in)

# summarize design data
# we now do this in data_interpretation.R so that we can filter out specific dates where issues occur!
# gi_dsgn_summary <- dsgn_storm_data %>% group_by(ow_uid) %>% summarize(n = n(),
#                                                             overtopping_count = sum(overtop),
#                                                             mean_event = mean(eventdepth_in),
#                                                             avg_RPSU = mean(percentstorageused_relative, na.rm = TRUE)) %>%
#   dplyr::left_join(gi_metrics, by = "ow_uid") %>%
#   dplyr::select(ow_uid,smp_id,ow_suffix,mean_event,n,overtopping_count,avg_RPSU) %>%
#   unique() %>% dplyr::filter(!is.na(smp_id))
# 
# gi_dsgn_summary <- gi_dsgn_summary %>% dplyr::mutate(overtop_perc = overtopping_count/n)
# 
# write.csv(gi_dsgn_summary, paste0(folderpath, "/", Sys.Date(), "/gi_design_storm_summary.csv"))

