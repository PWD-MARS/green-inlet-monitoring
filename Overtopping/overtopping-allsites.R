library(RPostgres)
library(tidyverse)
library(odbc)
library(pool)
library(pwdgsi)
library(RODBC)

options(stringsAsFactors=FALSE)

setwd("C:/Users/Monica.Gucciardi/Documents/github/green-inlet-monitoring/Overtopping")
source("casestudy_plot.R")

#Connect to the MARS database
marsDBCon <- tryCatch({
  dbPool(
    drv = RPostgres::Postgres(),
    host = "PWDMARSDBS1",
    port = 5434,
    dbname = "mars_prod",
    user= Sys.getenv("admin_uid"),
    password = Sys.getenv("admin_pwd"),
    timezone = NULL)},
  error = function(e){e})

#Connect to the MARS database
sandboxCon <- tryCatch({
  dbPool(
    drv = RPostgres::Postgres(),
    host = "PWDMARSDBS1",
    port = 5434,
    dbname = "cwl_sandbox_april2026",
    user= Sys.getenv("admin_uid"),
    password = Sys.getenv("admin_pwd"),
    timezone = NULL)},
  error = function(e){e})

# 1: Site metric gathering ----
# VUSP variables cited refer to the Head Differential graphic on page 11 
# of the 2024 VUSP presentation by Johanna Batterton and Brian Cruice
# \\pwdoows\OOWS\Watershed Sciences\GSI Monitoring
# \03 Reports and Presentations\ 02 Conferences\2024\Villanova Symposium 2024
# \Presentations\Batterton\Batterton_VUSP Symposium 2024.pdf
# x_y means "x, subscript y"

## 1.1 Site ow information ----
inlets <- dbGetQuery(marsDBCon, 
                     "select o.ow_uid, o.smp_id, ow_suffix, g.gage_uid
      from fieldwork.tbl_ow o
        left join admin.tbl_smp_gage g
          on o.smp_id = g.smp_id
      where (o.ow_suffix like 'GI%')
        and o.smp_id in ('1-1-1', '1-3-1', '171-1-1', '171-2-1', 
                         '179-5-1', '439-1-1', '488-5-1', '1006-1-1')") %>%
  filter(ow_uid != 1077) #179-5-1 GI2 was not monitored

## 1.2 Site design storm information ----
# This value is tracked in GreenIT as the "Storm Size Managed" (SSM)

# There are several versions of this value:
# Raw SSM: Direct result of the formula:
# Inches of rain = volume of system storage / system drainage area
# Credited SSM: Raw SSM, but rounded to the nearest 0.1 and capped at 2.0
# Capped for regulatory reasons in the definition of greened acres

# This analysis uses the *credited* SSM

# Stored at the system level so we must convert SMP IDs into systems
systems <- mutate(inlets, 
                  system_id = str_replace(string = smp_id, 
                                          pattern = "(\\d+-\\d+)-\\d+",
                                          replacement = "\\1")) %>%
  pull(system_id) %>%
  unique

ssmquery <- paste0("select system_id, sys_creditedstormsizemanaged_in 
    from external.tbl_systembdv
    where system_id in ('", paste(systems, collapse = "', '"), "')")

ssm <- dbGetQuery(marsDBCon, ssmquery)

## 1.3 Overtopping elevations----
# VUSP variable d_g
# Deployment depth for the sensor, e.g. distance from grate to sensor tip
otquery <- paste0("select ow_uid, start_dtime, end_dtime, deployment_depth_ft
            from fieldwork.viw_ow_plus_measurements 
            where ow_uid in (", paste(inlets$ow_uid, collapse = ", "), ')')

otdepths <- dbGetQuery(marsDBCon, otquery)

## 1.5 Unified calculation table
calctable <- mutate(inlets, 
                    system_id = str_replace(string = smp_id, 
                                            pattern = "(\\d+-\\d+)-\\d+",
                                            replacement = "\\1")) %>%
  left_join(ssm, by = "system_id") %>%
  left_join(otdepths, by = "ow_uid") %>%
  select(ow_uid, smp_id, ow_suffix, gage_uid,
         ssm_in = sys_creditedstormsizemanaged_in,
         d_g_ft = deployment_depth_ft)

  start <- ymd("2019-01-01", tz = "America/New_York")
  end <- ymd("2024-01-01", tz = "America/New_York")
  
  # Manual range overrides
  # Some systems have cutoff dates or exclusion periods
  ignore <- transmute(inlets, smp_id,
                      start = start,
                      end = end,
                      pause = NA_Date_,
                      resume = NA_Date_) %>% unique
  
  # 1-1-1 Override: Short-circuiting observed on 2020-11-13
  ignore$end[ignore$smp_id == "1-1-1"] <- ymd('2020-11-13', 
                                              tz = "America/New_York")
  
  # 439-1-1 Override: 
  ignore$end[ignore$smp_id == '439-1-1'] <- ymd('2022-01-14',
                                                tz = "America/New_York")
  
  # 179-5-1 Exclusion: Capped distribution pipe
  ignore$pause[ignore$smp_id == '179-5-1'] <- ymd('2021-11-08', 
                                                  tz = "America/New_York")
  
  ignore$resume[ignore$smp_id == '179-5-1'] <- ymd('2021-12-30', 
                                                   tz = "America/New_York")
  
  calctable <- left_join(calctable, ignore)

# 2: OT Calculations ----
# Perform OT calculations one site at a time
otresults <- data.frame(NULL)
for(i in 1:nrow(calctable)){
  #for(i in 1){ #Debug: Just the first site
  
  ## 2.1 Pull the time series data ----
  # Level data prod
  levelquery <- paste0("select * from data.tbl_ow_leveldata_raw 
      where ow_uid = ",  calctable$ow_uid[i], 
                       " and dtime >= '", calctable$start[i], "'",
                       " and dtime <= '", calctable$end[i], "'")
  print(levelquery)
  sitelevel <- dbGetQuery(marsDBCon, levelquery)
  
  # Level data sandbox
  sitelevel_sandbox <- dbGetQuery(sandboxCon, levelquery)
  
  # Rain gage data 
  rainquery <- paste0("select * from data.tbl_gage_rain 
      where gage_uid = ", calctable$gage_uid[i],
                      " and dtime >= '", calctable$start[i], "'",
                      " and dtime <= '", calctable$end[i], "'")
  print(rainquery)
  siterain <- dbGetQuery(marsDBCon, rainquery)
  
  #Rain event metadata
  eventquery <- paste0("select * from data.tbl_gage_event 
      where gage_uid = ", calctable$gage_uid[i],
                       " and eventdatastart >= '", calctable$start[i], "'",
                       " and eventdataend <= '", calctable$end[i], "'",
                       " and eventdepth_in <= ", calctable$ssm_in[i])
  print(eventquery)
  siteevents <- dbGetQuery(marsDBCon, eventquery)
  
  # 2.2 Apply exclusions ----
  if(!is.na(calctable$pause[i])){
    exclude <- interval(start = calctable$pause[i], end = calctable$resume[i])
    
    #Drop monitoring data in the exclusion zone
    sitelevel <- filter(sitelevel, !(dtime %within% exclude))
    sitelevel_sandbox <- filter(sitelevel_sandbox, !(dtime %within% exclude))
    
    #Drop rain in the exclusion zone
    siterain <- filter(siterain, !(dtime %within% exclude))
    
    #Events must not start or stop in the exclusion zone
    siteevents <- filter(siteevents, 
                         !(eventdatastart %within% exclude),
                         !(eventdataend %within% exclude))
  }  
  
  # 2.3 Eventwise OT calculation ----
  siteresults <- data.frame(NULL)
  for(j in 1:nrow(siteevents)){
    
    ### 2.3.1 Data range isolation ----
    event <- siteevents[j, ]
    print(paste("Checking event", j))
    
    # Isolate only the event duration and...
      # The beginning of the next event, OR
      # 24 hours later, whichever comes first
      #If this is the last event, there is no next event to check
    endtime <- ifelse(j != nrow(siteevents),
                      #If there are events left to come...
                      min(event$eventdataend + hours(24),
                          siteevents$eventdatastart[j+1]),
                      #If there are not events left to come...
                      event$eventdataend + hours(24)) %>%
      as.POSIXct(tz = "America/New_York") #min() returns an integer
    
    #Filter prod level data to range of interest
    eventlevel <- filter(sitelevel, 
                         dtime >= event$eventdatastart,
                         dtime <= endtime) %>%
      arrange(dtime)
    
    #Filter sandbox level data to range of interest
    eventlevel_sandbox <- filter(sitelevel_sandbox, 
                         dtime >= event$eventdatastart,
                         dtime <= endtime) %>%
      arrange(dtime)
    
    #Likewise with rain data
    #Rain data always ends at the end of the rain event
    eventrain <- filter(siterain, 
                        dtime >= event$eventdatastart,
                        dtime <= event$eventdataend) %>%
      arrange(dtime)
    
    #If the event has no data for this event, skip it
    if(nrow(eventlevel) == 0 | nrow(eventlevel_sandbox) == 0){
      
      #Compose error message if level data doesn't exist on prod and/or sandbox
        #Note: Level data should never exist on one but not the other. If that
        # happens, check this!!!
      message <- paste("No level data on",
                       ifelse(nrow(eventlevel) == 0, "Prod", ""),
                       ifelse(nrow(eventlevel) == 0 && 
                                nrow(eventlevel_sandbox) == 0,
                              "and", ""),
                       ifelse(nrow(eventlevel_sandbox) == 0, "Sandbox", ""))
      
      print(message)
      
      result <- mutate(event, 
                       ow_uid = calctable$ow_uid[i], #Tag result with well ID
                       ot_prod = NA,
                       ot_sandbox = NA,
                       steps_prod = NA,
                       steps_sandbox = NA,
                       note = message)
      
      siteresults <- rbind(siteresults, result)
      next
    }
    
    
    ## 2.3.2 OT Calculation ----
    #If the peak water level during this period is >= d_g_ft, we have an OT
    otrange <- eventlevel$level_ft >= calctable$d_g_ft[i]
    otrange_sandbox <- eventlevel_sandbox$level_ft >= calctable$d_g_ft[i]
    
    
    #Compose result
    result <- mutate(event, 
                     ow_uid = calctable$ow_uid[i], #Tag result with well ID
                     ot_prod = any(otrange),
                     ot_sandbox = any(otrange_sandbox),
                     steps_prod = sum(otrange, na.rm = TRUE),
                     steps_sandbox = sum(otrange_sandbox, na.rm = TRUE),
                     note = NA)
    
    siteresults <- rbind(siteresults, result)
    
    # if(calctable$ow_uid[i] == 1076 & event$gage_event_uid == 43964){
    #   browser()
    # }
    
  #   if(any(otrange) | any(otrange_sandbox)){
  #     #Debug
  #     print(paste("Event", event$gage_event_uid,
  #                 "at gage", event$gage_uid,
  #                 "at site", calctable$smp_id[i], calctable$ow_suffix[i],
  #                 "has overtopping! Plotting."))
  #     
  #     
  #     ## 2.3.3 Plot the event ----
  #     
  #     dir.create("otplots", showWarnings = FALSE)
      # eventplot <- marsCombinedPlot_Offset(event = event$gage_event_uid,
      #                               structure_name = paste(calctable$smp_id[i], calctable$ow_suffix[i]),
      #                               obs_datetime = eventlevel$dtime,
      #                               obs_level_ft = eventlevel$level_ft,
      #                               obs_datetime_2 = eventlevel_sandbox$dtime,
      #                               obs_level_ft_2 = eventlevel_sandbox$level_ft,
      #                               storage_depth_ft = calctable$d_g_ft[i],
      #                               rainfall_datetime = eventrain$dtime,
      #                               rainfall_in = eventrain$rainfall_in,
      #                               overflow_label = "Top of Inlet Grate",
      #                               installation_height_ft = 0.27)
  #     
  #     
  #     plotname <- paste(calctable$smp_id[i],
  #                       calctable$ow_suffix[i],
  #                       event$gage_event_uid,
  #                       sep = "_")
  #     
  #     ggsave(filename = paste0(plotname, ".png"),
  #            plot = eventplot,
  #            path = "otplots",
  #            width = 12,
  #            height = 8,
  #            units = "in")
  #     
  #   }
  #   
  }
  otresults <- rbind(otresults, siteresults)
}
  
#Make a good-looking table for the meeting
  pretty <- filter(otresults, !is.na(ot_prod)) %>%
    group_by(ow_uid) %>%
    summarize(
      `Mean Event Size (in)` = round(mean(eventdepth_in), 2),
      `Storm Events (Prod)` = sum(!is.na(ot_prod)),
      `Storm Events (SB)` = sum(!is.na(ot_sandbox)),
      `OT Events (Prod)` = sum(ot_prod, na.rm = TRUE),
      `OT Events (SB)` = sum(ot_sandbox, na.rm = TRUE),
      `Percent OT (Prod)` = round(sum(ot_prod, na.rm = TRUE)/
                                    sum(!is.na(ot_prod)) * 100, 1),
      `Percent OT (SB)` = round(sum(ot_sandbox, na.rm = TRUE)/
                                  sum(!is.na(ot_sandbox)) * 100, 1)
    ) %>% 
    left_join(inlets) %>%
    select(-gage_uid) %>%
    select(9, 10, 2, 3, 4, 5, 6, 7, 8)

  stormdata <- filter(otresults, !is.na(ot_prod)) %>%
    summarize(`First Storm` = min(eventdatastart),
              `Last Storm` = max(eventdatastart),
              `Smallest Storm (in)` = min(eventdepth_in),
              `Largest Storm (in)` = max(eventdepth_in))
  
  mismatch <-filter(otresults, !is.na(ot_prod)) %>%
    transmute(gage_event_uid, ow_uid,
              flip = ot_prod != ot_sandbox,
              durationchange = steps_prod != steps_sandbox) 
  
                           

# 3: Save the OT calculations ----
otfilename <- paste(today(), "OT_allsites.csv", sep = "_")
write.csv(otresults, file = otfilename, row.names=FALSE)
