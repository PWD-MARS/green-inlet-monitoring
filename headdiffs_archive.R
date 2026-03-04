library(RPostgres)
library(tidyverse)
library(odbc)
library(pool)
library(pwdgsi)
library(openxlsx)

options(stringsAsFactors=FALSE)

projectfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/2025 Extension/"
setwd(projectfolder)

#Connect to the MARS database
marsDBCon <- tryCatch({
  dbPool(
    drv = RPostgres::Postgres(),
    host = "PWDMARSDBS1",
    port = 5434,
    dbname = "green_inlet_archive",
    user= Sys.getenv("admin_uid"),
    password = Sys.getenv("admin_pwd"),
    timezone = "EST")},
  error = function(e){e})

# 1: Site metric gathering ----
# VUSP variables cited refer to the Head Differential graphic on page 11 
# of the 2024 VUSP presentation by Johanna Batterton and Brian Cruice
# \\pwdoows\OOWS\Watershed Sciences\GSI Monitoring
# \03 Reports and Presentations\ 02 Conferences\2024\Villanova Symposium 2024
# \Presentations\Batterton\Batterton_VUSP Symposium 2024.pdf
# x_y means "x, subscript y"

## 1.1 Site ow information ----
wells <- dbGetQuery(marsDBCon, 
                     "select o.ow_uid, o.smp_id, ow_suffix, g.gage_uid
    from fieldwork.tbl_ow o
      left join admin.tbl_smp_gage g
        on o.smp_id = g.smp_id
    where (o.ow_suffix like 'GI%' or o.ow_suffix = 'OW1')
      and o.smp_id in ('1-1-1', '1-3-1', '171-1-1', '171-2-1', 
                       '179-5-1', '439-1-1', '488-5-1', '1006-1-1')")
wells <- filter(wells, ow_uid != 1077) #179-5-1 GI2 was not monitored


## 1.2 Site design storm information ----
# This value is tracked in GreenIT as the "Storm Size Managed" (SSM)

# There are several versions of this value:
# Raw SSM: Direct result of the formula:
# Inches of rain = volume of system storage / system drainage area
# Credited SSM: Raw SSM, but rounded to the nearest 0.1 and capped at 2.0
# Capped for regulatory reasons in the definition of greened acres

# This analysis uses the *credited* SSM

# Stored at the system level so we must convert SMP IDs into systems
systems <- mutate(wells, 
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
# VUSP variable d_g and d_o
# Deployment depth for the sensor, e.g. distance to sensor tip
 # For inlets, d_g is distance from the inlet grate to sensor tip
 # For wells, d_o is distance from the well cap to sensor tip

# Note: These records include OW measurements, which have had multiple depths
 # recorded for d_o across their entire deployment history. Subsesquent code
 # MUST allow for piecewise calculation of d_g-relevant metrics or else risk
 # erroneous values. 
 
# None of these redeployments take place during the VUSP analysis range
  # (Jan 2019 to Jan 2024), but when evaluating future events, we will need to
  # be responsive to this. Be careful!

# VUSP reproduction ends on 2024-01-01
vusp_end <- ymd("2024-01-01", tz = "America/New_York") #project ends Jan '24

otquery <- paste0("select ow_uid, start_dtime_est as deployment_start_dtime, 
  end_dtime_est as deployment_end_dtime, 
  deployment_depth_ft
        from fieldwork.viw_ow_plus_measurements 
        where ow_uid in (", paste(wells$ow_uid, collapse = ", "), ')')

otdepths <- dbGetQuery(marsDBCon, otquery) %>%
  filter(deployment_start_dtime <= vusp_end)

## 1.4 Analysis period  ----
# earliest deployment time to present
vusp_start <- ymd("2019-01-01", tz = "America/New_York") #project starts Jan '19

# Manual range overrides
# Some systems have cutoff dates or exclusion periods
ignore <- transmute(wells, smp_id,
                    start = vusp_start,
                    end = vusp_end,
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

## 1.5 Site elevations ----
 # Subject to revision during field surveying in 2026!
 # For now, we use the VUSP numbers

# VUSP variable e_g and e_o
  # Elevation for the sensor, e.g. distance above sea level
  # For inlets, e_g is measured from the inlet grate
  # For wells, e_o is measured from the well cap 
 elevations <- openxlsx::read.xlsx(xlsxFile = "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/relative_elevations.xlsx",
                     sheet = "elev") %>%
  select(ow_uid, as_built_elev)

## 1.6 Unified calculation table ----
calctable <- mutate(wells, 
                    system_id = str_replace(string = smp_id, 
                                            pattern = "(\\d+-\\d+)-\\d+",
                                            replacement = "\\1")) %>%
  left_join(ssm, by = "system_id") %>%
  left_join(otdepths, by = "ow_uid") %>%
  left_join(ignore, by = "smp_id", ) %>%
  left_join(elevations, by = "ow_uid") %>%
  rowwise() %>%
  transmute(ow_uid, smp_id, ow_suffix, gage_uid,
         ssm_in = sys_creditedstormsizemanaged_in,
         e_ft = as_built_elev, #e_g and e_o will be differentiated later
         d_ft = deployment_depth_ft, #d_g and d_o will be differentiated later
         d_start = deployment_start_dtime, d_end = deployment_end_dtime,
         start = max(start, d_start), #Analysis begins either at
          # start of the analysis period (if the deployment began before Jan 19)
          # or the start of the deployment period, whichever comes later
         pause, resume, end)


## Break point 1: GI Uniqueness ----
# The below analysis expects there to only be one GI per site. If this is untrue
 # the below code will break. Check for any duplicated SMP IDs when we filter to
 # only GI records
gi_duplicated <- filter(calctable, str_detect(ow_suffix, "GI")) %>%
  pull(smp_id) %>%
  duplicated %>%
  any

if(gi_duplicated){
  stop("Fatal error: Multiple GI records present for at least one site.")
}

# 2: HD Calculations ----
# Perform HD calculations one site at a time
hdresults <- data.frame(NULL)

sites <- unique(calctable$smp_id) #There are multiple rows in calctable per site
for(i in 1:length(sites)){
  #for(i in 1){ #Debug: Just the first site
  
  # Compute calculation range:
    # Calulation range is period where OW and GI have monitoring overlap
    # Start = start date for OW or GI monitoring, whichever comes later
    # End = end date for OW or GI monitoring, whichever comes first
  sitetable <- filter(calctable, smp_id == sites[i])
  calc_start <- max(c(sitetable$d_start, sitetable$start), na.rm = TRUE)
  calc_end <- min(c(sitetable$d_end, sitetable$end), na.rm = TRUE)
  
  #Extract the green inlet stats and OW stats for the site being evaluated
  site_gi <- filter(sitetable, 
                    str_detect(ow_suffix, "GI")) %>%
    mutate(d_g_ft = d_ft,
           e_g_ft = e_ft) %>% #Give variable correct VUSP name
    select(-d_ft, -e_ft) 
  
  site_ow <- filter(sitetable, 
                    str_detect(ow_suffix, "OW")) %>%
    mutate(d_o_ft = d_ft,
           e_o_ft = e_ft) %>% #Give variable correct VUSP name
    select(-d_ft, -e_ft) 
  
  
  ## 2.1 Pull the time series data ----
  # Level data
  gilevelquery <- paste0("select * from data.tbl_ow_leveldata_raw 
      where ow_uid = ",  site_gi$ow_uid, 
                       " and dtime_est >= '", calc_start, "'",
                       " and dtime_est <= '", calc_end, "'")
  sitegilevel <- dbGetQuery(marsDBCon, gilevelquery) %>%
    mutate(dtime = with_tz(dtime_est, tzone = "America/New_York"))
  
  owlevelquery <- paste0("select * from data.tbl_ow_leveldata_raw 
      where ow_uid = ",  site_ow$ow_uid, 
                         " and dtime_est >= '", calc_start, "'",
                         " and dtime_est <= '", calc_end, "'")
  siteowlevel <- dbGetQuery(marsDBCon, owlevelquery) %>%
    mutate(dtime = with_tz(dtime_est, tzone = "America/New_York"))
  
  # Rain gage data 
  rainquery <- paste0("select * from data.tbl_gage_rain 
      where gage_uid = ", site_gi$gage_uid,
                      " and dtime_edt >= '", calc_start, "'",
                      " and dtime_edt <= '", calc_end, "'")
  siterain <- dbGetQuery(marsDBCon, rainquery) %>%
    mutate(dtime = force_tz(dtime_edt, tzone = "America/New_York"))
  
  
  #Rain event metadata
  eventquery <- paste0("select * from data.tbl_gage_event 
      where gage_uid = ", site_gi$gage_uid,
                       " and eventdatastart_edt >= '", calc_start, "'",
                       " and eventdataend_edt <= '", calc_end, "'",
                       " and eventdepth_in <= ", site_gi$ssm_in)
  siteevents <- dbGetQuery(marsDBCon, eventquery) %>%
    mutate(eventdatastart = force_tz(eventdatastart_edt, tzone = "America/New_York"),
           eventdataend = force_tz(eventdataend_edt, tzone = "America/New_York"))

  # 2.2 Apply exclusions ----
  if(!is.na(site_gi$pause)){
    exclude <- interval(start = site_gi$pause, end = site_gi$resume)
    
    #Drop monitoring data in the exclusion zone
    sitegilevel <- filter(sitegilevel, !(dtime %within% exclude))
    siteowlevel <- filter(siteowlevel, !(dtime %within% exclude))
    
    #Drop rain in the exclusion zone
    siterain <- filter(siterain, !(dtime %within% exclude))
    
    #Events must not start or stop in the exclusion zone
    siteevents <- filter(siteevents, 
                         !(eventdatastart %within% exclude),
                         !(eventdataend %within% exclude))
  }  
  
  # 2.3 Eventwise HD calculation ----
  siteresults <- data.frame(NULL)
  for(j in 1:nrow(siteevents)){
    
    #delta_h_max: Maximum possible difference between inlet and well sensors
    # i.e. Inlet is overtopping and well is empty
    delta_h_max_ft <- site_gi$e_g_ft - site_ow$e_o_ft + site_ow$d_o_ft
    
    ### 2.3.1 Data range isolation ----
    event <- siteevents[j, ]
    
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
    
    #Filter level data to range of interest
    eventgilevel <- filter(sitegilevel, 
                         dtime >= event$eventdatastart,
                         dtime <= endtime) %>%
      arrange(dtime)
    
    eventowlevel <- filter(siteowlevel, 
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
    if(any(nrow(eventowlevel) == 0, nrow(eventgilevel) == 0)){
      result <- mutate(event, 
                       smp_id = sites[i], #Tag result with SMP ID
                       delta_h_ft = NA, 
                       delta_h_max_ft = delta_h_max_ft,
                       note = paste0(ifelse(nrow(eventowlevel) == 0, 
                                           "No OW data; ",
                                           ""),
                                    ifelse(nrow(eventgilevel) == 0,
                                           "No GI data; ",
                                           "")))
      siteresults <- rbind(siteresults, result)
      next
    }
    
    
    ## 2.3.2 HD Calculation ----
    #To find HD for the event, find the peak inlet water level, then find the 
     # corresponding water level in the well, and plug them into the equation
     # Δh = (eG – dG + wG) – (eO – dO + wO)
    index_gi <- which.max(eventgilevel$level_ft)
    peak_dtime_gi <- eventgilevel$dtime[index_gi]
    
    #If the sampling intervals of the well and inlet are different, peak_index
     # may not correspond to the index of the appropriate well measurement
    # To remedy this, we find the closest timestamp to peak_dtime instead
    timediffs <- abs(eventowlevel$dtime - peak_dtime_gi)
    index_ow <- which.min(timediffs)
    
    delta_h_ft <- (site_gi$e_g_ft - 
                     site_gi$d_g_ft + 
                     sitegilevel$level_ft[index_gi]) -
                  (site_ow$e_o_ft - 
                     site_ow$d_o_ft + 
                     siteowlevel$level_ft[index_ow])
    
    #Compose result
    result <- mutate(event, 
                     smp_id = sites[i], #Tag result with SMP ID
                     delta_h_ft = delta_h_ft, 
                     delta_h_max_ft = delta_h_max_ft,
                     note = NA)
    siteresults <- rbind(siteresults, result)
    
    
  }
  hdresults <- rbind(hdresults, siteresults)
}
  
write.csv(hdresults, file = "2025-12-12_HD_archive.csv", row.names=FALSE)
