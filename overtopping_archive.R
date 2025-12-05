library(RPostgres)
library(tidyverse)
library(odbc)
library(pool)
library(pwdgsi)

options(stringsAsFactors=FALSE)

projectfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/2025 Extension/Overtopping Calculations"
setwd(projectfolder)

# 0: Notes on Archival DB usage----

## 0.1 Background ----
# This file is meant to use an archival database contemporaneous to the VUSP
# 2024 green inlet monitoring analysis. Its goal is to make a best-guess
# about what the VUSP 2024 analysis would have looked like had it not contained
# the various bugs it was later discovered to contain.

# For more information on what those bugs were, see Monica Gucciardi's PPT
# presentation on the subject here:
# \\pwdoows\OOWS\Watershed Sciences\GSI Monitoring\06 Special Projects
# \40 Green Inlet Monitoring\2025 Extension\20241020_databasediffs.pptx

# The archival database was a copy of the production database at the time
# (mars_data, as it was then called) produced at 8:00 PM on April 5 2024.
# The archive file is here: \\pwdoows\OOWS\Watershed Sciences\GSI Monitoring
  # \07 Databases and Tracking Spreadsheets\18 MARS Database Back Up Files\PG 14
  # \20240405T2000_mars_data.pgdump"

## 0.2 Time Zone and DST differences ----
# This archival database differs from the current production database, mars_prod,
# in a few key respects. 

# Notably, the datetime fields in several key tables do
# not contain time zone metadata. They store timestamps referencing local time,
# but the database does not know what timezone that is.

# Additionally, the time series data in data.tbl_ow_leveldata_raw did not observe
# daylight savings time appropriately; the sensors did not spring forwards on
# the DST spring-forwards date. This was a longstanding MARS policy up until
# this was corrected in the summer of 2025, when the datetime fields were
# reconstructed to contain time zone metadata, and the time series contained
# in that table were retroactively sprung-forwards and fallen-back as appropriate.

## 0.3 Data Structure Names ----
# Finally, key datetime fields had their names changed during that correction.
# In the archive DB, datetime fields are appended with _est or _edt depending
# on whether the data in the field was observant to daylight savings time.
  # Data structures that weren't (timestamps appended with _est): 
    # data.tbl_ow_leveldata_raw, 
    # fieldwork.viw_ow_plus_measurements
  # Data structures that were (timestamps appended with _edt): 
    # data.tbl_gage_rain
    # data.tbl_gage_event

## 0.4 How this file was created ----
# This file has a counterpart, overtopping.R, that references mars_prod.
# The version that references mars_prod was written and debugged first.
# Once that file was debugged appropriately and ready for code review, this
# copy was created. 

# This copy was edited in only the necessary respects to support the use of the
# archival database. No other code, calculation methods, or comments have been
# changed. The specific changes include:
  # Preamble
  # - timezone = "EST" in the call to dbConnect()

  # Section 1.3
  # - datetime fields appended with _est

  # Section 1.4
  # - datetime fields appended with _est

  # Section 2.1
  # - datetime fields appended with _est or _edt as appropriate
  # - water level data sprung-forwards with with_tz(tzone = "America/New_York")
    # This would have been performed by an analyst of the day, because the
    # asymmetrical DST-compliance of level data vs rainf dta was a known DB bug.
  # - rainfall time zone set in-place with force_tz(tzone = "America/New_York")
    # This data correctly managed daylight savings time in the DB, so it is set
    # without modifying the clock time.

  # Section 2.2
  # - datetime fields appended with _est or _edt as appropriate
  
  # Section 2.3
  # - datetime fields appended with _est or _edt as appropriate
  # - "archive" appended to plot names

  # Section 3
  # - Filename changed to "OT_archive.csv" to distinguish from the prod version.

## 0.5 The future of this file ----
# Overtopping.R (that targets mars_prod) should be the primary object of code
# review. The methods of analysis are most-cleanly visible there, without
# extraneous time zone cruft.

# Once the methods in overtopping.R are verified, the sections of this file can
# be reviewed to verify that they differ only in respect to the changes shown in
# section 0.4.

# If edits to the methodology of the analysis are proposed, or bugs are found in
# the code of overtopping.R, the edits will be made to overtopping.R, and then
# the variable name/time zone adjudtments enumerated in section 0.4 will be
# reapplied to the new overtopping.R for to generate the new archive version.


#Connect to the MARS database
marsDBCon <- tryCatch({
  dbPool(
    drv = RPostgres::Postgres(),
    host = "PWDMARSDBS1",
    port = 5434,
    dbname = "green_inlet_archive",
    user= Sys.getenv("admin_uid"),
    password = Sys.getenv("admin_pwd"),
    timezone = NULL)},
  error = function(e) e )

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
    where o.ow_suffix like 'GI%'
      and o.smp_id in ('1-1-1', '1-3-1', '171-1-1', '171-2-1', 
                       '179-5-1', '439-1-1', '488-5-1', '1006-1-1')")
inlets <- filter(inlets, ow_uid != 1077) #179-5-1 GI2 was not monitored

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
otquery <- paste0("select ow_uid, start_dtime_est, end_dtime_est, deployment_depth_ft
            from fieldwork.viw_ow_plus_measurements 
            where ow_uid in (", paste(inlets$ow_uid, collapse = ", "), ')')

otdepths <- dbGetQuery(marsDBCon, otquery)

## 1.4 Analysis period  ----
# earliest deployment time to present
start <- min(otdepths$start_dtime)

# Manual range overrides
# Some systems have cutoff dates or exclusion periods
ignore <- transmute(inlets, smp_id,
                    end = ymd("2024-01-01", tz = "America/New_York"),
                    pause = NA_Date_,
                    resume = NA_Date_)

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

## 1.5 Unified calculation table
calctable <- mutate(inlets, 
                    system_id = str_replace(string = smp_id, 
                                            pattern = "(\\d+-\\d+)-\\d+",
                                            replacement = "\\1")) %>%
  left_join(ssm, by = "system_id") %>%
  left_join(otdepths, by = "ow_uid") %>%
  left_join(ignore, by = "smp_id") %>%
  select(ow_uid, smp_id, ow_suffix, gage_uid,
         ssm_in = sys_creditedstormsizemanaged_in,
         d_g_ft = deployment_depth_ft,
         end, pause, resume)

# Break point 1: Deployment validity ----
# As of Dec 2025, end_dtime is NA for all records 
# (the deployment depth has never changed between deployments)
# If this ever were to change, we would have multiple analysis ranges for
# some wells, and the subsequent analyses would need to use the appropriate
# value in the appropriate time range
if(any(duplicated(otdepths$ow_uid))){
  stop("Fatal error: multiple deployment depths for one or more sites. 
         Code below assumes only one deployment depth per site.")
}


# 2: OT Calculations ----
# Perform OT calculations one site at a time
otresults <- data.frame(NULL)
for(i in 1:nrow(calctable)){
  #for(i in 1){ #Debug: Just the first site
  
  ## 2.1 Pull the time series data ----
  # Level data
  levelquery <- paste0("select * from data.tbl_ow_leveldata_raw 
      where ow_uid = ",  calctable$ow_uid[i], 
                       " and dtime_est >= '", start, "'",
                       " and dtime_est <= '", calctable$end[i], "'")
  sitelevel <- dbGetQuery(marsDBCon, levelquery) %>%
    mutate(dtime_est = with_tz(dtime_est, tzone = "America/New_York"))
    #New for archival DB. See section 0.4.
  
         
  # Rain gage data 
  rainquery <- paste0("select * from data.tbl_gage_rain 
      where gage_uid = ", calctable$gage_uid[i],
                      " and dtime_edt >= '", start, "'",
                      " and dtime_edt <= '", calctable$end[i], "'")
  siterain <- dbGetQuery(marsDBCon, rainquery) %>%
    mutate(dtime_edt = force_tz(dtime_edt, tzone = "America/New_York"))
  
  #Rain event metadata
  eventquery <- paste0("select * from data.tbl_gage_event 
      where gage_uid = ", calctable$gage_uid[i],
                       " and eventdatastart_edt >= '", start, "'",
                       " and eventdataend_edt <= '", calctable$end[i], "'",
                       " and eventdepth_in <= ", calctable$ssm_in[i])
  siteevents <- dbGetQuery(marsDBCon, eventquery) %>%
    mutate(eventdatastart_edt = force_tz(eventdatastart_edt, tzone = "America/New_York"),
           eventdataend_edt = force_tz(eventdataend_edt, tzone = "America/New_York"))
  
  # 2.2 Apply exclusions ----
  if(!is.na(calctable$pause[i])){
    exclude <- interval(start = calctable$pause[i], end = calctable$resume[i])
    
    #Drop monitoring data in the exclusion zone
    sitelevel <- filter(sitelevel, !(dtime_est %within% exclude))
    
    #Drop rain in the exclusion zone
    siterain <- filter(siterain, !(dtime_edt %within% exclude))
    
    #Events must not start or stop in the exclusion zone
    siteevents <- filter(siteevents, 
                         !(eventdatastart_edt %within% exclude),
                         !(eventdataend_edt %within% exclude))
  }
  
  # 2.3 Eventwise OT calculation ----
  siteresults <- data.frame(NULL)
  for(j in 1:nrow(siteevents)){
    
    ### 2.3.1 Data range isolation ----
    event <- siteevents[j, ]
    
    # Isolate only the event duration and...
    # The beginning of the next event, OR
    # 24 hours later, whichever comes first
    #If this is the last event, there is no next event to check
    endtime <- ifelse(j != nrow(siteevents),
                      #If there are events left to come...
                      min(event$eventdataend_edt + hours(24),
                          siteevents$eventdatastart_edt[j+1]),
                      #If there are not events left to come...
                      event$eventdataend_edt + hours(24)) %>%
      as.POSIXct(tz = "America/New_York") #min() returns an integer
    
    #Filter level data to range of interest
    eventlevel <- filter(sitelevel, 
                         dtime_est >= event$eventdatastart_edt,
                         dtime_est <= endtime) %>%
      arrange(dtime_est)
    
    #Likewise with rain data
    #Rain data always ends at the end of the rain event
    eventrain <- filter(siterain, 
                        dtime_edt >= event$eventdatastart_edt,
                        dtime_edt <= event$eventdataend_edt) %>%
      arrange(dtime_edt)
    
    #If the event has no data for this event, skip it
    if(nrow(eventlevel) == 0){
      result <- mutate(event, 
                       ow_uid = calctable$ow_uid[i], #Tag result with well ID
                       ot = NA, steps = NA, note = "No level data.")
      siteresults <- rbind(siteresults, result)
      next
    }
    
    
    ## 2.3.2 OT Calculation ----
    #If the peak water level during this period is >= d_g_ft, we have an OT
    otrange <- eventlevel$level_ft >= calctable$d_g_ft[i]
    
    
    #Compose result
    result <- mutate(event, 
                     ow_uid = calctable$ow_uid[i], #Tag result with well ID
                     ot = any(otrange),
                     steps = sum(otrange),
                     note = NA)
    
    siteresults <- rbind(siteresults, result)
    
    if(any(otrange)){
      #Debug
      print(paste("Event", event$gage_event_uid,
                  "at gage", event$gage_uid,
                  "at site", calctable$smp_id[i], calctable$ow_suffix[i],
                  "has overtopping! Plotting."))
      
      
      ## 2.3.3 Plot the event ----
        dir.create("otplots", showWarnings = FALSE)
        eventplot <- marsCombinedPlot(event = event$gage_event_uid,
           structure_name = paste(calctable$smp_id[i], calctable$ow_suffix[i]),
           obs_datetime = eventlevel$dtime_est,
           obs_level_ft = eventlevel$level_ft,
           storage_depth_ft = calctable$d_g_ft[i],
           rainfall_datetime = eventrain$dtime_edt,
           rainfall_in = eventrain$rainfall_in)


        plotname <- paste(calctable$smp_id[i],
                          calctable$ow_suffix[i],
                          event$gage_event_uid,
                          "archive",
                          sep = "_")

        ggsave(filename = paste0(plotname, ".png"),
               plot = eventplot,
               path = "otplots",
               width = 12,
               height = 8,
               units = "in")
      
    }
    
  }
  otresults <- rbind(otresults, siteresults)
}

# 3: Save the OT calculations ----
otfilename <- paste(today(), "OT_archive.csv", sep = "_")
write.csv(otresults, file = otfilename, row.names=FALSE)
