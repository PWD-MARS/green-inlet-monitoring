library(tidyverse)
library(lubridate)
library(odbc)
library(pool)

projectfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/2025 Extension/Overtopping Calculations"
setwd(projectfolder)

#Redownload well data

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
  error = function(e) e )

  inlets <- dbGetQuery(marsDBCon, 
                       "select o.ow_uid, o.smp_id, ow_suffix, g.gage_uid
      from fieldwork.tbl_ow o
        left join admin.tbl_smp_gage g
          on o.smp_id = g.smp_id
      where o.ow_suffix like 'GI%'
        and o.smp_id in ('1-1-1', '1-3-1', '171-1-1', '171-2-1', 
                         '179-5-1', '439-1-1', '488-5-1', '1006-1-1')")
  inlets <- filter(inlets, ow_uid != 1077) #179-5-1 GI2 was not monitored
  
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
  
  otquery <- paste0("select ow_uid, start_dtime, end_dtime, deployment_depth_ft
            from fieldwork.viw_ow_plus_measurements 
            where ow_uid in (", paste(inlets$ow_uid, collapse = ", "), ')')
  
  otdepths <- dbGetQuery(marsDBCon, otquery)
  
  calctable_prod <- mutate(inlets, 
                      system_id = str_replace(string = smp_id, 
                                              pattern = "(\\d+-\\d+)-\\d+",
                                              replacement = "\\1")) %>%
    left_join(ssm, by = "system_id") %>%
    left_join(otdepths, by = "ow_uid") %>%
    select(ow_uid, smp_id, ow_suffix, gage_uid,
           ssm_in = sys_creditedstormsizemanaged_in,
           d_g_ft = deployment_depth_ft)

#Connect to the archive
marsArchiveCon <- tryCatch({
  dbPool(
    drv = RPostgres::Postgres(),
    host = "PWDMARSDBS1",
    port = 5434,
    dbname = "green_inlet_archive",
    user= Sys.getenv("admin_uid"),
    password = Sys.getenv("admin_pwd"),
    timezone = NULL)},
  error = function(e) e )

inlets <- dbGetQuery(marsArchiveCon, 
                     "select o.ow_uid, o.smp_id, ow_suffix, g.gage_uid
      from fieldwork.tbl_ow o
        left join admin.tbl_smp_gage g
          on o.smp_id = g.smp_id
      where o.ow_suffix like 'GI%'
        and o.smp_id in ('1-1-1', '1-3-1', '171-1-1', '171-2-1', 
                         '179-5-1', '439-1-1', '488-5-1', '1006-1-1')")
inlets <- filter(inlets, ow_uid != 1077) #179-5-1 GI2 was not monitored

systems <- mutate(inlets, 
                  system_id = str_replace(string = smp_id, 
                                          pattern = "(\\d+-\\d+)-\\d+",
                                          replacement = "\\1")) %>%
  pull(system_id) %>%
  unique

ssmquery <- paste0("select system_id, sys_creditedstormsizemanaged_in 
    from external.tbl_systembdv
    where system_id in ('", paste(systems, collapse = "', '"), "')")

ssm <- dbGetQuery(marsArchiveCon, ssmquery)

otquery <- paste0("select ow_uid, start_dtime_est, end_dtime_est, deployment_depth_ft
            from fieldwork.viw_ow_plus_measurements 
            where ow_uid in (", paste(inlets$ow_uid, collapse = ", "), ')')

otdepths <- dbGetQuery(marsArchiveCon, otquery)

calctable_archive <- mutate(inlets, 
                         system_id = str_replace(string = smp_id, 
                                                 pattern = "(\\d+-\\d+)-\\d+",
                                                 replacement = "\\1")) %>%
  left_join(ssm, by = "system_id") %>%
  left_join(otdepths, by = "ow_uid") %>%
  select(ow_uid, smp_id, ow_suffix, gage_uid,
         ssm_in = sys_creditedstormsizemanaged_in,
         d_g_ft = deployment_depth_ft)


#Generate summary tables from OT CSVs
ot_prod <- read_csv("2025-12-04_OT_prod.csv") %>%
  mutate(eventdatastart = force_tz(eventdatastart, tzone = "America/New_York"),
         eventdataend = force_tz(eventdataend), tzone = "America/New_York")

ot_archive <- read_csv("2025-12-04_OT_archive.csv") %>%
  mutate(eventdatastart_edt = force_tz(eventdatastart_edt, tzone = "America/New_York"),
         eventdataend_edt = force_tz(eventdataend_edt), tzone = "America/New_York")

summary_prod <- filter(ot_prod, !is.na(ot)) %>%
  group_by(ow_uid) %>%
  summarize(n = n(),
            ot = sum(ot, na.rm = TRUE),
            mean_in = round(mean(eventdepth_in), 2),
            percent = round(sum(ot, na.rm = TRUE)/n(), 2)) %>%
  left_join(calctable_prod) %>%
  select(smp_id, ow_suffix, n, ot, percent, ssm_in, d_g_ft)

write.csv(summary_prod, file = "summary_prod.csv", row.names=FALSE)

summary_archive <- filter(ot_archive, !is.na(ot)) %>%
  group_by(ow_uid) %>%
  summarize(n = n(),
            ot = sum(ot, na.rm = TRUE),
            mean_in = round(mean(eventdepth_in), 2),
            percent = round(sum(ot, na.rm = TRUE)/n(), 2)) %>%
  left_join(calctable_archive) %>%
  select(smp_id, ow_suffix, n, ot, percent, ssm_in, d_g_ft)

write.csv(summary_archive, file = "summary_archive.csv", row.names=FALSE)

