library(RPostgres)
library(tidyverse)
library(odbc)
library(pool)
library(pwdgsi)
library(openxlsx)

projectfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/2025 Extension/"
setwd(projectfolder)

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

# Read categorical variables from spreadsheet
sys_char <- openxlsx::read.xlsx(xlsxFile = "SystemCharacteristics.xlsx",
                                sheet = "Characteristics") %>%
  select(smp_id, 
         slope = `Distrib..Slope.(%)`)
assets <- openxlsx::read.xlsx(xlsxFile = "Assets.xlsx",
                                sheet = "Inlet Depths") %>%
  transmute(ow_uid, smp_id, ow_suffix,
            trap = ifelse(Trap, "New (Without Trap)", "Old (With Trap)"))

# Read HD metrics
hd_archive <- read_csv("2025-12-12_HD_archive.csv") %>%
  rowwise() %>%
  mutate(hd_norm = max(delta_h_ft/delta_h_max_ft, 0, na.rm = FALSE)) %>%
  select(gage_event_uid, gage_uid, smp_id,
         delta_h_ft, delta_h_max_ft, hd_norm, note)

hd_prod <- read_csv("2025-12-12_HD_prod.csv") %>%
  rowwise() %>%
  mutate(hd_norm = max(delta_h_ft/delta_h_max_ft, 0, na.rm = FALSE)) %>%
  select(gage_event_uid, gage_uid, smp_id,
         delta_h_ft, delta_h_max_ft, hd_norm, note)

#Read OT metrics
ot_archive <- read_csv("Overtopping Calculations/2025-12-04_OT_archive.csv") %>%
  left_join(wells, by = c("ow_uid", "gage_uid")) %>%
  select(gage_event_uid, gage_uid, smp_id, ot)

ot_prod <- read_csv("Overtopping Calculations/2025-12-04_OT_prod.csv") %>%
  left_join(wells, by = c("ow_uid", "gage_uid")) %>%
  select(gage_event_uid, gage_uid, smp_id, ot)

unified_archive <- full_join(ot_archive, 
                             hd_archive,
                             by = c("smp_id", "gage_uid", "gage_event_uid"))
