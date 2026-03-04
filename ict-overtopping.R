library(tidyverse)
library(readxl)
library(lubridate)

# Raw data folders
  folder_13 <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Hartranft_1/Inlet Conveyance/1-3/20250915/Raw Data"
  folder_1711 <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Atlantic_Tioga_171/Inlet Conveyance/171-1/20250730/Raw Data"
  folder_1712 <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Atlantic_Tioga_171/Inlet Conveyance/171-2/20250722/Raw Data"
  folder_4885 <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Smith Playground_488/Inlet Conveyance/488-5/20250922/Raw Data"
  folder_1795 <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Morris Leeds_179/Inlet Conveyance/179-5/20250807/Raw Data"
  folder_10061 <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Bridge_Creston_Darrah_Penn_1006/Inlet Conveyance/1006-1-1/20250811/Raw Data"

  folderframe <- data.frame(
    smp_id = c("1-3-1", "171-1-1", "171-2-1", 
               "488-5-1", "179-5-1", "1006-1-1"),
    folder = c(folder_13, folder_1711, folder_1712,
               folder_4885, folder_1795, folder_10061),
# # Inlet Overflow depths from a combination of field notes and as-builts
##### these values might be incorrect in the notes. Using the deployment depths
##### from the database instead
#     gi_overflow = c((35+18+31)/12, #sump = 31in, pipe diam = 18in, grate = 31in
#                     (13.5+8+24)/12,#sump = 13.5in, pipe diam = 8in, grate = 24in
#                     (14+8+25)/12, #sump = 14in, pipe diam = 8in, grate = 25in
#                     NA, #Field notes have no measurements, taken from as-built
#                     (31+8+39)/12, #sump = 31in, pipe diam = 8in, grate = 39in
#                     (31.25+8+24)/12),
    stringsAsFactors=FALSE)
  
# Inlet Overflow depths
  inlets <- dbGetQuery(marsDBCon, 
                       "select o.ow_uid, o.smp_id, ow_suffix, g.gage_uid
    from fieldwork.tbl_ow o
      left join admin.tbl_smp_gage g
        on o.smp_id = g.smp_id
    where o.ow_suffix like 'GI%'
      and o.smp_id in ('1-1-1', '1-3-1', '171-1-1', '171-2-1', 
                       '179-5-1', '439-1-1', '488-5-1', '1006-1-1')")
  inlets <- filter(inlets, ow_uid != 1077) #179-5-1 GI2 was not monitored
  
  otquery <- paste0("select smp_id, deployment_depth_ft 
    from fieldwork.viw_ow_plus_measurements 
    where ow_uid in (", paste(inlets$ow_uid, collapse = ", "), ')')
  
  otdepths <- dbGetQuery(marsDBCon, otquery)
  
# DCIA for converting flow volume to depth of rain
  dciaquery <- paste("select distinct smp_id, dcia_ft2 
                     from external.viw_greenit_unified
                     where smp_id in ('",paste(inlets$smp_id, collapse ="', '"),
                     "')")
  dcia <- dbGetQuery(marsDBCon, dciaquery)
  
  siteframe <- left_join(folderframe, otdepths, by = "smp_id") %>%
    left_join(dcia, by = "smp_id")
  
# Density of water
  density <- read_csv("C:/Users/Monica.Gucciardi/Documents/github/green-inlet-monitoring/density.csv")

# Flow data
  # Read from ICT data sheet
  ict <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Testing/ICT Data.xlsx"
  
  #1-3-1
  flow_13 <- read_xlsx(path = ict,
                       sheet = "1-3 Flow",
                       col_names = TRUE,
                       col_types = c("numeric", "guess", "numeric", 
                                     "text", "text", "text"),
                       range = "A1:F100") %>%
    filter(!is.na(Time)) %>%
    transmute(elapsed_min = Time,
              dtime = EST,
              flow_cfm = CFM,
              vol_cf = CF,
              ot = ifelse(`OT?` == "Y", TRUE, FALSE),
              smp_id = "1-3-1")
  
  #Manually fix datetime
  day(flow_13$dtime) <- 15
  month(flow_13$dtime) <- 9
  year(flow_13$dtime) <- 2025
  flow_13$dtime <- force_tz(flow_13$dtime, "America/New_York")
  
  #171-1-1
  flow_1711 <- read_xlsx(path = ict,
                       sheet = "171-1 Flow",
                       col_names = TRUE,
                       col_types = c("numeric", "guess", "numeric", 
                                     "text", "text", "text"),
                       range = "A1:F100") %>%
    filter(!is.na(Time)) %>%
    transmute(elapsed_min = Time,
              dtime = EST,
              flow_cfm = CFM,
              vol_cf = CF,
              ot = ifelse(`OT?` == "Y", TRUE, FALSE),
              smp_id = "171-1-1")
  
  #Manually fix datetime
  day(flow_1711$dtime) <- 30
  month(flow_1711$dtime) <- 7
  year(flow_1711$dtime) <- 2025
  flow_1711$dtime <- force_tz(flow_1711$dtime, "America/New_York")
  
  #171-2-1
  flow_1712 <- read_xlsx(path = ict,
                       sheet = "171-2 Flow",
                       col_names = TRUE,
                       col_types = c("numeric", "guess", "numeric", 
                                     "text", "text", "text"),
                       range = "A1:F100") %>%
    filter(!is.na(Time)) %>%
    transmute(elapsed_min = Time,
              dtime = EST,
              flow_cfm = CFM,
              vol_cf = CF,
              ot = ifelse(`OT?` == "Y", TRUE, FALSE),
              smp_id = "171-2-1")
  
  #Manually fix datetime
  day(flow_1712$dtime) <- 22
  month(flow_1712$dtime) <- 7
  year(flow_1712$dtime) <- 2025
  flow_1712$dtime <- force_tz(flow_1712$dtime, "America/New_York")
  
  #488-5-1
  flow_4885 <- read_xlsx(path = ict,
                       sheet = "488-5 Flow",
                       col_names = TRUE,
                       col_types = c("numeric", "guess", 
                                     "numeric", "text", "text", "text"),
                       range = "A1:F100") %>%
    filter(!is.na(Time)) %>%
    transmute(elapsed_min = Time,
              dtime = EST,
              flow_cfm = CFM,
              vol_cf = CF,
              ot = ifelse(`OT?` == "Y", TRUE, FALSE),
              smp_id = "488-5-1")
  
  #Manually fix datetime
  day(flow_4885$dtime) <- 22
  month(flow_4885$dtime) <- 9
  year(flow_4885$dtime) <- 2025
  flow_4885$dtime <- force_tz(flow_4885$dtime, "America/New_York")
  
  #179-5-1
  flow_1795 <- read_xlsx(path = ict,
                       sheet = "179-5 Flow",
                       col_names = TRUE,
                       col_types = c("numeric", "guess", "numeric", 
                                     "text", "text", "text"),
                       range = "A1:F100") %>%
    filter(!is.na(Time)) %>%
    transmute(elapsed_min = Time,
              dtime = EST,
              flow_cfm = CFM,
              vol_cf = CF,
              ot = ifelse(`OT?` == "Y", TRUE, FALSE),
              smp_id = "179-5-1")
  
  #Manually fix datetime
  day(flow_1795$dtime) <- 7
  month(flow_1795$dtime) <- 8
  year(flow_1795$dtime) <- 2025
  flow_1795$dtime <- force_tz(flow_1795$dtime, "America/New_York")
  
  #1006-1-1
  flow_10061 <- read_xlsx(path = ict,
                       sheet = "1006-1 Flow",
                       col_names = TRUE,
                       col_types = c("numeric", "guess", "numeric", 
                                     "text", "text", "text"),
                       range = "A1:F100") %>%
    filter(!is.na(Time)) %>%
    transmute(elapsed_min = Time,
              dtime = EST,
              flow_cfm = CFM,
              vol_cf = CF,
              ot = ifelse(`OT?` == "Y", TRUE, FALSE),
              smp_id = "1006-1-1")
  
  #Manually fix datetime
  day(flow_10061$dtime) <- 11
  month(flow_10061$dtime) <- 8
  year(flow_10061$dtime) <- 2025
  flow_10061$dtime <- force_tz(flow_10061$dtime, "America/New_York")

# Read the site data
  gi_data <- data.frame()
  ow_data <- data.frame()
  for(i in 1:nrow(folderframe)){
    # Read the Baro CSV
    baro_file <- list.files(folderframe$folder[i], 
                            pattern = "BARO.*\\.csv", 
                            ignore.case = TRUE,
                            full.names=TRUE)
    
    site_baro <- read_csv(baro_file, 
                          col_names=c(NA, "dtime", "baro_psi", "baro_f"), 
                          skip = 2) %>%
      transmute(dtime = mdy_hms(dtime, tz = "America/New_York"),
                baro_psi,
                baro_f)
    
    # Read the OW CSV
    ow_file <- list.files(folderframe$folder[i], 
                            pattern = "OW1.*_LVL.*\\.csv", 
                            full.names=TRUE)
    
    if(length(ow_file) == 0){ #One site has a different name scheme
      ow_file <- list.files(folderframe$folder[i], 
                            pattern = "LVL_OW1.*\\.csv", 
                            full.names=TRUE)
    }
    
    site_ow <- read_csv(ow_file, 
                          col_names=c(NA, "dtime", "ow_psi", "ow_f"), 
                          skip = 2) %>%
      transmute(dtime = mdy_hms(dtime, tz = "America/New_York"),
                ow_psi,
                ow_f)
    
    # Read the GI CSV
    gi_file <- list.files(folderframe$folder[i], 
                            pattern = "GI.*\\.csv", 
                            full.names=TRUE)
    
    site_gi <- read_csv(gi_file, 
                          col_names=c(NA, "dtime", "gi_psi", "gi_f"), 
                          skip = 2) %>%
      transmute(dtime = mdy_hms(dtime, tz = "America/New_York"),
                gi_psi,
                gi_f)
    
    # Convert the CWL CSVs to water level
    site_level <- inner_join(site_ow, site_baro, by = "dtime") %>%
      left_join(density, by = join_by(closest(y$temp_f <= x$ow_f))) %>%
      transmute(dtime,
                level_ft = (ow_psi - baro_psi)*144/density_lbft3,
                smp_id = folderframe$smp_id[i])
    
    site_inlet <- inner_join(site_gi, site_baro, by = "dtime") %>%
      left_join(density, by = join_by(closest(y$temp_f <= x$gi_f))) %>%
      transmute(dtime,
                level_ft = (gi_psi - baro_psi)*144/density_lbft3,
                smp_id = folderframe$smp_id[i]) %>%
      left_join(otdepths, by = "smp_id") %>%
      mutate(ot = (level_ft >= deployment_depth_ft))
    
    gi_data <- rbind(gi_data, site_inlet)
  }

  flow_data <- rbind(flow_13, flow_1711, flow_1712, 
                     flow_1795, flow_4885, flow_10061)
  
  full_data <- inner_join(gi_data, flow_data, by = c("dtime", "smp_id"),
                         suffix = c("_calc", "_field"))
  
  for(i in 1:nrow(siteframe)){
    site_complete <- filter(full_data, smp_id == siteframe$smp_id[i]) %>%
      select(3, 1, 6, 7, 8, 2, 4, 5, 9)
    
    write_csv(site_complete, file = paste0(siteframe$smp_id[i], ".csv"))
  }