library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(pwdgsi)
library(pool)

excelfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Atlantic_Tioga_171/QAQC/171-2-1/GI1"
csvfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Atlantic_Tioga_171/Raw Data"

#Connect to DB
poolConn <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

#Enumerate QA sheets to check
excelsheets <- list.files(path = excelfolder, full.names=TRUE)

#Enumerate relevant raw CSVs
csvsheets <- list.files(path = csvfolder, 
                        pattern = "171-2-1_GI1_.*\\.csv",
                        recursive = TRUE,
                        full.names = TRUE)

#Pull the following info from the info sheet of a QA file
  # Cap to Hook
  # Hook to Sensor
  # Well Depth
  # Correction Factor
pullCorrectionFactor <- function(excelfile){
  #Sheet 1 is the site info sheet
  infosheet = suppressMessages(readxl::read_xlsx(excelfile, sheet = 1))
  
  #Variable names in column 4, values in column 5
  longdata = data.frame(varname = unlist(infosheet[5:8, 4]),
                        value = as.numeric(unlist(infosheet[5:8, 5])))
  
  #Transpose vertical to horizontal
  widedata = pivot_wider(longdata,
                        names_from = varname)
  widedata
  
}

pullCWLData <- function(excelfile){
  #Sheet 3 is the data sheet
  datasheet = suppressMessages(readxl::read_xlsx(excelfile, sheet = "Data"))
  rows <- nrow(datasheet) #variable for readability
  
  #dtime in column D, pressure in column E, temperature in column F
  rawdata = data.frame(rawdtime = unlist(datasheet[2:rows, 4]),
                        rawpres_psi = unlist(datasheet[2:rows, 5]),
                        rawtemp_f = unlist(datasheet[2:rows, 6]))
  
  #Process data for checks later
  longdata <- filter(rawdata, complete.cases(rawdata)) |> #Trim NAs
    transmute(dtime_excel = as.numeric(rawdtime), #Excel floating point
              pres_psi = round(as.numeric(rawpres_psi), 4), #Round to 4 decimals
              temp_f = round(as.numeric(rawtemp_f), 4)) %>%
    #Convert to POSIX date with methods here
    #https://stackoverflow.com/questions/19172632/converting-excel-datetime-serial-number-to-r-datetime
    mutate(dtime_raw = as.POSIXct(dtime_excel * (60*60*24),
                              origin = "1899-12-30",
                              tz = "GMT")) |> #GMT TZ required to count from correct origin
    mutate(dtime = round.POSIXt(dtime_raw, units = "mins")) |> #Round :59 up
    select(-dtime_raw)
  
    rownames(longdata) <- NULL
    longdata
}

pullBaroData <- function(excelfile){
  #Sheet 3 is the data sheet
  datasheet = suppressMessages(readxl::read_xlsx(excelfile, sheet = "Data"))
  rows <- nrow(datasheet) #variable for readability
  
  #dtime in column B, pressure in column C
  rawdata = data.frame(rawdtime = unlist(datasheet[2:rows, 2]),
                       rawpres_psi = unlist(datasheet[2:rows, 3]))
  
  #Process data for checks later
  longdata <- filter(rawdata, complete.cases(rawdata)) |> #Trim NAs
    transmute(dtime_excel = as.numeric(rawdtime), #Excel floating point
              pres_psi = round(as.numeric(rawpres_psi), 4)) |> #Round to 4 decimals
    #Convert to POSIX date with methods here
    #https://stackoverflow.com/questions/19172632/converting-excel-datetime-serial-number-to-r-datetime
    mutate(dtime_raw = as.POSIXct(dtime_excel * (60*60*24),
                              origin = "1899-12-30",
                              tz = "GMT")) |> #GMT TZ required to count from correct origin
    mutate(dtime = round.POSIXt(dtime_raw, units = "mins")) |> #Round :59 up
    select(-dtime_raw)

  
  rownames(longdata) <- NULL
  longdata
}

#Data import function, derived from Jon's code here
#https://github.com/PWD-MARS/shinyDownloadTools/issues/19
csv_import <- function(filepath){
  
  # I have modified the one file with an extra line that needed to be skipped
  # by deleting that line on disk, so now we don't need to create an exception
  file_raw <- readr::read_csv(file = filepath,
                              skip = 1,
                              col_select = 2:4,
                              show_col_types=FALSE,
                              name_repair = "unique_quiet") %>%
    suppressWarnings() #Suppress problems() warnings that don't matter to us
  
  names(file_raw) <- c("dtime_raw", "pres_psi", "temp_f")
  
  file_parsed <- file_raw %>%
    mutate(dtime = parse_date_time(dtime_raw, c("%m/%d/%y %I:%M:%S %p",
                                                    "%m/%d/%Y %H:%M:%S",
                                                    "%m/%d/%Y %H:%M"),
                                       tz = "GMT")) %>% #Files will be in GMT
                                                #from excel; dtimes will match
    select(-dtime_raw)
  file_parsed
}

#Data structure for results
results <- data.frame(filepath = excelsheets,
                      correction = NA,
                      barocheck = NA,
                      csvcheck = NA)

#Pull correction factors
for(i in 1:nrow(results)){
  infosheet <- pullCorrectionFactor(results$filepath[i])
  results$correction[i] <- infosheet$`Correction factor`
}


#Check CWL data
  #Pull all CSV data and assemble it
  csvdata <- data.frame(NULL)
  for(i in 1:length(csvsheets)){
    filedata <- csv_import(csvsheets[i])
    csvdata <- rbind(csvdata, filedata) |>
      arrange(dtime)
  }
  
  for(i in 1:nrow(results)){
    cwldata <- pullCWLData(results$filepath[i])
    
    cwl_join <- left_join(cwldata, csvdata, 
                          by = "dtime", 
                          suffix = c(".excel",".csv")) |>
      mutate(pres_equal = pres_psi.excel == pres_psi.csv, #NAs will return NA
             temp_equal = temp_f.excel == temp_f.csv)     #and will fail check
    
    results$csvcheck[i] <- all(all(cwl_join$pres_equal), 
                               all(cwl_join$temp_equal))
  }
  
#Check baro data
  #Pull appropriate baro range for the entire series
  baro <- marsFetchBaroData(poolConn, target_id = "171-2-1",
                            start_date = "2020-01-01",
                            end_date = "2025-12-31",
                            data_interval = "5 mins")
  
  for(i in 1:nrow(results)){
    barodata <- pullBaroData(results$filepath[i])
    
    baro_join <- left_join(barodata, baro, 
                          by = "dtime") |>
      mutate(baro_diff = (pres_psi/baro_psi - 1) * 100)
    
    results$barocheck[i] <- max(baro_join$baro_diff)
  }
  