library(readxl)
library(readr)
library(dplyr)

excelfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Atlantic_Tioga_171/QAQC/171-2-1/GI1"
csvfolder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/02 GSI Monitoring Sites/Atlantic_Tioga_171/Raw Data"

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
  datasheet = suppressMessages(readxl::read_xlsx(excelfile, sheet = 3))
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
    mutate(dtime = as.POSIXct(dtime_excel * (60*60*24),
                              origin = "1899-12-30",
                              tz = "GMT")) #GMT TZ required to count from correct origin
    
    rownames(longdata) <- NULL
    longdata
}

pullBaroData <- function(excelfile){
  #Sheet 3 is the data sheet
  datasheet = suppressMessages(readxl::read_xlsx(excelfile, sheet = 3))
  rows <- nrow(datasheet) #variable for readability
  
  #dtime in column B, pressure in column C
  rawdata = data.frame(rawdtime = unlist(datasheet[2:rows, 2]),
                       rawpres_psi = unlist(datasheet[2:rows, 3]))
  
  #Process data for checks later
  longdata <- filter(rawdata, complete.cases(rawdata)) |> #Trim NAs
    transmute(dtime_excel = as.numeric(rawdtime), #Excel floating point
              pres_psi = round(as.numeric(rawpres_psi), 4)) %>% #Round to 4 decimals
    #Convert to POSIX date with methods here
    #https://stackoverflow.com/questions/19172632/converting-excel-datetime-serial-number-to-r-datetime
    mutate(dtime = as.POSIXct(dtime_excel * (60*60*24),
                              origin = "1899-12-30",
                              tz = "GMT")) #GMT TZ required to count from correct origin

  
  rownames(longdata) <- NULL
  longdata
}



pullCWLData(excelfile = excelsheets[1]) -> hmm


