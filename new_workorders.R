#Cityworks work order exploration

library(tidyverse)
library(pool)
library(DBI)
library(lubridate)
library(RPostgres)

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

cw_con <- DBI::dbConnect(odbc::odbc(),
                         Driver = "ODBC Driver 17 for SQL Server",
                         Server = "PWDCWSQLP",
                         Database = "PWD_Cityworks",
                         uid = Sys.getenv("cw_uid"),
                         pwd= Sys.getenv("cw_pwd"))

#Finding and analyzing known pipe-jetting WOs
pjwo <- c(2430570, 2647997, 1903881, 1998605, 1971191, 2448998, 2448993, 
          2648867, 2648493, 3092109, 3092420, 2207043, 2176084, 2141812, 
          2176064, 2947490, 2947363, 2947063, 2947265, 1876040, 1876046, 
          3017919, 2563407, 2563409, 3058592, 3058593, 1683000, 1975797, 
          2448991, 2648044, 2648493, 3091147, 2430569)

wo <- dbGetQuery(cw_con, paste0("select * from Azteca.WORKORDER where
  WORKORDERID in (", paste(pjwo, collapse = ","), ")")) %>%
  select(WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, CANCELREASON, TEXT1)

wocom <- dbGetQuery(cw_con, paste0("select * from Azteca.WOCOMMENT where
  WORKORDERID in ('", paste(pjwo, collapse = "','"), "')")) %>%
  mutate(stripcommas = str_replace(COMMENTS, ',', '')) %>%
  mutate(stripnewline = str_replace(stripcommas, '\\s+', '')) %>%
  mutate(stripcrlf = str_replace(stripnewline, '\n', ' ')) %>%
  group_by(WORKORDERID) %>%
  summarize(comments = paste(stripcrlf, collapse = "; "))

#What entities were attached to those WOs?
oldentities <- dbGetQuery(cw_con, paste0("select WORKORDERID, ENTITYUID, ENTITYTYPE from azteca.workorderentity where workorderid in ('",
                                         paste(pjwo, collapse = "', '"), "')")) %>%
  transmute(WORKORDERID, ENTITYUID = ENTITYUID, ENTITYTYPE)

#Do we have all of these entities in the MARS DB?
marsOldEntities <- dbGetQuery(marsDBCon, paste0("select * from external.mat_assets where ",
                                                "component_id in ('", paste(oldentities$ENTITYUID, collapse = "', '"), "') OR ",
                                                "facility_id in ('", paste(oldentities$ENTITYUID[oldentities$ENTITYTYPE == "GSWIINLET"], collapse = "', '"), "')"))

#Which ones don't we have?
weirdones <- filter(oldentities, !(tolower(str_replace_all(ENTITYUID, "[{}]", "")) %in% marsOldEntities$facility_id)) %>%
  filter(!(ENTITYUID) %in% marsOldEntities$component_id)
#Three entities remaining:
# {6F5D5A94-A23F-4C37-8F58-A3A50ECBC849}, a wastewater inlet
# {4710D484-B28F-42A1-B855-417CA2C58BC9}, a wastewater inlet
# {575649EA-4B8B-4CAF-B7E9-63976D880C97}, a green inlet

#Do we have any such record of that green inlet?
marsWeirdEntities <- dbGetQuery(marsDBCon, paste0("select * from external.tbl_gswiinlet where ",
                                                  "facility_id in ('", paste(weirdones$ENTITYUID, collapse = "', '"), "')"))

#Yes, we do. It's a removed green inlet at 439-1-1. We can proceed, as we have internal records of all relevant green assets.

#Find the old work orders and locate their work order entities in the MARS database
marsassets <- dbGetQuery(marsDBCon, "select * from external.mat_assets") %>%
  mutate(facility_id = paste0('{', toupper(facility_id), '}'))

orders <- left_join(wo, wocom) %>% left_join(oldentities) %>% left_join(marsassets, by = c("ENTITYUID" = "value"))
categories <- unique(orders$DESCRIPTION)

#Not all relevant distribution pipes are included in this list. Make sure they are by filtering by SMP ID and manually adding them
smps <- unique(orders$smp_id)
inspectionruns <- filter(marsassets, smp_id %in% smps, 
                         asset_type == 'Inspection Run',
                         grepl('-20-', component_id)) #Distribution pipe type inspection run

#Melt the data frames to search for facility ID and component ID
marsassets <- marsassets %>% reshape2::melt(id.vars = c("smp_id", "system_id", "asset_type"))
inspectionruns <- inspectionruns %>% reshape2::melt(id.vars = c("smp_id", "system_id", "asset_type"))

#Compose final lists of entities
#Not including distribution pipes
searchentities <- oldentities$ENTITYUID[!(oldentities$ENTITYUID %in% inspectionruns$value)]
distpipeentities <- inspectionruns$value

#Finding updated work orders
# Search for all WOs with the same assets attached as the old work orders
entities <- dbGetQuery(cw_con, paste0("select * from azteca.workorderentity where entityuid in ('",
                                      paste(searchentities, collapse = "', '"), "')")) %>%
  mutate(ENTITYUID = toupper(ENTITYUID)) %>%
  transmute(WORKORDERID, ENTITYUID = toupper(ENTITYUID), ENTITYTYPE) %>% 
  left_join(marsassets, by = c("ENTITYUID" = "value")) %>%
  filter(!is.na(smp_id))

#Download those work orders with the correct work order categories
neworders <- unique(entities$WORKORDERID)

newquery <- paste0("select WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, CANCELREASON, TEXT1
                   from AZTECA.WORKORDER where WORKORDERID in (", paste(neworders, collapse = ","), ") ",
                   "and DESCRIPTION in ('", paste(categories, collapse = "', '"), "')")

freshorders <- dbGetQuery(cw_con, newquery) %>%
  left_join(entities, by = "WORKORDERID") %>% #Attach WO entities
  filter(!(WORKORDERID %in% orders$WORKORDERID)) %>% #Filter to only those not from the original analysis
  filter(!is.na(smp_id))

#Filter to only those WOs that took place aftet
filterdates <- group_by(orders, smp_id) %>% summarize(latest_date = max(INITIATEDATE))

realneworders <- group_by(freshorders, smp_id) %>%
  left_join(filterdates) %>%
  filter(INITIATEDATE > latest_date)

realnewcomments <- dbGetQuery(cw_con, paste0("select * from Azteca.WOCOMMENT where
  WORKORDERID in ('", paste(realneworders$WORKORDERID, collapse = "','"), "')")) %>%
  mutate(stripcommas = str_replace(COMMENTS, ',', '')) %>%
  mutate(stripnewline = str_replace(stripcommas, '\\s+', '')) %>%
  mutate(stripcrlf = str_replace_all(stripnewline, '[\r\n]', ' ')) %>%
  group_by(WORKORDERID) %>%
  summarize(comments = paste(stripcrlf, collapse = "; "))

realnewghostbusters <- full_join(realneworders, realnewcomments) %>%
  select(smp_id, ENTITYUID, ENTITYTYPE, WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, 
         CANCELREASON, TEXT1, comments) %>%
  filter(CANCEL == "N")

write_csv(realnewghostbusters, quote = 'all', file = "updated_work_orders-nodistpipe.csv")


### Only distribution pipes
#Finding updated work orders
# Search for all WOs with the same assets attached as the old work orders
entities <- dbGetQuery(cw_con, paste0("select * from azteca.workorderentity where entityuid in ('",
                                      paste(distpipeentities, collapse = "', '"), "')")) %>%
  mutate(ENTITYUID = toupper(ENTITYUID)) %>%
  transmute(WORKORDERID, ENTITYUID = toupper(ENTITYUID), ENTITYTYPE) %>% 
  left_join(marsassets, by = c("ENTITYUID" = "value")) %>%
  filter(!is.na(smp_id))

#Download those work orders with the correct work order categories
neworders <- unique(entities$WORKORDERID)

newquery <- paste0("select WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, CANCELREASON, TEXT1
                   from AZTECA.WORKORDER where WORKORDERID in (", paste(neworders, collapse = ","), ") ",
                   "and DESCRIPTION in ('", paste(categories, collapse = "', '"), "')")

freshorders <- dbGetQuery(cw_con, newquery) %>%
  left_join(entities, by = "WORKORDERID") %>% #Attach WO entities
  filter(!(WORKORDERID %in% orders$WORKORDERID)) %>% #Filter to only those not from the original analysis
  filter(!is.na(smp_id))

#Filter to only those WOs that took place aftet
filterdates <- group_by(orders, smp_id) %>% summarize(latest_date = max(INITIATEDATE))

realneworders <- group_by(freshorders, smp_id) %>%
  left_join(filterdates) %>%
  filter(INITIATEDATE > latest_date)

realnewcomments <- dbGetQuery(cw_con, paste0("select * from Azteca.WOCOMMENT where
  WORKORDERID in ('", paste(realneworders$WORKORDERID, collapse = "','"), "')")) %>%
  mutate(stripcommas = str_replace(COMMENTS, ',', '')) %>%
  mutate(stripnewline = str_replace(stripcommas, '\\s+', '')) %>%
  mutate(stripcrlf = str_replace_all(stripnewline, '[\r\n]', ' ')) %>%
  group_by(WORKORDERID) %>%
  summarize(comments = paste(stripcrlf, collapse = "; "))

realnewghostbusters <- full_join(realneworders, realnewcomments) %>%
  select(smp_id, ENTITYUID, ENTITYTYPE, WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, 
         CANCELREASON, TEXT1, comments) %>%
  filter(CANCEL == "N")

write_csv(realnewghostbusters, quote = 'all', file = "updated_work_orders-distpipeonly.csv")


