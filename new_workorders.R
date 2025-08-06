#Cityworks work order exploration

library(tidyverse)
library(pool)
library(DBI)
library(lubridate)
library(RPostgres)

#Known pipe-jetting WOs
pjwo <- c(2430570, 2647997, 1903881, 1998605, 1971191, 2448998, 2448993, 
          2648867, 2648493, 3092109, 3092420, 2207043, 2176084, 2141812, 
          2176064, 2947490, 2947363, 2947063, 2947265, 1876040, 1876046, 
          3017919, 2563407, 2563409, 3058592, 3058593, 1683000, 1975797, 
          2448991, 2648044, 2648493, 3091147, 2430569)

cw_con <- DBI::dbConnect(odbc::odbc(),
                         Driver = "ODBC Driver 17 for SQL Server",
                         Server = "PWDCWSQLP",
                         Database = "PWD_Cityworks",
                         uid = Sys.getenv("cw_uid"),
                         pwd= Sys.getenv("cw_pwd"))

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

oldentities <- dbGetQuery(cw_con, paste0("select WORKORDERID, ENTITYUID from azteca.workorderentity where workorderid in ('",
  paste(pjwo, collapse = "', '"), "')"))

categories <- unique(orders$DESCRIPTION)

assets <- openxlsx::read.xlsx(xlsxFile = 
  "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/06 Special Projects/40 Green Inlet Monitoring/MARS Analysis/Assets.xlsx",
  sheet = "Inlet Depths") %>%
  transmute(smp_id, facility_id = paste0('{', facility_id, '}'), component_id) %>%
  reshape2::melt("smp_id") %>%
  transmute(smp_id, variable, value = toupper(value))

orders <- left_join(wo, wocom) %>% left_join(oldentities) %>% left_join(assets, by = c("ENTITYUID" = "value"))

#ordersWithAssets <- left_join(orders, )
#Search by component ID too

entities <- dbGetQuery(cw_con, paste0("select * from azteca.workorderentity where entityuid in ('",
                                     paste(oldentities$ENTITYUID, collapse = "', '"), "')")) %>%
  mutate(ENTITYUID = toupper(ENTITYUID)) %>%
  transmute(WORKORDERID, ENTITYUID = toupper(ENTITYUID), ENTITYTYPE) %>% 
  left_join(assets, by = c("ENTITYUID" = "value"))

neworders <- unique(entities$WORKORDERID)

newquery <- paste0("select WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, CANCELREASON, TEXT1
                   from AZTECA.WORKORDER where WORKORDERID in (", paste(neworders, collapse = ","), ") ",
                   "and DESCRIPTION in ('", paste(categories, collapse = "', '"), "')")

realneworders <- dbGetQuery(cw_con, newquery) %>%
  left_join(entities, by = "WORKORDERID")%>% 
  filter(!(WORKORDERID %in% orders$WORKORDERID))

%>%
  filter(INITIATEDATE > max(orders$INITIATEDATE))

realnewcomments <- dbGetQuery(cw_con, paste0("select * from Azteca.WOCOMMENT where
  WORKORDERID in ('", paste(realneworders$WORKORDERID, collapse = "','"), "')")) %>%
  mutate(stripcommas = str_replace(COMMENTS, ',', '')) %>%
  mutate(stripnewline = str_replace(stripcommas, '\\s+', '')) %>%
  mutate(stripcrlf = str_replace(stripnewline, '\n', ' ')) %>%
  group_by(WORKORDERID) %>%
  summarize(comments = paste(stripcrlf, collapse = "; "))

realnewghostbusters <- full_join(realneworders, realnewcomments) %>%
  left_join(entities, by = "WORKORDERID") %>%
  select(smp_id, WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, 
         CANCELREASON, TEXT1, comments)


