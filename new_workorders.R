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
  select(WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, CANCELREASON)

wocom <- dbGetQuery(cw_con, paste0("select * from Azteca.WOCOMMENT where
  WORKORDERID in ('", paste(pjwo, collapse = "','"), "')")) %>%
  mutate(stripcommas = str_replace(COMMENTS, ',', '')) %>%
  mutate(stripnewline = str_replace(stripcommas, '\\s+', '')) %>%
  mutate(stripcrlf = str_replace(stripnewline, '\n', ' ')) %>%
  group_by(WORKORDERID) %>%
  summarize(comments = paste(stripcrlf, collapse = "; "))

orders <- left_join(wo, wocom)

categories <- unique(orders$DESCRIPTION)

assets <- c('{6DF5991C-E1DA-4D46-87E3-CFF8BF52CAD9}', #Our inlets
            '{D7A9BD57-1B2F-4BAE-BE1F-4509E7ECFC28}', 
            '{5521AD39-EBBB-4715-9373-5FA1D325EA9D}', 
            '{A0F97734-F06F-4D51-A3F5-EBFACF07DB01}', 
            '{60B63BCC-464B-4C45-80C0-B19A2F393C79}', 
            '{575649EA-4B8B-4CAF-B7E9-63976D880C97}', 
            '{AC1981EC-497B-41F9-9B4E-5A5BA816BAD3}', 
            '{8EB640A4-078E-4EAB-A264-91115117FD2E}', 
            '{6F836364-968C-407D-AE88-4E3C2BC294E4}')



entities <- dbGetQuery(cw_con, paste0("select * from azteca.workorderentity where entityuid in ('",
                                     paste(assets, collapse = "', '"), "')")) %>%
  mutate(ENTITYUID = toupper(ENTITYUID)) %>%
  select(WORKORDERID, ENTITYUID, ENTITYTYPE)

neworders <- unique(entities$WORKORDERID)

newquery <- paste0("select WORKORDERID, DESCRIPTION, INITIATEDATE, DATEWOCLOSED, CANCEL, CANCELREASON
                   from AZTECA.WORKORDER where WORKORDERID in (", paste(neworders, collapse = ","), ") ",
                   "and DESCRIPTION in ('", paste(categories, collapse = "', '"), "')")

realneworders <- dbGetQuery(cw_con, newquery) %>% 
  filter(!(WORKORDERID %in% orders$WORKORDERID)) %>%
  filter(INITIATEDATE > min(orders$INITIATEDATE))

realnewcomments <- dbGetQuery(cw_con, paste0("select * from Azteca.WOCOMMENT where
  WORKORDERID in ('", paste(realneworders$WORKORDERID, collapse = "','"), "')")) %>%
  mutate(stripcommas = str_replace(COMMENTS, ',', '')) %>%
  mutate(stripnewline = str_replace(stripcommas, '\\s+', '')) %>%
  mutate(stripcrlf = str_replace(stripnewline, '\n', ' ')) %>%
  group_by(WORKORDERID) %>%
  summarize(comments = paste(stripcrlf, collapse = "; "))

realnewghostbusters <- full_join(realneworders, realnewcomments)
