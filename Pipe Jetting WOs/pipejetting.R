#Pipe Jetting Work Orders

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
                         Server = "PWDCWSQLPROD", #New Cityworks Location
                         Database = "PWD_Cityworks",
                         uid = Sys.getenv("cw_uid"),
                         pwd= Sys.getenv("cw_pwd"))

