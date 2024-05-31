library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shiny)

counties <- st_read("data/counties.shp") 
drive <- st_read("data/drive.shp") 
st_write("data/data/drive.shp")
dnr <- st_read("data/dnr.shp") 
nps <- st_read("data/nps.shp") 
fws <- st_read("data/fws.shp")
golf <- st_read("data/golf.shp")

dnr$DESIG <- factor(dnr$DESIG,
                    levels = c("SP", "SF", "NRMA", "WMA", "NEA"))

locparks <- st_read("data/local_parks.shp")

# list.files("data/local_parks", pattern = ".shp$", full.names = FALSE)
# 
# aa <- st_read("data/local_parks/AAParks.shp") %>%
#   select(PARK_NAME)
# bc <- st_read("data/local_parks/BalCoPks.shp") %>%
#   select(FACILITY_N) %>%
#   rename(PARK_NAME = FACILITY_N)
# balt <- st_read("data/local_parks/BaltCityPks.shp") %>%
#   select(parkName) %>%
#   rename(PARK_NAME = parkName)
# cal <- st_read("data/local_parks/CalvertPks.shp") %>%
#   select(NAME) %>%
#   rename(PARK_NAME = NAME)
# car <- st_read("data/local_parks/CarrollPks.shp") %>%
#   select(SiteName) %>%
#   rename(PARK_NAME = SiteName)
# cec <- st_read("data/local_parks/CecilPks.shp") %>%
#   select(NAME) %>%
#   rename(PARK_NAME = NAME)
# char <- st_read("data/local_parks/CharlesPks.shp") %>%
#   select(Loc_Nm) %>%
#   rename(PARK_NAME = Loc_Nm)
# fred <- st_read("data/local_parks/FredPks.shp") %>%
#   select(Name) %>%
#   rename(PARK_NAME = Name)
# har <- st_read("data/local_parks/HarfordPks.shp") %>%
#   select(NAME) %>%
#   rename(PARK_NAME = NAME)
# how <- st_read("data/local_parks/HowardPks.shp") %>%
#   select(name) %>%
#   rename(PARK_NAME = name)
# moco <- st_read("data/local_parks/MoCoPks.shp") %>%
#   select(PARK_NAME) 
# pg <- st_read("data/local_parks/PGParks.shp") %>%
#   select(PARKNAME) %>%
#   rename(PARK_NAME = PARKNAME)
# qa <- st_read("data/local_parks/QAParks.shp") %>%
#   select(NAME) %>%
#   rename(PARK_NAME = NAME)
# wash <- st_read("data/local_parks/WashPks.shp") %>%
#   select(SITE) %>%
#   rename(PARK_NAME = SITE)
# 
# locparks <- rbind(aa, bc, balt, cal, car, cec, char, 
#                   fred, har, how, moco, pg, qa, wash) %>%
#   st_write("data/local_parks.shp")
