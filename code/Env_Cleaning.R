###############################################################################
#UPLOADING AND CLEANING ENVIRONMENTAL DATA FOR GRAPHING/ANALYSIS
###############################################################################

#Data source: https://nvs.nanoos.org/CruiseSalish downloaded May 15, 2023
#Sites of interest: P111, P119, P120, P122, P123, P128
#Cruises included: Aug 1999, Jul 2004, Aug 2008, Oct 2011, May 2012, Apr 2013, 
#################  May 2014, Oct 2014, Nov 2015, May 2015, Oct 2016, May 2017,
#################  Sept 2017, Oct 2017, May 2018, Oct 2018, May 2019, Jul 2020,
#################  Sept 2020, Oct 2022

###############################################################################
#PREP WORK
###############################################################################

#Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

#Upload data
setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBayRockfish/Envmtl_data")

Aug1999_down <- read.csv("SalishCruise_August1999_downcast.csv")
Aug1999_up <- read.csv("SalishCruise_August1999_labupcast.csv")
Jul2004_down <- read.csv("SalishCruise_July2004_downcast.csv")
Jul2004_up <- read.csv("SalishCruise_July2004_labupcast.csv")
Aug2008_down <- read.csv("SalishCruise_August2008_downcast.csv")
Aug2008_up <- read.csv("SalishCruise_August2008_labupcast.csv")
Oct2011_down <- read.csv("SalishCruise_October2011_downcast.csv")
Oct2011_up <- read.csv("SalishCruise_October2011_labupcast.csv")
May2012_down <- read.csv("SalishCruise_May2012_downcast.csv") 
Apr2013_down <- read.csv("SalishCruise_April2013_downcast.csv")
May2014_down <- read.csv("SalishCruise_May2014_downcast.csv")
Oct2014_down <- read.csv("SalishCruise_October2014_downcast.csv") 
Nov2015_down <- read.csv("SalishCruise_November2015_downcast.csv") 
May2015_down <- read.csv("SalishCruise_May2015_downcast.csv") 
Mar2016_down <- read.csv("SalishCruise_March2016_NEMO_downcast.csv", check.names=F)
Oct2016_down <- read.csv("SalishCruise_October2016_NEMO_downcast.csv", check.names=F) 
Oct2016_up <- read.csv("SalishCruise_October2016_NEMO_labupcast.csv")
May2017_down <- read.csv("SalishCruise_May2017_NEMO_downcast.csv", check.names=F)
May2017_up <- read.csv("SalishCruise_May2017_NEMO_labupcast.csv")
Sep2017_down <- read.csv("SalishCruise_September2017_NEMO_downcast.csv", check.names=F)
Sep2017_up <- read.csv("SalishCruise_September2017_NEMO_labupcast.csv")
Oct2017_down <- read.csv("SalishCruise_October2017_NEMO_downcast.csv", check.names=F)
Oct2017_up <- read.csv("SalishCruise_October2017_NEMO_labupcast.csv")
May2018_down <- read.csv("SalishCruise_May2018_NEMO_downcast.csv", check.names=F)
May2018_up <- read.csv("SalishCruise_May2018_NEMO_labupcast.csv")
Oct2018_down <- read.csv("SalishCruise_October2018_downcast.csv", check.names=F)
Oct2018_up <- read.csv("SalishCruise_October2018_labupcast.csv")
May2019_down <- read.csv("SalishCruise_May2019_downcast.csv", check.names=F)
May2019_up <- read.csv("SalishCruise_May2019_labupcast.csv")
Jul2020_down <- read.csv("SalishCruise_July2020_NEMO_downcast.csv", check.names=F)
Jul2020_up <- read.csv("SalishCruise_July2020_NEMO_labupcast.csv")
Sep2020_down <- read.csv("SalishCruise_September2020_NEMO_downcast.csv", check.names=F)
Sep2020_up <- read.csv("SalishCruise_September2020_NEMO_labupcast.csv")
SepOct2021_down <- read.csv("SalishCruise_SeptemberOctober2021_NEMO_downcast.csv", check.names=F)
May2022_down <- read.csv("SalishCruise_May2022_NEMO_downcast.csv", check.names=F)
Oct2022_down <- read.csv("SalishCruise_Oct2022_downcast.csv", check.names=F)
Oct2022_up <- read.csv("SalishCruise_October2022_NEMO_labupcast.csv")

#prep for combination

setnames(Mar2016_down, 
         old = c('NMEAlat', 'NMEANlon', 'latitude: Latitude', 'longitude: Longitude', 
                 'NMEAtimeUTC', 'CruiseID', 'depSM: Depth', 'sal00: Salinity  Practical',
                 'sbeox0Mg/L: Oxygen  SBE 43', "flECO-AFL: Fluorescence  WET Labs ECO-AFL/FL"),
         new = c('Latitude.DegMin', 'Longitude.DegMin', 'Latitude.Deg', 
                 'Longitude.Deg', 'UTC.Time', 'Cruise.ID', 'Depth', 'Salinity', 
                 'Oxygen.Concentration.MG.1', 'Chlorophyll.Fluorescence'))

down2a <- bind_rows(Oct2016_down, May2017_down, Sep2017_down, Oct2017_down, 
                    May2018_down, May2019_down, Jul2020_down, Sep2020_down, 
                    SepOct2021_down)
down2a$NMEAtimeUTC <- mdy_hms(down2a$NMEAtimeUTC, tz = "UTC")
down2a <- down2a %>% mutate(date = date(NMEAtimeUTC))

May2022_down$NMEAtimeUTC <- mdy_hms(May2022_down$NMEAtimeUTC, tz = "UTC")
down2b <- May2022_down %>% mutate(date = date(NMEAtimeUTC))

Oct2018_down$NMEAtimeUTC <- mdy(Oct2018_down$NMEAtimeUTC, tz = "UTC")
down2c <- Oct2018_down %>% mutate(date = date(NMEAtimeUTC))

Oct2022_down$NMEAtimeUTC <- mdy(Oct2022_down$NMEAtimeUTC, tz = "UTC")
down2d <- Oct2022_down %>% mutate(date = date(NMEAtimeUTC))

#Combine data files

up1 <- bind_rows(Aug1999_up, Jul2004_up, Aug2008_up, Oct2011_up)

up2 <- bind_rows(Oct2016_up, May2017_up, Sep2017_up, Oct2017_up, May2018_up,
                   Oct2018_up, May2019_up, Jul2020_up, Sep2020_up, Oct2022_up)

down1 <- bind_rows(Aug1999_down, Jul2004_down, Aug2008_down, Oct2011_down, 
                  May2012_down, Apr2013_down, May2014_down, Oct2014_down, 
                  Nov2015_down, May2015_down, Mar2016_down)
down2 <- bind_rows(down2a, down2b, down2c, down2d)

#Housekeeping
rm(Aug1999_up, Jul2004_up, Aug2008_up, Oct2011_up)
rm(Oct2016_up, May2017_up, Sep2017_up, Oct2017_up, May2018_up, Oct2018_up, May2019_up, Jul2020_up, Sep2020_up, Oct2022_up)
rm(Aug1999_down, Jul2004_down, Aug2008_down, Oct2011_down, May2012_down, 
   Apr2013_down, May2014_down, Oct2014_down, Nov2015_down, May2015_down, Mar2016_down)
rm(Oct2016_down, May2017_down, Sep2017_down, Oct2017_down, May2018_down, Oct2018_down, 
   May2019_down, Jul2020_down, Sep2020_down, SepOct2021_down, May2022_down, Oct2022_down)
rm(down2a, down2b, down2c, down2d)

#combine down files prep

setnames(down2, 
         old = c('NMEAlat', 'NMEANlon', 'Latitude', 'Longitude', 
                 'NMEAtimeUTC', 'CruiseID', 'depSM: Depth', 'sal00: Salinity  Practical',
                 'sbeox0Mg/L: Oxygen  SBE 43', "flECO-AFL: Fluorescence  WET Labs ECO-AFL/FL"),
         new = c('Latitude.DegMin', 'Longitude.DegMin', 'Latitude.Deg', 
                 'Longitude.Deg', 'UTC.Time', 'Cruise.ID', 'Depth', 'Salinity', 
                 'Oxygen.Concentration.MG.1', 'Fluorescence'))

#Retain relevant information
down1 <- down1 %>% 
  rename(Fluorescence = Chlorophyll.Fluorescence) %>%
  select(Cruise.ID, UTC.Time, Latitude.DegMin, Longitude.DegMin, Latitude.Deg, 
         Longitude.Deg, Station, Depth, Temperature, Salinity, 
         Oxygen.Concentration.MG.1, Fluorescence)
down1$UTC.Time <- mdy_hms(down1$UTC.Time, tz = "UTC")
down1 <- down1 %>% mutate(date = date(UTC.Time))

down2 <- down2 %>%
  select(Cruise.ID, UTC.Time, Latitude.DegMin, Longitude.DegMin, Latitude.Deg, 
         Longitude.Deg, Station, Depth, Temperature, Salinity, 
         Oxygen.Concentration.MG.1, Fluorescence)

down <- bind_rows(down1, down2)
#down$UTC.Time <- mdy_hms(down$UTC.Time, tz = "UTC")

#Housekeeping
rm(down1, down2)

#Combine up files

setnames(up2, 
        old = c('CRUISE_ID', 'LATITUDE_DEG', "LONGITUDE_DEG", "LATITUDE_DEC", 
               'LONGITUDE_DEC', 'STATION_NO', 'CTDTMP_DEG_C_ITS90', 
              'OXYGEN_MG_L_1', 'NITRATE_UMOL_L', "NITRITE_UMOL_L", 
             'AMMONIUM_UMOL_L', 'PHOSPHATE_UMOL_L', "DEPTH..M.", "CTD.FLU..mg.m3."),
        new = c('Cruise.ID', 'Latitude.DegMin', 'Longitude.DegMin', 'Latitude.Deg',
           'Longitude.Deg', 'Station', "Temperature", 'Oxygen.Concentration.MG', 'Nitrate', 'Nitrite', 'Ammonium',
         'Phosphate', "Depth", "Fluorescence"))

up2 <- up2 %>% mutate(date = mdy(DATE_UTC))

up1$UTC.Time <- mdy_hms(up1$UTC.Time, tz = "UTC")
up1 <- up1 %>% mutate(date = date(UTC.Time))

#Retain relevant information
up1 <- up1 %>% 
  rename(Fluorescence = Chlorophyll.Fluorescence) %>%
  select(Cruise.ID, UTC.Time, date, Latitude.DegMin, Longitude.DegMin, 
         Latitude.Deg, Longitude.Deg, Station, Depth, Temperature, Salinity, 
         Oxygen.Concentration.MG, Nitrate, Nitrite, Ammonium, Phosphate, 
         Fluorescence)

up2 <- up2 %>%
  select(Cruise.ID, DATE_UTC, TIME_UTC, date, Latitude.DegMin, Longitude.DegMin, 
         Latitude.Deg, Longitude.Deg, Station, Depth, Temperature, 
         Oxygen.Concentration.MG, Nitrate, Nitrite, Ammonium, Phosphate, Fluorescence)

#compatibility prep
up1$Latitude.Deg <- as.numeric(up1$Latitude.Deg)
up1$Longitude.Deg <- as.numeric(up1$Longitude.Deg)
up2$Station <- as.character(up2$Station)
up1$Depth <- as.numeric(up1$Depth)
up1$Temperature <- as.numeric(up1$Temperature)
up1$Oxygen.Concentration.MG <- as.numeric(up1$Oxygen.Concentration.MG)
up1$Nitrate <- as.numeric(up1$Nitrate)
up1$Nitrite <- as.numeric(up1$Nitrite)
up1$Ammonium <- as.numeric(up1$Ammonium)
up1$Phosphate <- as.numeric(up1$Phosphate)
up1$Fluorescence <- as.numeric(up1$Fluorescence)

#Combine + housekeeping
up <- bind_rows(up1, up2)
rm(up1, up2)

#combine into one giant database
down$Latitude.Deg <- as.numeric(down$Latitude.Deg)
down$Longitude.Deg <- as.numeric(down$Latitude.Deg)
down$Depth <- as.numeric(down$Depth)
down$Temperature <- as.numeric(down$Temperature)
down$Fluorescence <- as.numeric(down$Fluorescence)

env_dat <- bind_rows(up, down)
rm(up,down)

