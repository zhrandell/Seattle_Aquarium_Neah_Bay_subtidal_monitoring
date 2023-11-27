###############################################################################
#STEP 1: UPLOADING AND CLEANING ENVIRONMENTAL DATA FOR GRAPHING/ANALYSIS
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
setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBay/Seattle_Aquarium_Neah_Bay_subtidal_monitoring/data_input")

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

###############################################################################
#STEP 3: NEAH BAY ENVIRONMENTAL PLOTTING
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
library(ggplot2)
library(ggpubr)
library(cowplot)
library(DataCombine)

#filter env.dat for sites of interest, within depth of study
env_dat$Station <- tolower(env_dat$Station)
env_dat <- env_dat %>% 
  filter(., Station %in% c("111", "p111", "119", "p119", "120", "p120", "122", 
                           "p122", "123", "p123", "128", "p128")) %>%
  filter(., Depth <= 30)

#rename complex column names
env_dat <- env_dat %>% 
  rename("DO" = "Oxygen.Concentration.MG", "Chlor_a" = "Fluorescence")

#make tidy version for graphing
env_dat$Salinity <- as.numeric(env_dat$Salinity)

fig.dat <- env_dat %>%
  pivot_longer("Temperature":"Chlor_a", names_to = "Parameter", values_to = "Value")

##Select parameters of interest
fig.dat <- fig.dat %>% mutate(year = year(date))
fig.dat <- fig.dat %>% 
  filter(year >= 2005) %>%
  filter(Parameter %in% c("Temperature", "Salinity", "DO", "Chlor_a"))

#Relabel parameter values so that unit is included:
fig.dat <- as.data.frame(fig.dat)

translate <- data.frame(
  old = c("Chlor_a","DO","Salinity","Temperature"), 
  new = c( "Chlor_a (mg/L)","DO (mg/L)","Salinity (ppt)","Temperature (C)"))

fig.dat <- FindReplace(data = fig.dat, Var = "Parameter", replaceData = translate, from = "old", to = "new", exact = TRUE, vector = FALSE)

rm(translate)

#plot 1: environmental parameters over time
p1 <- ggplot(data = fig.dat, aes(x=date, y=Value, color=Parameter, group=Parameter)) + 
  geom_point(alpha=0.5) + theme_cowplot() + geom_smooth(se=FALSE, linetype="dotted") +
  scale_y_continuous(trans="log2") + theme(axis.title.x=element_blank())

p1
#plot 2: environmental parameters overall
#ggplot(data = fig.dat, aes(x=year, y=Value, group=year)) + 
# geom_boxplot() + theme_cowplot() + facet_wrap(~Parameter) 
###############################################################################
#STEP 3: CORRELATIONS - NEAH BAY SPECIES COUNTS VS ENVIRONMENTAL PARAMETERS
###############################################################################

#This code is to compare our fish counts against environmental parameters over time.
#Years of interest: 2005-2023

###############################################################################
#PREP WORK
###############################################################################

#Load libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggpubr)
library(cowplot)

#Load data files
setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBay/Seattle_Aquarium_Neah_Bay_subtidal_monitoring")
fish.dat <- read_csv("data_input/Neah_Bay_data.csv")
#use environmental database created by "Env_Cleaning" and "Env_Graphing" r scripts.

#prep both dataframes for merging
fish.dat <- fish.dat %>%
  filter(Direction == "Forward") %>%
  filter(Transect %in% c("T1") ) %>%
  pivot_longer("Black":"Puget_Sound", names_to = "Species", values_to = "Count")

fish.dat <- fish.dat %>% 
  select(-c(Transect, Direction)) %>%
  group_by(Year, Species) %>%
  mutate(Avg_Count=mean(Count)) %>%
  select(-c(Site, Count)) %>%
  unique()

fish.dat <- fish.dat %>%
  pivot_wider(names_from=Species, values_from=Avg_Count)

env.dat <- env_dat %>%
  select(-c(Latitude.DegMin:Longitude.Deg, DATE_UTC:Oxygen.Concentration.MG.1)) %>%
  mutate(Year=year(date)) %>%
  pivot_longer("Temperature":"Chlor_a", names_to = "Parameter", values_to="Value") %>%
  group_by(Year, Parameter)%>%
  mutate(Avg_Value=mean(Value, na.rm=TRUE)) %>%
  select(c(Year,Parameter,Avg_Value)) %>%
  unique() %>%
  pivot_wider(names_from=Parameter, values_from=Avg_Value)

#merge dataframes
dat<-merge(env.dat,fish.dat,by=c("Year"),all=TRUE)
dat <- filter(dat, Year >=2005)

#Housekeeping
rm(env_dat, env.dat, fish.dat)

#scatterplotting
shapes <- c("Greenling" = 20, "Canary" = 2, "Quillback" = 1, "Yelloweye" = 0, "Chlor_a" = 8)

ggplot(dat, aes(x=Year)) +
  geom_point(aes(y=Greenling, shape="Greenling"), size=5) + geom_smooth(aes(y=Greenling), color = "grey", se=FALSE) +
  geom_point(aes(y=Canary, shape="Canary"), size=5) + geom_smooth(aes(y=Canary), color = "grey", se=FALSE) +
  geom_point(aes(y=Quillback, shape="Quillback"), size=5) + geom_smooth(aes(y=Quillback), color = "grey", se=FALSE) +
  geom_point(aes(y=Yelloweye, shape="Yelloweye"), size=5) + geom_smooth(aes(y=Yelloweye), color = "grey", se=FALSE) +
  geom_point(aes(y=Chlor_a, shape="Chlor_a"), size=5) + geom_smooth(aes(y=Chlor_a), se=FALSE) +
  theme_cowplot() + labs(shape="Legend", y="Value", x="Year") + scale_shape_manual(values=shapes) +
  scale_y_continuous(trans="log2")

#make individual plots

ggplot(dat, aes(x=Year)) +
  geom_point(aes(y=Greenling), size=4, color="orange") +
  geom_point(aes(y=Canary), size=4, color="blue") + 
  geom_point(aes(y=Quillback), size=4, color="skyblue") +
  geom_point(aes(y=Yelloweye), size=4, color="gray8") + 
  geom_point(aes(y=YOY), size=4, color="violetred") + 
  theme_cowplot() + labs(shape="Legend", y="Count", x="Year") +
  geom_smooth(aes(y=Chlor_a), se=FALSE) +
  scale_y_continuous(trans="log2")

p1 <- ggplot(dat, aes(x=Year)) +
  geom_point(aes(y=Greenling), size=3) +
  theme_cowplot() + labs(y="Mean count") +
  geom_smooth(aes(y=Chlor_a), se=FALSE) +
  scale_y_continuous(trans="log2", sec.axis=dup_axis(name="Chlor a (mg/L)")) +
  annotate("text", x=2010, y=1.5, label="Tau= -0.6") +
  annotate("text", x=2010, y=1, label="p=0.017*") +
  ggtitle("A. Greenling") +
  theme(axis.title.y.right=element_text(color="blue"), 
        axis.text.y.right=element_text(color="blue"),
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=-0.2))


p2 <- ggplot(dat, aes(x=Year)) +
  geom_point(aes(y=Canary), size=3) + 
  theme_cowplot() + labs(y="Mean count", x="Year") +
  geom_smooth(aes(y=Chlor_a), se=FALSE) +
  scale_y_continuous(trans="log2", sec.axis=dup_axis(name="Chlor a (mg/L)")) +
  annotate("text", x=2008, y=1.5, label="Tau= -0.46") +
  annotate("text", x=2008, y=1, label="p=0.070") +
  ggtitle("B. Canary") +
  theme(axis.title.y.right=element_text(color="blue"), 
        axis.text.y.right=element_text(color="blue"),
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=-0.2))


p3 <- ggplot(dat, aes(x=Year)) +
  geom_point(aes(y=Quillback), size=3) +
  theme_cowplot() + labs(y="Mean count", x="Year") +
  geom_smooth(aes(y=Chlor_a), se=FALSE) +
  scale_y_continuous(trans="log2", sec.axis=dup_axis(name="Chlor a (mg/L)")) +
  annotate("text", x=2009, y=1, label="Tau= -0.45") +
  annotate("text", x=2009, y=0.7, label="p=0.078") +
  ggtitle("C. Quillback") +
  theme(axis.title.y.right=element_text(color="blue"), 
        axis.text.y.right=element_text(color="blue"),
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=-0.2))

p4 <- ggplot(dat, aes(x=Year)) +
  geom_point(aes(y=Yelloweye), size=3) + 
  theme_cowplot() + labs(y="Mean count", x="Year") +
  geom_smooth(aes(y=Chlor_a), se=FALSE) +
  scale_y_continuous(trans="log2", sec.axis=dup_axis(name="Chlor a (mg/L)")) +
  annotate("text", x=2009, y=1.5, label="Tau= -0.471") +
  annotate("text", x=2009, y=1, label="p=0.089") +
  ggtitle("D. Yelloweye") +
  theme(axis.title.y.right=element_text(color="blue"), 
        axis.text.y.right=element_text(color="blue"),
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=-0.2))

ggarrange(p1,p2,p3,p4)

ggarrange(p1,p2,p3,p4,
          labels = c("A. Greenling", "B. Canary", "C. Quillback", "D. Yelloweye"),
          hjust=-0.1, vjust=0.05)

#Correlations
cor.DO <- cor.test(dat$Vermilion, dat$DO, method = "kendall")
print(cor.DO)

cor.temp <- cor.test(dat$Puget_Sound, dat$Temperature, method = "kendall")
print(cor.temp)

cor.chl <- cor.test(dat$Yelloweye, dat$Chlor_a, method = "kendall")
print(cor.chl)

#Results: close to sig and negative Chlor_a & Canary/Quillback/Yelloweye, 
#sig neg Chlor_a & Greenling
#sig pos DO & Vermilion

################################################################################
#END OF CODE
################################################################################