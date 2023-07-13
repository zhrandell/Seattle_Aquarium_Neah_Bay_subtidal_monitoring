###############################################################################
#NEAH BAY ENVIRONMENTAL PLOTTING
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

#plot 2: environmental parameters overall
#ggplot(data = fig.dat, aes(x=year, y=Value, group=year)) + 
 # geom_boxplot() + theme_cowplot() + facet_wrap(~Parameter) 