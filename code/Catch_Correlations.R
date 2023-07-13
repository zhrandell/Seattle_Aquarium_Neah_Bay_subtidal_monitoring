###############################################################################
#NEAH BAY LANDINGS VS UNDERWATER OBSERVATIONS
###############################################################################

#This code is to compare our underwater observations with fisheries landings.
#Years of interest: 2005-2021

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
setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBayRockfish/Raw_data")

marine.dat <- read_csv("new_Neah_Bay_data.csv")
landings.dat <- read_csv("RecFIN_landings_2000to2022.csv")

#tweak databases so their formatting is ready for merging
landings.dat$Species <- tolower(landings.dat$Species)

landings.dat <- landings.dat %>% rename("RecFin_Count" = "Count")
landings.dat <- filter(landings.dat, Site == "NEAH BAY")

setnames(marine.dat, skip_absent = TRUE, 
         old = c("Site", "Transect", "Black", "Canary", "China", "Copper", "Quillback", "Tiger", 
                 "Widow", "Yellowtail", "Vermilion","Yelloweye", "Puget_Sound"), 
         new = c("SEAQ_Site", "SEAQ_Transect", "black rockfish", "canary rockfish", "china rockfish", "copper rockfish",
                 "quillback rockfish", "tiger rockfish", "widow rockfish",
                 "yellowtail rockfish", "vermilion rockfish", "yelloweye rockfish",
                 "puget sound rockfish"))

#make marine.dat tidy
marine.dat <- marine.dat %>%
  select(-"YOY") %>%
  pivot_longer("black rockfish":"puget sound rockfish", names_to = "Species", values_to = "Count")
marine.dat$Species <- tolower(marine.dat$Species)

marine.dat <- marine.dat %>% 
  group_by(Year, Species) %>%
  mutate(SEAQ_Count = sum(Count)) #sum count across sites to make total observations column

#filter for parameters of interest
marine.dat <- marine.dat %>% 
  filter(Direction == "Forward") %>%
  filter(SEAQ_Transect %in% c("T1") ) %>%
  select(-c(SEAQ_Site, Count)) %>%
  unique()

#combine databases to yield year, species, SEAQ_count, RecFIN_count as columns
dat <- merge(marine.dat, landings.dat, by = c("Year", "Species"), all.x = TRUE)

#make dataframe cute
dat["Site"][is.na(dat["Site"])] <- "NEAH BAY" #replace landings' datas empty rows with NEAH BAY LOCATION
dat["RecFin_Count"][is.na(dat["RecFin_Count"])] <- 0 #Add zero count where NAs for landings data with corresponding SEAQ values

#select desired columms
dat <- dat %>%
  select(Year, Location, Species, RecFin_Count, SEAQ_Count)

#plotting

ggplot(dat, aes(x=SEAQ_Count, y=RecFin_Count)) +
  geom_point() + geom_smooth(method="glm", se=FALSE) + theme_cowplot() + 
  xlab("Dive survey counts") + ylab("Creel landings") + 
  scale_y_continuous(trans="log2") + scale_x_continuous(trans="log2") +
  annotate("text", x=500, y=18, label="Tau=0.392  ")+
  annotate("text", x=500, y=10, label="p=2.2e-16**") #p=2.2e-16, tau=0.392 #https://www.statology.org/add-text-to-ggplot/#:~:text=You%20can%20use%20the%20annotate,text%20to%20plots%20in%20ggplot2.&text=where%3A,label%3A%20The%20text%20to%20display.

time.dat <- dat %>%
  pivot_longer("RecFin_Count":"SEAQ_Count", names_to = "Method", values_to = "Count")

ggplot(time.dat, aes(x=Year, y=Count, group=Method, color=Method)) + 
  geom_line() + theme_cowplot() + scale_y_continuous(trans="log2") + 
  facet_wrap(~Species)

#run correlations
cor.all <- cor.test(dat$SEAQ_Count, dat$RecFin_Count, method = "kendall")
print(cor.all) #significant positive p=2.2e-16, tau=0.392

lm <- lm(RecFin_Count ~ SEAQ_Count, data=dat)

black <- dat[which(dat$Species=="black rockfish"),]
cor.black <- cor.test(black$SEAQ_Count, black$RecFin_Count, method="kendall")
print(cor.black)
rm(black)

cab <- dat[which(dat$Species=="cabezon"),]
cor.cab <- cor.test(cab$SEAQ_Count, cab$RecFin_Count, method="kendall")
print(cor.cab)
rm(cab)

can <- dat[which(dat$Species=="canary rockfish"),]
cor.can <- cor.test(can$SEAQ_Count, can$RecFin_Count, method="kendall")
print(cor.can)
rm(can)

chi <- dat[which(dat$Species=="china rockfish"),]
cor.chi <- cor.test(chi$SEAQ_Count, chi$RecFin_Count, method="kendall")
print(cor.chi)
rm(chi)

cop <- dat[which(dat$Species=="copper rockfish"),]
cor.cop <- cor.test(cop$SEAQ_Count, cop$RecFin_Count, method="kendall")
print(cor.cop)
rm(cop)

green <- dat[which(dat$Species=="greenling"),]
cor.green <- cor.test(green$SEAQ_Count, green$RecFin_Count, method="kendall")
print(cor.green)
rm(green)

hal <- dat[which(dat$Species=="halibut"),]
cor.hal <- cor.test(hal$SEAQ_Count, hal$RecFin_Count, method="kendall")
print(cor.hal)
rm(hal)

ling <- dat[which(dat$Species=="lingcod"),]
cor.ling <- cor.test(ling$SEAQ_Count, ling$RecFin_Count, method="kendall")
print(cor.ling)
rm(ling)

puget <- dat[which(dat$Species=="puget sound rockfish"),]
cor.puget <- cor.test(puget$SEAQ_Count, puget$RecFin_Count, method="kendall")
print(cor.puget)
rm(puget)

quil <- dat[which(dat$Species=="quillback rockfish"),]
cor.quil <- cor.test(quil$SEAQ_Count, quil$RecFin_Count, method="kendall")
print(cor.quil)
rm(quil)

tig <- dat[which(dat$Species=="tiger rockfish"),]
cor.tig <- cor.test(tig$SEAQ_Count, tig$RecFin_Count, method="kendall")
print(cor.tig)
rm(tig)

#ver <- dat[which(dat$Species=="vermilion rockfish"),]
#cor.ver <- cor.test(ver$SEAQ_Count, ver$RecFin_Count, method="kendall")
#print(cor.ver)
#rm(ver)

wid <- dat[which(dat$Species=="widow rockfish"),]
cor.wid <- cor.test(wid$SEAQ_Count, wid$RecFin_Count, method="kendall")
print(cor.wid)
rm(wid)

wolf <- dat[which(dat$Species=="wolfeel"),]
cor.wolf <- cor.test(wolf$SEAQ_Count, wolf$RecFin_Count, method="kendall")
print(cor.wolf)
rm(wolf)

yeye <- dat[which(dat$Species=="yelloweye rockfish"),]
cor.yeye <- cor.test(yeye$SEAQ_Count, yeye$RecFin_Count, method="kendall")
print(cor.yeye)
rm(yeye)

ytail <- dat[which(dat$Species=="yellowtail rockfish"),]
cor.ytail <- cor.test(ytail$SEAQ_Count, ytail$RecFin_Count, method="kendall")
print(cor.ytail)
rm(ytail)