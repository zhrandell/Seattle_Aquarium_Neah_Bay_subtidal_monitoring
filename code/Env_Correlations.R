###############################################################################
#NEAH BAY SPECIES COUNTS VS ENVIRONMENTAL PARAMETERS
###############################################################################

#This code is to compare our fish counts against environmental parameters over time.
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

fish.dat <- read_csv("new_Neah_Bay_data.csv")
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