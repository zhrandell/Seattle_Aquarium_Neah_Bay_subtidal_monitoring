###############################################################################
#NEAH BAY YOY
###############################################################################

#This code is to test for significance of the peak in YOY counts in 2016.

###############################################################################
#PREP WORK
###############################################################################

#Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(FSA)
library(PMCMRplus)

#Load data files
setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBay/Seattle_Aquarium_Neah_Bay_subtidal_monitoring")
dat <- read_csv("data_input/Neah_Bay_data.csv")

################################################################################
##DATA CLEANING
################################################################################

## remove first column, Location 
dat <- dat[-1]

## removes "Site" from the Site column, leaving just the #
dat$Site <- gsub("^.{0,5}", "", dat$Site)


## removes empty characters (blanks spaces) from strings 
dat$Site <- gsub(" ", "", dat$Site)


## function to place the last column [, ncol] in the first column position i.e. [, 1]
front.ofthe.line <- function(dat){
  num.col <- ncol(dat)
  dat <- dat[c(num.col, 1:num.col-1)]
  return(dat)
}

## select desired data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## select either the Forward or Reverse data
## the data default to BOTH ... if you want both directions, don't run this function
select_direction <- function(x){
  out <- filter(dat, Direction %in% c(x))
  return(out)
}


## select the transects you desire to analyze
## the data default to ALL, i.e., T1, T2, T3, T4
select_transect <- function(x){
  out <- filter(dat, Transect %in% c(x))
}


## to select only Forward data = enter F
## to select only Reverse data = enter R 
## to select BOTH Forward and Reverse data ... don't run this function
dat <- select_direction("Forward")


## enter whatever combination of "T1", "T2", "T3", "T4" is desired
dat <- select_transect(c("T1"))

## END data selection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat <- dat %>%
  select(Year, Site, YOY) %>%
  group_by(Year) %>%
  mutate(Total=sum(YOY))

#Turn 'Year' into a categorical variable for statistical analysis
stat.dat <- dat
stat.dat$Year <- as.factor(stat.dat$Year)

################################################################################
##STATISTICAL ANALYSIS
################################################################################

#statistical test: Kruskal Wallace (non-parametric equivalent of one-way Anova)
kruskal.test(YOY ~ Year, data = stat.dat)

#diagnostics

#Dunn's Kruskal-Wallis post-hoc test for #Year
posthocs1<-dunnTest(YOY ~ Year, data=stat.dat, method="holm")
print(posthocs1)

#Results:

##RESULT: KW p-value=0.081 AKA marginally significant. 
#Dunn's posthocs test: 2010 vs. 2016 p=0.019

################################################################################
#OUTLIER DETECTION
################################################################################

#make boxplot to see number of suspected outliers
ggplot(dat) + aes(y = YOY) +
  geom_boxplot(alpha=0.5) + theme_minimal()

#determine values of outliers
boxplot.stats(dat$YOY)$out

boxplot.stats(dat$YOY) #120 is extreme of upper whisker

################################################################################
##VISUALIZATION
################################################################################

#Figure 5, 500x300
ggplot(dat, aes(x=Year, y=YOY)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() +
  ylab("Young-of-the-year count") +
  annotate("segment", x=2005, xend=2023, y=120, yend=120, color="gray", linewidth=0.5, linetype="dashed") #+
  #scale_y_continuous(trans="log2") #+
  #stat_compare_means(comparisons=my_comparisons)
  #geom_line(aes(group=Site, color=Site))

#Alternate to Figure 5
ggplot(dat, aes(x=Year, y=YOY)) +
  geom_point(size=5,alpha=0.25) +
  theme_cowplot() +
  ylab("Young-of-the-year count") +
  annotate("segment", x=2005, xend=2023, y=120, yend=120, color="red", linewidth=0.5, linetype="dashed")

ggsave("Fig5.tiff", plot=last_plot(), width=12, height=7, bg="white", dpi=600)

fig.dat <- dat %>%
  select(Year, Total) %>%
  unique()

my_comparisons <- list(c("2010", "2016"), c("2014", "2016"))

ggplot(dat, aes(x=Year, y=Total)) +
  geom_line(aes()) + 
  theme_cowplot() +
  ylab("Young-of-the-Year Rockfish") 
################################################################################
##END OF CODE
################################################################################