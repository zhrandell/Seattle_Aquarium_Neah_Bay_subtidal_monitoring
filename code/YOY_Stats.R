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
setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBayRockfish/Raw_data")

dat <- read_csv("new_Neah_Bay_data.csv")

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

##RESULT: KW p-value=0.086 AKA marginally significant. 
#Dunn's posthocs test: 2014 vs. 2016 p=0.084; 2010 vs. 2016 p=0.018

#Individual comparisons
test <- dat %>% 
  select(-Total) %>% 
  pivot_wider(names_from=Year,values_from = YOY)

#vectors for years with no sites removed
y05 <- c(3)
y06 <- c(38,60,50,0)
y07 <- c(0,1,0,0)
y08 <- c(0,0,0,348)
y09 <- c(0,4,0,30)
y10 <- c(0,0,0,0,0)
y11 <- c(20,0,0,170,0)
y12 <- c(105,50,0,281,0)
y13 <- c(400,120,0,250,0)
y14 <- c(10,0,0,0,0)
y15 <- c(160,10,0,65,0)
y16 <- c(1950,1150,160,1150)
y17 <- c(30,0,10,0,0)
y18 <- c(0,0,50,150,0)
y19 <- c(10,0,0,20,0)
y21 <- c(8,0,0,181)

wilcox.test(y13,y06,paired=FALSE)

################################################################################
##VISUALIZATION
################################################################################
ggplot(dat, aes(x=Year, y=YOY)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() +
  ylab("Young-of-the-year count") +
  scale_y_continuous(trans="log2") #+
  #stat_compare_means(comparisons=my_comparisons)
  #geom_line(aes(group=Site, color=Site))

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