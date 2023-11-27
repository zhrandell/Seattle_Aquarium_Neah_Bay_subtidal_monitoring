## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Seattle Aquarium long-term rockfish monitoring around Neah Bay, Washington  
## multivariate analyses community structure
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(tidyverse)
library(vegan)

## set your paths in a project folder 
input <- "C:/Users/randellz/Dropbox (Seattle Aquarium)/Coastal Complexity & Resilience Team Folder/GitHub/Seattle_Aquarium_Neah_Bay_subtidal_monitoring/data_input"
output <- "C:/Users/randellz/Dropbox (Seattle Aquarium)/Coastal Complexity & Resilience Team Folder/GitHub/Seattle_Aquarium_Neah_Bay_subtidal_monitoring/data_output"

setwd(input)
dat <- read.csv("Neah_Bay_data.csv", header=TRUE)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## tidy up data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## remove first column & rename col 
dat <- dat[-1]
colnames(dat)[5] <- 'Black & Deacon'

## removes "Site" from the Site column, leaving just the #
dat$Site <- gsub("^.{0,5}", "", dat$Site)


## removes empty characters (blanks spaces) from strings 
dat$Site <- gsub(" ", "", dat$Site)


## function to place the last column [, ncol] in the first column position i.e. [, 1]
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## create unique identifier for each combination of site / transect 
## (as all sites have T1, T2, etc.)
create.key <- function(data){
  data$Key <- data$Site
  data$Key <- with(data, paste0(Key, Transect))
  data <- front.ofthe.line(data)
  return(data)
}


## remove the "19" and "20" from year dates for easier visualization, e.g., "1991" --> "91" 
short.date <- function(data){
  data$short.date <- gsub("20","", as.character(data$Year))
  data <- front.ofthe.line(data)
  return(data)
}


## create unique identifyer for data w/ multiple transects
dat <- create.key(dat)


## create a shorter year identifier, e.g., "10" instead of "2010"
dat <- short.date(dat)
## END data tidying ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





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


## to select only Forward data = enter "Forward" 
dat <- select_direction("Forward")


## enter whatever combination of "T1", "T2", "T3", "T4" is desired
dat <- select_transect(c("T1"))
## END data selection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## drop YOY
dat <- subset(dat, select = -c(YOY))



## prep for NMDS analysis, transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create separate metadata and spp dataframes 
ncol_metadata <- 6
info <- dat[,1:ncol_metadata]
spp <- dat[,-(1:ncol_metadata)]



## check and see if any species have 0 observations
colSums(spp)


## remove columns with 0 observations
spp <- spp %>% select_if(negate(function(col) is.numeric(col) && sum(col) <=2))


## recheck to confirm columns were deleted
colSums(spp)
## END NMDS prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## perform multivariate analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ord <- metaMDS(comm = spp, distance="bray", k=2, min = 1000, trymax=2000, 
               autotransform = F, wascores = TRUE)


## save a new ordination 
setwd(output)
#setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBay/Seattle_Aquarium_Neah_Bay_subtidal_monitoring/data_output")
save(ord, file = "ord_T1_no_YOY.rda")


## work with ordination: stress, NMDS coords 
load("ord_T1_only.rda")


## visualize stress, check ordination, xy coordinates 
## open graphics window
graphics.off()
windows(6,6,record=T)


## plot
plot(ord)
stressplot(ord)



## overlay correlation with log species  
dist <- ord$dist
ord.points <- postMDS(ord$points, dist)
spp_scores <- as.data.frame(wascores(ord.points, spp))     
names(spp_scores)[1] <- "spp_x"
names(spp_scores)[2] <- "spp_y"
write.csv(spp_scores, "spp_scores_T1_no_YOY.csv")


## NMDS ordination coordinates saved as data frame
save.coords <- function(ord, info, spp){
  t1 <- as.data.frame(scores(ord))
  t2 <- cbind(t1, info, spp)
  return(t2)
}


## bind nmds coordinates to dataframe with site info and spp counts
NMDS_coords <- save.coords(ord.points, info, spp)


## save final output as CSV files for further analysis / visualization  ~~~~~~~~
setwd(output)
write.csv(NMDS_coords,'NMDS_coords_T1_no_YOY.csv')
## END save / load of final CSV output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
