#Step 1: load libraries---------------------------------------------------------
library(tidyverse)
library(vegan)

#Step 2: import Neah_Bay_data csv and create dat frame -------------------------
dat <- Neah_Bay_data
#dat <- dat %>% 
 # filter(Year < 2022) #use to recreate original figure, otherwise skip to include 2022-2023

#Step 3: tidy and filter data --------------------------------------------------
dat <- dat[-1] ## remove first column, which codes for site location
dat$Site <- gsub("^.{0,5}", "", dat$Site)## removes "Site" from the Site column, leaving just the #
dat$Site <- gsub(" ", "", dat$Site) ## removes empty characters (blanks spaces) from strings 
dat <- dat %>%
  filter(Transect=="T1") %>%
  filter(Year > 2009)
dat <- subset(dat, select = -c(YOY)) #drop YOY if desired

#Step 4: Make data tweaks for better visuals in graphing -----------------------

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

#Step 5: Set up for analysis ---------------------------------------------------
ncol_metadata <- 6 #number of columns with metadata
info <- dat[,1:ncol_metadata] #metadata dataframe
spp <- dat[,-(1:ncol_metadata)] #species counts dataframe
spp <- spp %>% 
  select_if(negate(function(col) is.numeric(col) && sum(col) <=0)) #removes columns with 0 observations

#Step 6: Run analysis ----------------------------------------------------------
ord <- metaMDS(comm = spp, distance="bray", k=2, min = 1000, trymax=2000, 
               autotransform = F, wascores = TRUE)

## overlay correlation with log species  
dist <- ord$dist
ord.points <- postMDS(ord$points, dist)
spp_scores <- as.data.frame(wascores(ord.points, spp))     
names(spp_scores)[1] <- "spp_x"
names(spp_scores)[2] <- "spp_y"

## NMDS ordination coordinates saved as data frame
save.coords <- function(ord, info, spp){
  t1 <- as.data.frame(scores(ord))
  t2 <- cbind(t1, info, spp)
  return(t2)
}

## bind nmds coordinates to dataframe with site info and spp counts
NMDS_coords <- save.coords(ord.points, info, spp)

#Step 7: Visualizations --------------------------------------------------------

#data set-up
dat <- NMDS_coords
dat$short.date <- as.factor(dat$short.date)
dat$Site <- as.factor(dat$Site)
len <- nrow(dat)
dat$id <-seq(1:len) #add column with row/data point numbers

#ggplot parameter set up
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 plot.title = element_text(size=14), 
                 axis.text=element_text(size=12),
                 axis.title=element_text(size=13))

legend.theme = theme(legend.title = element_text(size=13), 
                     legend.text = element_text(size=13))

no.legend = theme(legend.position = "none")

## specify hex codes for custom colors
site.cols <- c(
  "#FFA824",  # site 1
  "#87CEEB",  # site 2
  "#32CC99",  # site 5
  "#DE85B1",  # site 4
  "#36648B"   # site 5
)

## custom graphical params
my.cols <- scale_color_manual(values=site.cols)
txt.angle <- 45
x.lab <- theme(axis.text.x = element_text(angle = txt.angle, hjust=1)) 
x.breaks <- scale_x_continuous(breaks=seq(2005,2021,by=1))
no.leg <- theme(legend.position = "none")
text.size = 4

#graphing
graphics.off()
windows(15,8, record=T)

plot.all.sites <- function(x){
  t1 <- ggplot(data=x, aes(x=MDS1, y=MDS2, color=Site, group = Site)) +
    geom_point() + geom_path() + coord_fixed() + my.cols + no.legend + my.theme +
    geom_text(label=dat$short.date, size=text.size, color="black") +
    facet_wrap(~Site, nrow=2)
  return(t1)
}


p1 <- plot.all.sites(dat)
print(p1)