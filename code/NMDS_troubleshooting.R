#Step 1: load libraries---------------------------------------------------------
library(tidyverse)
library(vegan)
library(ggpubr)#graphing
library(cowplot)#graphing
library(here)

#Step 2: import Neah_Bay_data csv and create dat frame -------------------------
dat <- read_csv(here("./data_input/Neah_Bay_data.csv"))
#dat <- dat %>% 
 # filter(Year < 2022) #use to recreate original figure, otherwise skip to include 2022-2023

#Step 3: tidy and filter data --------------------------------------------------
dat <- dat[-1] ## remove first column, which codes for site location
dat$Site <- gsub("^.{0,5}", "", dat$Site)## removes "Site" from the Site column, leaving just the #
dat$Site <- gsub(" ", "", dat$Site) ## removes empty characters (blanks spaces) from strings 
dat <- dat %>%
  filter(Transect=="T1") %>%
  filter(Year > 2009)
dat_no_YOY <- subset(dat, select = -c(YOY)) #drop YOY if desired

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
dat_no_YOY <- create.key(dat_no_YOY)

## create a shorter year identifier, e.g., "10" instead of "2010"
dat <- short.date(dat)
dat_no_YOY <- short.date(dat_no_YOY)

#Step 5: Set up for analysis ---------------------------------------------------
ncol_metadata <- 6 #number of columns with metadata
info <- dat[,1:ncol_metadata] #metadata dataframe
spp <- dat[,-(1:ncol_metadata)] #species counts dataframe
spp <- spp %>% 
  select_if(negate(function(col) is.numeric(col) && sum(col) <=0)) #removes columns with 0 observations

info_no_YOY <- dat_no_YOY[,1:ncol_metadata] #metadata dataframe
spp_no_YOY <- dat_no_YOY[,-(1:ncol_metadata)] #species counts dataframe
spp_no_YOY <- spp_no_YOY %>% 
  select_if(negate(function(col) is.numeric(col) && sum(col) <=0))

#Step 6: Run analysis ----------------------------------------------------------
ord <- metaMDS(comm = spp, distance="bray", k=2, min = 1000, trymax=2000, 
               autotransform = F, wascores = TRUE)

ord_no_YOY <- metaMDS(comm = spp_no_YOY, distance="bray", k=2, min = 1000, trymax=2000, 
               autotransform = F, wascores = TRUE)

## overlay correlation with log species  
dist <- ord$dist
ord.points <- postMDS(ord$points, dist)
spp_scores <- as.data.frame(wascores(ord.points, spp))     
names(spp_scores)[1] <- "spp_x"
names(spp_scores)[2] <- "spp_y"

dist_no_YOY <- ord_no_YOY$dist
ord.points_no_YOY <- postMDS(ord_no_YOY$points, dist_no_YOY)
spp_scores_no_YOY <- as.data.frame(wascores(ord.points_no_YOY, spp_no_YOY))     
names(spp_scores_no_YOY)[1] <- "spp_x"
names(spp_scores_no_YOY)[2] <- "spp_y"

## NMDS ordination coordinates saved as data frame
save.coords <- function(ord, info, spp){
  t1 <- as.data.frame(scores(ord))
  t2 <- cbind(t1, info, spp)
  return(t2)
}

save.coords_no_YOY <- function(ord_no_YOY, info_no_YOY, spp_no_YOY){
  t1 <- as.data.frame(scores(ord_no_YOY))
  t2 <- cbind(t1, info_no_YOY, spp_no_YOY)
  return(t2)
}

## bind nmds coordinates to dataframe with site info and spp counts
NMDS_coords <- save.coords(ord.points, info, spp)
NMDS_coords_no_YOY <- save.coords_no_YOY(ord.points_no_YOY, info_no_YOY, spp_no_YOY)

#Step 7: Visualizations --------------------------------------------------------

#data set-up (with YOY)
dat <- NMDS_coords
dat$short.date <- as.factor(dat$short.date)
dat$Site <- as.factor(dat$Site)
len <- nrow(dat)
dat$id <-seq(1:len) #add column with row/data point numbers

#data set-up (no YOY)
dat_no_YOY <- NMDS_coords_no_YOY
dat_no_YOY$short.date <- as.factor(dat_no_YOY$short.date)
dat_no_YOY$Site <- as.factor(dat_no_YOY$Site)
len <- nrow(dat_no_YOY)
dat_no_YOY$id <-seq(1:len)

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

# #Step 8: Additional NMDS Analyses and visualizations----------------------------
# 
# ##code below is from Puget Sound rockfish manuscript; looking into disper function
# ###########to compare dispersal/centroids of the data with vs. without YOY
# 
# #load data
# data <- read.csv(here("./data_output/NMDS_coords_T1_no_YOY.csv"))
# data <- data[,-1]
# 
# ## filter by season to calculate season ellipses 
# filter.basin <- function(x){filter(data, Location %in% c(x))}
# 
# Admiralty <- filter.basin("Admiralty Inlet")
# Central <- filter.basin("Central Puget Sound")
# Hood <- filter.basin("Hood Canal")
# South <- filter.basin("South Puget Sound")
# 
# ## analysis of dispersion
# ## function to calculate homogeneity of dispersion 
# disper.f <- function(grouping, spp){
#   group_location <- grouping
#   dist <- vegdist(spp, method="bray")
#   disp <- betadisper(dist, group=grouping, type="centroid")
#   t1 <- anova(disp)
#   return(t1)
# }
# 
# ## calculate homogeneity of dispersion
# 
# basin_1 <- arrange(filter.basin(c("Admiralty Inlet","Central Puget Sound")), Location)
# basin_2 <- arrange(filter.basin(c("Admiralty Inlet","Hood Canal")), Location)
# basin_3 <- arrange(filter.basin(c("Central Puget Sound","Hood Canal")), Location)
# basin_4 <- arrange(filter.basin(c("Admiralty Inlet", "South Puget Sound")), Location)
# basin_5 <- arrange(filter.basin(c("Central Puget Sound", "South Puget Sound")), Location)
# basin_6 <- arrange(filter.basin(c("Hood Canal", "South Puget Sound")), Location)
# 
# d1 <- disper.f(basin_1$Location, basin_1[, 9:19])
# d2 <- disper.f(basin_2$Location, basin_2[, 9:19])
# d3 <- disper.f(basin_3$Location, basin_3[, 9:19])
# d4 <- disper.f(basin_4$Location, basin_4[, 9:19])
# d5 <- disper.f(basin_5$Location, basin_5[, 9:19])
# d6 <- disper.f(basin_6$Location, basin_6[, 9:19])
# 
# d1 #AI vs. CPS: p=0.75
# d2 #AI vs. HC: p<0.01
# d3 #CPS vs. HC: p<0.01
# d4 #AI vs. SPS: p<0.001
# d5 #CPS vs. SPS: p<0.001
# d6 #HC vs. SPS: p<0.001
# 
# #HOUSEKEEPING
# rm(d1,d2,d3,d4,d5,d6)
# 
# ## data conversion / filtering
# ## define different cols for convert.factor function 
# col.all <- c("Date", "Site", "Location", "Season", "Month", "Year")
# 
# ## function to convert columns to factors
# convert.factor <- function(data, col_X){
#   data[col_X] <- lapply(data[col_X], as.factor)
#   return(data)
# }
# 
# ## make the conversion 
# data <- convert.factor(data, col.all)
# 
# ## create dataframe with spp only 
# data_spp <- data[, -c(1:8)]  
# 
# #Graphing parameters
# my.theme = theme(panel.grid.major = element_blank(),
#                  panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), 
#                  axis.line = element_line(colour = "black"),
#                  plot.title = element_text(size=14), 
#                  axis.text=element_text(size=12),
#                  axis.title=element_text(size=14))
# 
# legend.theme = theme(legend.title = element_text(size=14), 
#                      legend.text = element_text(size=14))
# 
# no.legend = theme(legend.position = "none")
# 
# basin.cols<- c("Admiralty Inlet" = "gray40", "Central Puget Sound" = "gray60", "Hood Canal" = "black", "South Puget Sound"="gray20")
# basin.shapes <- c("Admiralty Inlet" = 0, "Hood Canal" = 16, "Central Puget Sound" = 17, "South Puget Sound"=6)
# #basin.cols <- c(
# # "gray30",  #Admiralty
# #"gray60",  #Central
# #"black")  #Hood 
# 
# 
# #Create graph
# graphics.off()
# windows(10.5, 7, record=T)
# pt.size <- 2
# 
# plot.ord <- function(data, Admiralty, Central, Hood, South){
#   t2 <- ggplot(data, aes(x = NMDS1, y = NMDS2)) + my.theme + coord_fixed() + 
#     scale_color_manual(values=basin.cols) + scale_shape_manual(values=basin.shapes) +
#     geom_point(aes(color=Location, shape=Location), size=pt.size) + legend.theme +
#     ylab("NMDS Axis-2") + xlab("NMDS Axis-1") + theme(legend.position="none") +
#     stat_ellipse(data=Admiralty, aes(x=NMDS1, y=NMDS2), col=basin.cols[1]) +
#     stat_ellipse(data=Central, aes(x=NMDS1, y=NMDS2), col=basin.cols[2]) +
#     stat_ellipse(data=Hood, aes(x=NMDS1, y=NMDS2), col=basin.cols[3]) +
#     stat_ellipse(data=South, aes(x=NMDS1, y=NMDS2), col=basin.cols[4]) +
#     annotate("text", x=1.5, y=0.75, label="Pr(>F)<0.001") +
#     annotate("segment",x=1.45,xend=1.45,y=0.4,yend=0.65,linetype="dashed") +
#     annotate("segment",x=0.82,xend=1.4,y=1,yend=0.82,linetype="dashed")
#   return(t2)
# }
# 
# p2 <- plot.ord(data, Admiralty, Central, Hood, South)
# print(p2 + xlab("NMDS Axis−1") + ylab("NMDS Axis−2"))
# p3 <- p2 + xlab("NMDS Axis−1") + ylab("NMDS Axis−2")
# 
# ggarrange(comm, p3, nrow=1, labels=c("A", "B"), widths=c(1.75,1), label.x=0.1)
# #1225x450
# 
# #Statistical analysis: 
# perm.all <- function(data, spp){
#   t1 <- how(nperm=10000)
#   setBlocks(t1) <- with(data, Location)
#   permanova.1 <- adonis2(spp ~ Location, 
#                          by="terms", data=data, methods="bray", perm=10000)
#   return(permanova.1)
# }
# 
# p1 <- perm.all(data, data_spp)
# print(p1)
# 
# ##Determine if Admiralty Inlet and Central Puget Sound (aka only 2 that don't have significantly different dispersions) have significantly different centroids
# 
# onlyNS <- data %>%
#   filter(Location %in% c("Admiralty Inlet", "Central Puget Sound"))
# 
# which(data$Location=="Admiralty Inlet")
# which(data$Location=="Central Puget Sound")
# 
# NS_spp <- spp[c(1:10, 12, 14:16, 19, 24:26, 30:32, 35, 36, 38, 39, 41:43, 49:57, 61:63,
#                 67, 68, 69:72, 76, 77, 81:86, 87, 88, 92, 94, 95, 97:99, 101, 103, 104,
#                 107:110, 112:114, 119, 120, 123:127, 131, 132, 135, 138, 139, 140:147),]
# 
# p2 <- perm.all(onlyNS, NS_spp)
# print(p2)
# 
# #Housekeeping
# rm(Admiralty, basin_1, basin_2, basin_3, Central, comm, data_spp, Hood, perm.all)
# rm(legend.theme, my.theme, no.legend, p1, p2, spp, code, col.all, fig, input)
# rm(output, pt.size, convert.factor, disper.f, filter.basin, log_transform, plot.ord)
# rm(onlyNS, NS_spp)