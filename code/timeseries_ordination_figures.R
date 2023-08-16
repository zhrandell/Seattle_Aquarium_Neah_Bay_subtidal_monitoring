## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Seattle Aquarium long-term rockfish monitoring around Neah Bay, Washington  
## data visualization and changepoint analysis 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())


library(tidyverse)
library(changepoint)
library(zoo)


## set your paths in a project folder 
input <- "D:/OneDrive/Active_Projects/Neah_Bay/data_input"
output <- "D:/OneDrive/Active_Projects/Neah_Bay/data_output"
code <- "D:/OneDrive/Active_Projects/Neah_Bay/code" 
fig <- "D:/OneDrive/Active_Projects/Neah_Bay/figures"

output <- "C:/Users/randellz/Dropbox (Seattle Aquarium)/Coastal Complexity & Resilience Team Folder/GitHub/Seattle_Aquarium_Neah_Bay_subtidal_monitoring/data_output"

setwd(output)
dat <- read.csv("NMDS_coords.csv")
#log_dat <- read.csv("log_NMDS_coords.csv")

dat <- dat[,-1]
#log_dat <- log_dat[,-1]
## END startp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat$short.date <- as.factor(dat$short.date)
dat$Site <- as.factor(dat$Site)


len <- nrow(dat)
dat$id <-seq(1:len) 


## calculate total abundance 
dat <- dat %>%
  rowwise(id) %>%
  mutate(total=sum(c(Black, Cabezon, Canary, China, Copper, Greenling, Lingcod, 
                     Quillback, Tiger, Widow, Yellowtail, Vermillion, Yelloweye)))


## filter by site to calculate site ellipses 
filter.site <- function(x){filter(dat, Site %in% c(x))}

S1 <- filter.site("1") 
S2 <- filter.site("2")
S3 <- filter.site("3")
S4 <- filter.site("4")
S5 <- filter.site("5")


## long-form data structure for time series plots 
long <- dat %>%
  gather("spp_name", "count",
         Black, Cabezon, Canary, China, Copper, Greenling, Lingcod, 
         Quillback, Tiger, Widow, Yellowtail, Vermillion, Yelloweye) %>%
  dplyr::arrange(desc(spp_name))
## END data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data prep for changepoint analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## sum data across sites
dat <- dat[, -c(23, 24)]


## avg data across sites
dat.avg <- dat %>%
  group_by(Year) %>%
  summarise(across(
    .cols = -c(1:7),
    .fns = list(avg=mean),
    .names = "{col}"))



## function to format data for changepoint 
change.pt <- function(col, spp){
  t1 <- dat.avg[, c(1, col)]
  t2 <- t1 %>% spread(Year, spp)
  t3 <- as.numeric(t2)
  out <- cpt.meanvar(t3, Q=5, method="AMOC")
  return(out)
}



Black <- change.pt(2, "Black")
Cabezon <- change.pt(3, "Cabezon")
Canary <- change.pt(4, "Canary")
China <- change.pt(5, "China")
Copper <- change.pt(6, "Copper")
Greenling <- change.pt(7, "Greenling")
Lingcod <- change.pt(8, "Lingcod")
Quillback <- change.pt(9, "Quillback")
Tiger <- change.pt(10, "Tiger")
Widow <- change.pt(11, "Widow")
Yellowtail <- change.pt(12, "Yellowtail")
YOY <- change.pt(13, "YOY")
Vermillion <- change.pt(14, "Vermillion")
Yelloweye <- change.pt(15, "Yelloweye")


## plot changepoints 
plot.changepts <- function(spp, full_spp, title){
  offset <- 0.001
  zero <- 2005
  x1 <- spp@cpts[[1]]
  x2 <- spp@cpts[[2]]
  y1 <- spp@param.est[["mean"]][1]
  y2 <- spp@param.est[["mean"]][2]
  
  p1 <- ggplot(dat.avg, aes(Year, full_spp)) + geom_path() + geom_point() + my.theme + x.lab + x.breaks +
    geom_rect(data=dat.avg, aes(xmin=zero, xmax=zero+x1, ymin=y1-offset, ymax=y1+offset), color="red") +
    geom_rect(data=dat.avg, aes(xmin=zero+x1, xmax=zero+x2, ymin=y2-offset, ymax=y2+offset), color="red") +
    theme(axis.title.x=element_blank()) + ylab("Site averaged abundance") + ggtitle(title)
  return(p1)
}


p.black <- plot.changepts(Black, dat.avg$Black, "Black & Deacon Rockfish")
print(p.black)

p.china <- plot.changepts(China, dat.avg$China, "China Rockfish")
print(p.china)

p.greenling <- plot.changepts(Greenling, dat.avg$Greenling, "Rock greenling")
print(p.greenling)

p.canary <- plot.changepts(Canary, dat.avg$Canary, "Canary Rockfish")
print(p.canary)

p.copper <- plot.changepts(Copper, dat.avg$Copper, "Copper Rockfish")
print(p.copper)

p.quill <- plot.changepts(Quillback, dat.avg$Quillback, "Quillback Rockfish")
print(p.quill)

p.YOY <- plot.changepts(YOY, dat.avg$YOY, "Young of the Year rockfish")
print(p.YOY)

p.ling <- plot.changepts(Lingcod, dat.avg$Lingcod, "Lingcod")
print(p.ling)

p.vermillion <- plot.changepts(Vermillion, dat.avg$Vermillion, "Vermillion Rockfish")
print(p.vermillion)

p.yellowEye <- plot.changepts(Yelloweye, dat.avg$Yelloweye, "Yelloweye Rockfish")
print(p.yellowEye)

p.yellowtail <- plot.changepts(Yellowtail, dat.avg$Yellowtail, "Yellowtail Rockfish")
print(p.yellowtail)

p.widow <- plot.changepts(Widow, dat.avg$Widow, "Widow Rockfish")
print(p.widow)





## custom graphical parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#no.x.title <- theme(axis.title.x = element_blank())
#strip.text <- theme(strip.text.x = element_text(size=14))
#no.strip <- theme(strip.background = element_blank(), strip.text.x = element_blank())
## END custom graphical params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot each individual site through time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graphics.off()
windows(15,8, record=T)
text.size = 4


plot.all.sites <- function(x){
  t1 <- ggplot(data=x, aes(x=NMDS1, y=NMDS2, color=Site, group = Site)) +
    geom_point() + geom_path() + coord_fixed() + my.cols + no.legend + my.theme +
    geom_text(label=dat$short.date, size=text.size, color="black") +
    facet_wrap(~Site, nrow=2)
  return(t1)
}


p1 <- plot.all.sites(dat)
print(p1)
## END plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot all five sites in single ordination with ellipses ~~~~~~~~~~~~~~~~~~~~~~
graphics.off()
windows(10.5, 8, record=T)
pt.size <- 2


plot.ord <- function(data, S1, S2, S3, S4, S5){
  t2 <- ggplot(data, aes(x = NMDS1, y = NMDS2)) + my.theme + coord_fixed() + 
    geom_point(aes(color=Site), size=pt.size) + 
    scale_color_manual(values=site.cols) + ylab("NMDS Axis-2") + xlab("NMDS Axis-1") +
    stat_ellipse(data=S1, aes(x=NMDS1, y=NMDS2), col=site.cols[1]) +
    stat_ellipse(data=S2, aes(x=NMDS1, y=NMDS2), col=site.cols[2]) +
    stat_ellipse(data=S3, aes(x=NMDS1, y=NMDS2), col=site.cols[3]) + 
    stat_ellipse(data=S4, aes(x=NMDS1, y=NMDS2), col=site.cols[4]) +
    stat_ellipse(data=S5, aes(x=NMDS1, y=NMDS2), col=site.cols[5])  
  return(t2)
}

p2 <- plot.ord(dat, S1, S2, S3, S4, S5)
print(p2)
## END plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize correlation coefficients
graphics.off()
windows(10.5, 8, record=T)


setwd(output)
spp_scores <- read.csv("spp_scores_noYOY.csv", header=T)
names(spp_scores)[1]<-"spp_name"


pt.type <- 21
pt.col <- "black"
pt.fill <- "red"
text.col <- "red"


plot.spp.correlations <- function(x){
  t1 <- ggplot(data=dat, aes(NMDS1, NMDS2)) + my.theme + coord_fixed() +
    geom_point(data=dat, aes(NMDS1, NMDS2), pch=pt.type, fill="gray", col="gray") +
    geom_segment(data=spp_scores, aes(x=spp_x, y=spp_y, xend=0, yend=0)) + 
    geom_point(data=spp_scores, aes(x=spp_x, y=spp_y)) +
    geom_text(data=spp_scores, aes(x=spp_x, y=spp_y, label=spp_name), color=text.col)
  return(t1)
}


p3 <- plot.spp.correlations(dat)  
print(p3)
## END plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot time series for all species ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graphics.off()
windows(14, 10, record=T)


plot.all.spp <- function(x){
  t1 <- ggplot(long, aes(x=Year, y=count, group=Key, color=Key)) + my.theme +
    geom_point() + geom_path() + my.cols + no.legend + x.lab + x.breaks + ylab("spp abundance") +
    facet_wrap(~spp_name, scales="free_y") 
  return(t1)
}


p4 <- plot.all.spp(dat)
print(p4)
## END plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot time series of total abundance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graphics.off()
windows(8, 5, record=T)


plot.total.abun <- function(x){
  t1 <- ggplot(dat, aes(x=Year, y=total, group=Key, color=Key)) + my.theme + no.legend +
    geom_point() + geom_path() + my.cols + ylab("Total (log10) abundance per site") +
    x.lab + x.breaks
  return(t1)
}

p5 <- plot.total.abun(dat)
print(p5)
## END plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## change-point detection experimentation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(1)
x=c(rnorm(100,0,1),rnorm(100,0,10))
ansvar=cpt.var(x)
plot(ansvar)
print(ansvar) # identifies 1 changepoint at 100


# change in mean
y=c(rnorm(100,0,1),rnorm(100,5,1))
ansmean=cpt.mean(y)
plot(ansmean,cpt.col='blue')
print(ansmean)

# change in mean and variance
z=c(rnorm(100,0,1),rnorm(100,2,10))
ansmeanvar=cpt.meanvar(z)
plot(ansmeanvar,cpt.width=3)
print(ansmeanvar)
## END change-point detection experimentation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


