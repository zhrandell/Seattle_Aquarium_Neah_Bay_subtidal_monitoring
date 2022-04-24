## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Seattle Aquarium long-term rockfish monitoring around Neah Bay, Washington  
## multivariate analyses community structure
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(tidyverse)
library(vegan)

## set your paths in a project folder 
input <- "D:/OneDrive/Active_Projects/Neah_Bay/data_input"
output <- "D:/OneDrive/Active_Projects/Neah_Bay/data_output"
code <- "D:/OneDrive/Active_Projects/Neah_Bay/code" 
fig <- "D:/OneDrive/Active_Projects/Neah_Bay/figures"

setwd(output)
dat <- read.csv("coords.csv")
dat <- dat[,-1]
## END startp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



dat$short.date <- as.factor(dat$short.date)
dat$Site <- as.factor(dat$Site)



## custom graphical parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 plot.title = element_text(size=14))

legend.theme = theme(legend.title = element_text(size=13), 
                     legend.text = element_text(size=13)) 

no.axes = theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size=13))

left = theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.title.y = element_text(size=14),
             axis.text.y = element_text(size=13))

bottom = theme(axis.title.x = element_blank(),
               axis.text.x = element_text(size=11.5),
               axis.title.y = element_blank(),
               axis.text.y = element_text(size=13))


## specify hex codes for custom colors
site.cols <- c(
  "#6497b1",  #gray; site 1
  "#008080",  #teal; site 2
  "#03396c",  #dark blue; site 5
  #"#7BBF6A",  #light green; site 3
  "#3D8B37",  #dark green; site 4
  #"#CC6677",  #light maroon; site 6 
  "#882255"   #maroon; site 7
)


## custom graphical params
txt.angle <- 45
x.lab <- theme(axis.text.x = element_text(angle = txt.angle, hjust=1)) 
x.breaks <- scale_x_continuous(breaks=seq(2009,2019,by=1))
no.leg <- theme(legend.position = "none")
no.x.title <- theme(axis.title.x = element_blank())
strip.text <- theme(strip.text.x = element_text(size=14))
no.strip <- theme(strip.background = element_blank(), strip.text.x = element_blank())
## END custom graphical params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



windows(10,2, record=T)

text.size = 5


p1 <- ggplot(data=dat, aes(x=NMDS1, y=NMDS2, color=Site, group = Site)) + my.theme +
  geom_point() + geom_path() +
  geom_text(label=dat$short.date, size=text.size, color="black") +
  facet_wrap(~Site, nrow=1) + coord_fixed()
  #scale_color_manual(values=site.cols)
print(p1)





