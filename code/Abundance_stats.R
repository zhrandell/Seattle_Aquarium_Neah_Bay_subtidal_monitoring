###############################################################################
#NEAH BAY CHANGES IN ABUNDANCE
###############################################################################

#This code is to evaluate our abundance data for significant changes over time.
#Methods: Changepoint analysis and PCA
#Years of interest: 2005-2023

###############################################################################
#PREP WORK
###############################################################################

#Load libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

#Load data
setwd("C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/NeahBay/Seattle_Aquarium_Neah_Bay_subtidal_monitoring")
dat <- read_csv("data_input/Neah_Bay_data.csv")

#Tidy data
dat <- dat %>% 
  filter(Transect=="T1") %>%
  select(-c(Direction, Location, Transect))

long.dat <- dat %>% 
  pivot_longer(cols = c(3:19), names_to = "Species", values_to = "Count")

RF.dat <- dat %>%
  select(-c(Cabezon, Greenling, Halibut, Lingcod, Wolfeel, YOY)) %>%
  rowwise() %>%
  mutate(Total = sum(c(Black, Canary, China, Copper, Puget_Sound, Quillback, 
                       Tiger, Vermillion, Widow, Yelloweye, Yellowtail))) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(Avg = mean(Total)) %>%
  select(c(Year, Avg)) %>%
  unique() #create database with just rockfish, including a column with total and average totals

RF.long.dat <- long.dat %>%
  filter(Species %in% c("Black", "Canary", "China", "Copper", "Puget_Sound", 
                        "Quillback", "Tiger", "Vermillion", "Widow", "Yelloweye",
                        "Yellowtail")) #create a tidy version for ggplot2

#Visualize species of interest

data <- RF.long.dat %>%
  filter(Species %in% c("Quillback", "Canary", "Yellowtail","Copper", "Vermillion"))

ggplot(data=data, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + facet_wrap(~Species)

################################################################################
#PRELIMINARY VISUALIZATIONS (FIGURE 2-3)
################################################################################
test <- long.dat %>% 
  filter(Species!="YOY")%>%
  group_by(Year,Site)%>%
  mutate(Total=sum(Count))

#Figure 2 OPTION 2- keep. plot at 750x400
ggplot(data=test, aes(x=Year,y=Total,group=Site, fill=Site)) +
  geom_point(size=2, alpha=0.5, aes(shape=Site)) +
  geom_smooth(method="lm",se=FALSE, linewidth=0.75, color="gray25") +
  scale_shape_manual(values=c(21,22,23,24,25))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  facet_wrap(~Site) +
  ylab("Total adult fish")+
  theme_cowplot()+
  theme(legend.position="none")

#Figure 3
test2 <- test %>%
  filter(Species != "Halibut") %>%
  filter(Species != "Wolfeel") %>%
  filter(Species != "Puget_Sound") #remove counts less than 2

#plot at 1500x1000
#Option 3- keep
descending <- c("Black_Deacon","Widow","Greenling","Yellowtail","Canary","China","Lingcod","Quillback","Copper","Cabezon","Tiger","Vermilion","Yelloweye")

ggplot(data=test2,aes(x=Year,y=Count)) +
  geom_point(size=2.5, alpha=0.5, aes(shape=Site,group=Site, fill=Site))+
  geom_smooth(method="lm",se=FALSE, linewidth=0.75, color="gray25")+
  scale_shape_manual(values=c(21,22,23,24,25))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  facet_wrap(~factor(Species, levels=descending), scales="free",nrow=3) +
  theme_cowplot() +
  theme(text = element_text(size = 20))

################################################################################
#CHANGEPOINT ANALYSIS
################################################################################

##PREP WORK --------------------------------------------------------------------
#load libraries

library(changepoint.np)
library(changepoint)
library(bcp)

## avg data across sites
dat.avg <- dat %>%
  group_by(Year) %>%
  summarise(across(
    .cols = -c(1),
    .fns = list(avg=mean),
    .names = "{col}"))

#log transform data
#log_transform <- function(x){
#  out <- log10(x+1)
#  return(out)
#}

#dat.avg[2:18] <- log_transform(dat.avg[2:18])

#TEST FOR NORMALITY-------------------------------------------------------------

#1. visualize histograms
data <- dat.avg %>%
  pivot_longer(cols = c(2:18), names_to = "Species", values_to = "Avg")

ggplot(data, aes(x=Avg, color=Species)) +
  geom_histogram(aes(fill=Species)) +
  facet_wrap(~Species) +
  scale_x_continuous(trans="log2")

ggplot(RF.dat, aes(x=Avg)) +
  geom_histogram()

#2.visualize qq-plots
qqnorm(data$Avg)
qqline(data$Avg)

qqnorm(dat.avg$Canary)
qqline(dat.avg$Canary)

qqnorm(dat.avg$Copper)
qqline(dat.avg$Copper)

qqnorm(dat.avg$Quillback)
qqline(dat.avg$Quillback)

qqnorm(dat.avg$Vermilion)
qqline(dat.avg$Vermilion)

#qqnorm(RF.dat$Avg)
#qqline(RF.dat$Avg)

#3. Shapiro-Wilk normality test

shapiro.test(data$Avg) #non-normal
shapiro.test(dat.avg$Canary) #non-normal
shapiro.test(dat.avg$Copper) #non-normal
shapiro.test(dat.avg$Quillback) #non-normal
shapiro.test(dat.avg$Vermilion) #non-normal
#shapiro.test(RF.dat$Avg) #normal

#MAKE FUNCTIONS ---------------------------------------------------------------------

##function to calculate confidence intervals
#conf.cpt <- function(col){
#  t1 <- as.vector(dat.avg[col])
#  t2 <- as.numeric(unlist(t1))
#  out <- cpt.meanvar(t2, penalty="Asymptotic", pen.value=0.05, method="AMOC", class=FALSE)
#  return(out)
#}

##function to generate meanvar plots
#change.pt <- function(col, spp){
#  t1 <- dat.avg[, c(1, col)]
#  t2 <- t1 %>% spread(Year, spp)
#  t3 <- as.numeric(t2)
#  out <- cpt.meanvar(t3, Q=5)
#  return(out)
#}

##function to calculate changepoints non-parametrically
np.cpt <- function(col){
  t1 <- as.vector(dat.avg[col])
  t2 <- as.numeric(unlist(t1))
  out <- cpt.np(t2)
  return(out)
}

##function to calculate changepoints using bcp
#bcp.pt <- function(col) {
#  t1 <- as.vector(dat.avg[col])
#  t2 <- as.numeric(unlist(t1))
#  out <- bcp(t2)
#}

##DETERMINE CHANGEPOINTS--------------------------------------------------------

#list of excluded species, where there is no clear changepoint or changepoint is at the beginning or end: 
#Black(2), Cabezon(3), China(5), Greenling(7), Lingcod(8), Tiger(10), Widow(11),
#Wolfeel(12), Yellowtail(13), YOY(14), Halibut(16), Yelloweye(17), Puget_Sound(18)

#cCanary <- conf.cpt(4) #cpt @6, conf 0.999
#Canary <- change.pt(4, "Canary") #mean/var goes from 0/0 to 1.43/1.56 @6
npCanary <- np.cpt(4) #cpt @9

#cCopper <- conf.cpt(6) #cpt @8, conf 0.987
#Copper <- change.pt(6, "Copper") #mean/var goes from 0.09/0.01 to 0.81/0.38 @8
npCopper <- np.cpt(6) #cpt @8

#cQuillback <- conf.cpt(9) #cpt @4 conf 0.999
#Quillback <- change.pt(9, "Quillback") #mean/var goes from 0/0 to 0.37/0.16 @4
npQuillback <- np.cpt(9) #cpt @10

#cVermilion <- conf.cpt(15) #cpt @9 conf 0.999
#Vermilion <- change.pt(15, "Vermilion") #mean/var goes from 0/0 to 0.24/0.04 @9
npVermilion <- np.cpt(15) #cpt @9

#commands to run through param.est(x), plot(x), summary(x)

#TOTAL ADULT ROCKFISH

#add column to dat.avg for coding simplicity
#dat.avg <- left_join(dat.avg, RF.dat, by="Year")

#cARF <- conf.cpt(19)
#ARF <- change.pt(19, "Avg")

#HOUSEKEEPING
rm(npCanary,npCopper,npQuillback,npVermilion,np.cpt)

################################################################################
#VISUALIZE DATA
################################################################################

#load necessary library
library(ggpubr)

#Create data tables with data for each species
Adata <- RF.long.dat %>%
  filter(Species=="Copper")

Bdata <- RF.long.dat %>%
  filter(Species == "Canary")

Cdata <- RF.long.dat %>%
  filter(Species=="Vermillion")

Ddata <- RF.long.dat %>%
  filter(Species == "Quillback")

#Create individual plots
A <- ggplot(data=Adata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. copper") +
  annotate("segment", x=2012, xend=2012, y=0, yend=5, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(a)")

B <- ggplot(data=Bdata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. canary") +
  annotate("segment", x=2013, xend=2013, y=0, yend=14, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(b)")

C <- ggplot(data=Cdata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. vermilion") +
  annotate("segment", x=2013, xend=2013, y=0, yend=1, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(c)")

D <-ggplot(data=Ddata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. quillback") +
  annotate("segment", x=2014, xend=2014, y=0, yend=5, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(d)")

#Create multipaneled plot
ggarrange(A,B,C,D)

#Housekeeping
rm(A,Adata,B,Bdata,C,Cdata,D,Ddata)

################################################################################
#NON-PARAMETRIC MEANS COMPARISON PRE/POST CHANGEPOINT(S)
################################################################################

#Steps: 
#1. Create pre/post vectors for analysis
#2. Run Mann-Whitney test
#Repeat for each species

#COPPER ROCKFISH ---------------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Copper" & Year < 2012) %>%
  pull(Count)

#mean=0.071

pos <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Copper" & Year > 2012) %>%
  pull(Count)

#mean=0.813

wilcox.test(pre,pos)

#p-value=0.003722

#CANARY ROCKFISH ---------------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Canary" & Year < 2013) %>%
  pull(Count)

#mean=0.025

pos <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Canary" & Year > 2013) %>%
  pull(Count)

#mean=1.986

wilcox.test(pre,pos)

#p-value=0.0007851

#VERMILION ROCKFISH ------------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Vermilion" & Year < 2013) %>%
  pull(Count)

#mean=0

pos <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Vermilion" & Year > 2013) %>%
  pull(Count)

#mean=0.236

wilcox.test(pre,pos)

#p-value=0.007137

#QUILLBACK ROCKFISH ------------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Quillback" & Year < 2014) %>%
  pull(Count)

#mean=0.028

pos <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Quillback" & Year > 2014) %>%
  pull(Count)

#mean=0.658

wilcox.test(pre,pos)

#p-value=0.0008778

#HOUSEKEEPING
rm(pre,pos)

################################################################################
#ZACH'S CHANGEPOINT ANALYSIS CODE (FOR REFERENCE)
################################################################################

## data prep for changepoint analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## sum data across sites
dat <- dat[, -c(21, 22)]


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
  out <- cpt.meanvar(t3, Q=5)
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
wolfeel <- change.pt(12, "Wolfeel")
Yellowtail <- change.pt(13, "Yellowtail")
YOY <- change.pt(14, "YOY")
Vermilion <- change.pt(15, "Vermillion")
Yelloweye <- change.pt(17, "Yelloweye")
Puget_Sound <- change.pt(18, "Puget_Sound")

## plot changepoints 
plot.changepts <- function(spp, full_spp, title){
  offset <- 0.003
  zero <- 2010
  x1 <- spp@cpts[[1]]
  x2 <- spp@cpts[[2]]
  y1 <- spp@param.est[["mean"]][1]
  y2 <- spp@param.est[["mean"]][2]
  
  p1 <- ggplot(dat.avg, aes(Year, full_spp)) + geom_path() + geom_point() + my.theme + x.lab + x.breaks +
    geom_rect(data=dat.avg, aes(xmin=zero, xmax=zero+x1, ymin=y1-offset, ymax=y1+offset), color="red", fill="red") +
    geom_rect(data=dat.avg, aes(xmin=zero+x1, xmax=zero+x2, ymin=y2-offset, ymax=y2+offset), color="red", fill="red") +
    theme(axis.title.x=element_blank()) + ylab("Site averaged abundance") + ggtitle(title)
  return(p1)
}

p.canary <- plot.changepts(Canary, dat.avg$Canary, "Canary Rockfish")
print(p.canary)

p.quill <- plot.changepts(Quillback, dat.avg$Quillback, "Quillback Rockfish")
print(p.quill)

p.vermillion <- plot.changepts(Vermillion, dat.avg$Vermillion, "Vermillion Rockfish")
print(p.vermillion)

p.yelloweye <- plot.changepts(Yelloweye, dat.avg$Yelloweye, "Yelloweye Rockfish")
print(p.yelloweye)




windows(12, 8, record=T)
fig1 <- ggarrange(p.canary, p.quill, p.vermillion, p.yelloweye, nrow=2)







p.black <- plot.changepts(Black, dat.avg$Black, "Black & Deacon Rockfish")
print(p.black)

p.china <- plot.changepts(China, dat.avg$China, "China Rockfish")
print(p.china)

p.greenling <- plot.changepts(Greenling, dat.avg$Greenling, "Rock greenling")
print(p.greenling)

p.copper <- plot.changepts(Copper, dat.avg$Copper, "Copper Rockfish")
print(p.copper)

p.YOY <- plot.changepts(YOY, dat.avg$YOY, "Young of the Year rockfish")
print(p.YOY)

p.ling <- plot.changepts(Lingcod, dat.avg$Lingcod, "Lingcod")
print(p.ling)

p.yellowtail <- plot.changepts(Yellowtail, dat.avg$Yellowtail, "Yellowtail Rockfish")
print(p.yellowtail)

p.widow <- plot.changepts(Widow, dat.avg$Widow, "Widow Rockfish")
print(p.widow)

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

################################################################################
#PCA ANALYSIS: PUTTING ON THE BACK BURNER FOR NOW
################################################################################

#library(ggcorrplot)
library(factoextra)

#data prep
#METHOD1 https://www.datacamp.com/tutorial/pca-analysis-r
colSums(is.na(dat)) #check for NAs to remove

num_dat <- dat[,-2] #select only numerical data
norm_dat <- scale(num_dat) #normalize data
norm_dat <- norm_dat[, !colSums(is.na(norm_dat))] #remove any columns/species with NAs from normalized data
corr_matrix <- cor(norm_dat)
ggcorrplot(corr_matrix) #display correlations between variables
dat_pca <- princomp(corr_matrix)
summary(dat_pca)
dat_pca$loadings[,1:2]
fviz_pca_var(dat_pca, col.var="black")


#METHOD2 https://www.statology.org/principal-components-analysis-in-r/

colSums(dat[3:19]) #check for zero counts/no observations
num_dat <- dat %>%
  select(-c(Wolfeel, Puget_Sound)) #remove species without any counts
num_dat$Site <- as.numeric(as.factor(num_dat$Site)) #turn Site into a numeric value
num_dat <- as.matrix(num_dat) #turn dataframe into matrix for analysis

results <- prcomp(num_dat, scale=TRUE)
results$rotation <- -1*results$rotation #reverse eigenvectors from negative (default) to positive
results$x <- -1*results$x

biplot(results, scale=0)

#METHOD 3 aka only total RF abundance

ARF.dat <- RF.long.dat %>%
  filter(Species != "YOY") %>%
  group_by(Year, Site) %>%
  mutate(Total_adult_rockfish=sum(Count)) %>%
  select(-c(Species,Count)) %>%
  unique()

ARF.dat$Site <- as.numeric(as.factor(ARF.dat$Site)) #turn Site into a numeric value
ARF.dat <- as.matrix(ARF.dat) #turn dataframe into matrix for analysis

results <- prcomp(ARF.dat, scale=TRUE)
results$rotation <- -1*results$rotation #reverse eigenvectors from negative (default) to positive
results$x <- -1*results$x

biplot(results, scale=0)

################################################################################
#END OF CODE
################################################################################
