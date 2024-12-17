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
library(lme4)
library(ggthemes)
library(lsmeans)
library(here)
library(EnvStats)
library(ggeffects)
library(ggh4x)
library(glmmTMB)
library(DHARMa)
library(MASS)
library(dplyr)

#Load data
dat <- read_csv(here("./data_input/Neah_Bay_data.csv"))

#Tidy data
dat <- dat %>% 
  filter(Transect=="T1") %>%
  dplyr::select(-c(Direction, Location, Transect))

long.dat <- dat %>% 
  pivot_longer(cols = c(3:19), names_to = "Species", values_to = "Count")

#remove non-rockfish and create database with just rockfish, including a column with total and average totals
RF.dat <- dat %>%
  dplyr::select(-c(Cabezon, Greenling, Halibut, Lingcod, Wolfeel, YOY)) %>%
  rowwise() %>%
  mutate(Total = sum(c(Black, Canary, China, Copper, Puget_Sound, Quillback, 
                       Tiger, Vermillion, Widow, Yelloweye, Yellowtail))) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(Avg = mean(Total)) %>%
  dplyr::select(c(Year, Total, Avg)) %>%
  unique() 

RF.long.dat <- long.dat %>%
  filter(Species %in% c("Black", "Canary", "China", "Copper", "Puget_Sound", 
                        "Quillback", "Tiger", "Vermillion", "Widow", "Yelloweye",
                        "Yellowtail")) #create a tidy version for ggplot2

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

#Figure 2 with trend lines removed (not statistically robust, so potentially misleading in manuscript)
ggplot(data=test, aes(x=Year,y=Total,group=Site, fill=Site)) +
  geom_point(size=2, alpha=0.5, aes(shape=Site)) +
  scale_shape_manual(values=c(21,22,23,24,25))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  facet_wrap(~Site) +
  ylab("Total adult fish")+
  theme_cowplot()+
  theme(legend.position="none")

#Figure 3
test2 <- test %>%
  drop_na() %>%
  filter(Species != "Halibut") %>%
  filter(Species != "Wolfeel") %>%
  filter(Species != "Puget_Sound") #remove counts less than 2

#Rename "Black" as "Black & Deacon" for figure
conditions <- c("Black")
replacement_values <- c("Black & Deacon")
test2$Species <- replace(test2$Species, test2$Species %in% conditions, replacement_values)

#plot at 1500x800
descending <- c("Black & Deacon","Widow","Yellowtail","Greenling","China","Canary","Lingcod","Quillback","Copper","Tiger","Cabezon","Vermillion","Yelloweye")

ggplot(data=test2,aes(x=Year,y=Count)) +
  geom_point(size=2.5, alpha=0.5, aes(shape=Site,group=Site, fill=Site))+
  geom_smooth(method="lm",se=FALSE, linewidth=0.75, color="gray25")+
  scale_shape_manual(values=c(21,22,23,24,25))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  facet_wrap(~factor(Species, levels=descending), scales="free",nrow=3) +
  theme_cowplot() +
  theme(text = element_text(size = 20))

#Plot with trendline removed
ggplot(data=test2,aes(x=Year,y=Count)) +
  geom_point(size=2.5, alpha=0.5, aes(shape=Site,group=Site, fill=Site))+
  scale_shape_manual(values=c(21,22,23,24,25))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"))+
  facet_wrap(~factor(Species, levels=descending), scales="free",nrow=3) +
  theme_cowplot() +
  theme(text = element_text(size = 20))

#Housekeeping
rm(test, test2, conditions, replacement_values, descending)


#format columns
cols_fac <- as.vector(colnames(RF.long.dat)[c(2, 3)]) #select columns that are factors
RF.long.dat[cols_fac] <- lapply(RF.long.dat[cols_fac], factor) #turn those columns into factors
cols_num <- as.vector(colnames(RF.long.dat)[c(1,4)]) #select columns that are numeric
RF.long.dat[cols_num] <- lapply(RF.long.dat[cols_num], as.numeric) #turn those columns into numeric

#sum number of observations of each species (trying to decide what species to remove because too few data)
RF.long.dat %>% 
  group_by(Species) %>%
  summarise(
    Total = sum(Count)
  )

#make a histogram of the counts for each species (trying to decide what species to remove because too few data)
ggplot(RF.long.dat, aes(x = Count, fill = Species)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +  # Customize binwidth as needed
  facet_wrap(~ Species, scales = "free") +  # Create a separate plot for each species
  theme_minimal() 

#remove puget sound, vermillion,  yelloweye, widow
RF.long.dat.filt <- RF.long.dat %>%
  filter(!(Species %in% c("Puget_Sound", "Vermillion", "Yelloweye", "Widow")))

#change Black to Black and Deacon for plot
levels(RF.long.dat.filt$Species)[levels(RF.long.dat.filt$Species) == "Black"] <- "Black and Deacon"

#create models (with and without interaction)
mod1 <- glmmTMB(Count ~ Year * Species + (1|Site), family = "nbinom2", data = RF.long.dat.filt) 
summary(mod1)
mod2 <- glmmTMB(Count ~ Year + Species + (1|Site), family = "nbinom2", data = RF.long.dat.filt) 
summary(mod2)

#test interaction
anova(mod1, mod2)

#check model fit/diagnostics
hist(residuals(mod1))
plot(residuals(mod1))
resids <- simulateResiduals(mod1)
plot(resids)

#because year x species interaction was significant, build a model for each species to determine whether species is a significant predictor for each one
#black & deacon
#filter data
black <- RF.long.dat.filt %>%
  filter(Species %in% "Black and Deacon")

#build model with and without predictor
black.mod1 <- glm.nb(Count ~ Year, data = black)
black.mod2 <- glm.nb(Count ~ 1 , data = black)

#test whether year is a significant predictor
p <- c(anova(black.mod1, black.mod2)[2,8])
LRT <- c(anova(black.mod1, black.mod2)[2,7])
Species <- c("black")

#check model fit
hist(residuals(black.mod1))
plot(residuals(black.mod1))
resids <- simulateResiduals(black.mod1)
plot(resids)

#canary
#filter data
canary <- RF.long.dat.filt %>%
  filter(Species %in% "Canary")

#build model with and without predictor
canary.mod1 <- glm.nb(Count ~ Year, data = canary)
canary.mod2 <- glm.nb(Count ~ 1, data = canary)

#test whether year is a significant predictor
p <- c(p, anova(canary.mod1, canary.mod2)[2,8])
LRT <- c(LRT, anova(canary.mod1, canary.mod2)[2,7])
Species <- c(Species, "canary")

#check model fit
hist(residuals(canary.mod1))
plot(residuals(canary.mod1))
resids <- simulateResiduals(canary.mod1)
plot(resids)

#china
#filter data
china <- RF.long.dat.filt %>%
  filter(Species %in% "China")

#build model with and without predictor
china.mod1 <- glm.nb(Count ~ Year, data = china)
china.mod2 <- glm.nb(Count ~ 1, data = china)

#test whether year is a significant predictor
p <- c(p, anova(china.mod1, china.mod2)[2,8])
LRT <- c(LRT, anova(china.mod1, china.mod2)[2,7])
Species <- c(Species, "china")

#check model fit
hist(residuals(china.mod1))
plot(residuals(china.mod1))
resids <- simulateResiduals(china.mod1)
plot(resids)

#copper
#filter data
copper <- RF.long.dat.filt %>%
  filter(Species %in% "Copper")

#build model with and without predictor
copper.mod1 <- glm.nb(Count ~ Year, data = copper)
copper.mod2 <- glm.nb(Count ~ 1, data = copper)

#test whether year is a significant predictor
p <- c(p, anova(copper.mod1, copper.mod2)[2,8])
LRT <- c(LRT, anova(copper.mod1, copper.mod2)[2,7])
Species <- c(Species, "copper")

#check model fit
hist(residuals(copper.mod1))
plot(residuals(copper.mod1))
qqPlot(residuals(copper.mod1))
resids <- simulateResiduals(copper.mod1)
plot(resids)

#quillback
#filter data
quillback <- RF.long.dat.filt %>%
  filter(Species %in% "Quillback")

#build model with and without predictor
quillback.mod1 <- glm.nb(Count ~ Year, data = quillback)
quillback.mod2 <- glm.nb(Count ~ 1, data = quillback)

#test whether year is a significant predictor
p <- c(p, anova(quillback.mod1, quillback.mod2)[2,8])
LRT <- c(LRT, anova(quillback.mod1, quillback.mod2)[2,7])
Species <- c(Species, "quillback")

#check model fit
hist(residuals(quillback.mod1))
plot(residuals(quillback.mod1))
qqPlot(residuals(quillback.mod1))
resids <- simulateResiduals(quillback.mod1)
plot(resids)

#tiger
#filter data
tiger <- RF.long.dat.filt %>%
  filter(Species %in% "Tiger")

#build model with and without predictor
tiger.mod1 <- glm.nb(Count ~ Year, data = tiger)
tiger.mod2 <- glm.nb(Count ~ 1, data = tiger)

#test whether year is a significant predictor
p <- c(p, anova(tiger.mod1, tiger.mod2)[2,8])
LRT <- c(LRT, anova(tiger.mod1, tiger.mod2)[2,7])
Species <- c(Species, "tiger")

#check model fit
hist(residuals(tiger.mod1))
plot(residuals(tiger.mod1))
qqPlot(residuals(tiger.mod1))
resids <- simulateResiduals(tiger.mod1)
plot(resids)

#yellowtail
#filter data
yellowtail <- RF.long.dat.filt %>%
  filter(Species %in% "Yellowtail")

#build model with and without predictor
yellowtail.mod1 <- glm.nb(Count ~ Year, data = yellowtail)
yellowtail.mod2 <- glm.nb(Count ~ 1, data = yellowtail)

#test whether year is a significant predictor
p <- c(p, anova(yellowtail.mod1, yellowtail.mod2)[2,8])
LRT <- c(LRT, anova(yellowtail.mod1, yellowtail.mod2)[2,7])
Species <- c(Species, "yellowtail")

#check model fit
hist(residuals(yellowtail.mod1))
plot(residuals(yellowtail.mod1))
qqPlot(residuals(yellowtail.mod1))
resids <- simulateResiduals(yellowtail.mod1)
plot(resids)

#create predicted value using the mixed effects model
count_pred <- as.data.frame(ggpredict(mod1, terms = c("Year", "Species"), interval = "confidence"))
colnames(count_pred)[6] <- "Species"

#p values to add to graphs
LRT <- round(LRT, 2)
p.vals <- as.data.frame(cbind(Species, p, LRT))
p.vals$p <- c("p = 0.02", "p = 0.01", "p < 0.001", "p < 0.01", "p < 0.001", "p = 0.02", "p = 0.02")
p.vals$LRT <- paste("LRT = ", p.vals$LRT)
p.vals$x <- 2008
p.vals$y.p.val <- c(300, 13, 15, 3.5, 6, 3.5, 22.5)
p.vals$Species <- str_to_title(p.vals$Species)
p.vals$Species <- as.factor(p.vals$Species)
levels(p.vals$Species)[levels(p.vals$Species) == "Black"] <- "Black and Deacon"

#plot data, prediction line, and confidence interval 
fig3_base <- ggplot(count_pred, aes(x = x, y = predicted)) +
  geom_line() + # Line plot of predicted values 
  geom_ribbon(data = count_pred, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.3) +# add CI
  geom_point(data = RF.long.dat.filt, aes(x = Year, y = Count, shape=Site,group=Site, fill=Site), size = 2) +
  scale_shape_manual(values=c(21,22,23,24,25),
                     labels = c("Site 1", "Site 2", "Site 3", "Site 4", "Site 5")) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7"),
                    labels = c("Site 1", "Site 2", "Site 3", "Site 4", "Site 5"))+
  theme_cowplot() +
  theme(legend.position = "right") +
  xlab("Year") +
  ylab("Count") +
  scale_x_continuous(limits = c(2005, 2024)) +
  geom_text(data = p.vals, (aes(x = x, -Inf, label = p)),
            col = "black",
            vjust = -7.75) +
  geom_text(data = p.vals, (aes(x = x, -Inf, label = LRT)),
            col = "black",
            vjust = -6.5) +
  facet_wrap(~Species, scales = "free", ncol = 2) 

#center the plot on the bottom row
design <- c(
  "
  AABB
  CCDD
  EEFF
  #GG#
  "
)

fig3_base + facet_manual(~Species, design = design, scales = "free")

ggsave("Figure_3_With_Model_predictions.png", plot = last_plot(), 
       path = here("./figures"))

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

#create total adult rockfish column
dat.avg <- dat.avg %>%
  select(-c(YOY,Cabezon,Greenling,Lingcod,Wolfeel,Halibut)) %>%
  mutate(Rockfish=rowSums(.[2:12]))

#TEST FOR NORMALITY-------------------------------------------------------------

#1. visualize histograms
data <- dat.avg %>%
  pivot_longer(cols = c(2:13), names_to = "Species", values_to = "Avg")

ggplot(data, aes(x=Avg, color=Species)) +
  geom_histogram(aes(fill=Species)) +
  facet_wrap(~Species) +
  scale_x_continuous(trans="log2")

#2.visualize qq-plots
qqnorm(dat.avg$Rockfish)
qqline(dat.avg$Rockfish)

qqnorm(dat.avg$Canary)
qqline(dat.avg$Canary)

qqnorm(dat.avg$Copper)
qqline(dat.avg$Copper)

qqnorm(dat.avg$Quillback)
qqline(dat.avg$Quillback)

qqnorm(dat.avg$Tiger)
qqline(dat.avg$Tiger)

qqnorm(dat.avg$China)
qqline(dat.avg$China)

#qqnorm(RF.dat$Avg)
#qqline(RF.dat$Avg)

#3. Shapiro-Wilk normality test

shapiro.test(dat.avg$Rockfish) #non-normal
shapiro.test(dat.avg$Canary) #non-normal
shapiro.test(dat.avg$China) #NORMAL
shapiro.test(dat.avg$Copper) #non-normal
shapiro.test(dat.avg$Quillback) #non-normal
shapiro.test(dat.avg$Tiger) #non-normal
#shapiro.test(RF.dat$Avg) #normal

#MAKE FUNCTIONS ---------------------------------------------------------------------

##function to calculate confidence intervals
conf.cpt <- function(col){
  t1 <- as.vector(dat.avg[col])
  t2 <- as.numeric(unlist(t1))
  out <- cpt.meanvar(t2, penalty="Asymptotic", pen.value=0.05, method="AMOC", class=FALSE)
  return(out)
}

##function to generate meanvar plots
change.pt <- function(col, Species){
  t1 <- dat.avg[, c(1, col)]
  t2 <- t1 %>% spread(Year, Species)
  t3 <- as.numeric(t2)
  out <- cpt.meanvar(t3, Q=5)
  return(out)
}

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
#Black(2), Cabezon(3), Greenling(7), Lingcod(8), Widow(11), Wolfeel(12), Yellowtail(13), 
#YOY(14), Vermilion(15), Halibut(16), Yelloweye(17), Puget_Sound(18)

npRockfish <- np.cpt(13) #cpt @ 12 aka 2016

npCanary <- np.cpt(3) #cpt @ 8 aka 2012

npCopper <- np.cpt(5) #cpt @ 8 aka 2012

npQuillback <- np.cpt(6) #cpt @ 10 aka 2014

cChina <- conf.cpt(4) 
China <- change.pt(4, "China") #cpt @ 17 aka 2022

npTiger <- np.cpt(7) #cpt @ 15 aka 2019

#commands to run through param.est(x), plot(x), summary(x)

#TOTAL ADULT ROCKFISH

#add column to dat.avg for coding simplicity
#dat.avg <- left_join(dat.avg, RF.dat, by="Year")

#cARF <- conf.cpt(19)
#ARF <- change.pt(19, "Avg")

#HOUSEKEEPING
rm(npCanary,npCopper,npQuillback,npTiger,cChina, China, np.cpt, npRockfish,change.pt,conf.cpt)

################################################################################
#VISUALIZE DATA
################################################################################

#load necessary library
library(ggpubr)

#Create data table with data for total adult rockfish
Adata <- RF.long.dat %>%
  group_by(Year,Site) %>%
  mutate(Total=sum(Count)) %>%
  select(-c(Species,Count)) %>%
  unique()

#Create data tables with data for each species
Bdata <- RF.long.dat %>%
  filter(Species=="Copper")

Cdata <- RF.long.dat %>%
  filter(Species == "Canary")

Ddata <- RF.long.dat %>%
  filter(Species == "Quillback")

Edata <- RF.long.dat %>%
  filter(Species == "Tiger")

Fdata <- RF.long.dat %>%
  filter(Species == "China")

#Create individual plots
F <- ggplot(data=Adata, aes(x=Year, y=Total)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. adult rockfish") +
  annotate("segment", x=2016, xend=2016, y=0, yend=320, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(f)")

A <- ggplot(data=Bdata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. copper") +
  annotate("segment", x=2012, xend=2012, y=0, yend=4, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(a)")

Ad <-  ggplot(data=Bdata, aes(x=Year, y=Count)) +
  geom_point(aes(group=Year, size=3, alpha=0.25)) +
  theme_cowplot() + ylab("No. copper") + theme(legend.position = "none") +
  annotate("segment", x=2012, xend=2012, y=0, yend=4, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(a)") #d options are scatterplots rather than boxplots

B <- ggplot(data=Cdata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. canary") +
  annotate("segment", x=2012, xend=2012, y=0, yend=14, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(b)")

Bd <- ggplot(data=Cdata, aes(x=Year, y=Count)) +
  geom_point(aes(group=Year, size = 3, alpha=0.25)) +
  theme_cowplot() + ylab("No. canary") + theme(legend.position = "none") +
  annotate("segment", x=2012, xend=2012, y=0, yend=14, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(b)")

C <-ggplot(data=Ddata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. quillback") +
  annotate("segment", x=2014, xend=2014, y=0, yend=7, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(c)")

Cd <-ggplot(data=Ddata, aes(x=Year, y=Count)) +
  geom_point(aes(group=Year, size=3, alpha=0.25)) +
  theme_cowplot() + ylab("No. quillback") + theme(legend.position = "none") +
  annotate("segment", x=2014, xend=2014, y=0, yend=7, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(c)")

D <-ggplot(data=Edata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. tiger") +
  annotate("segment", x=2019, xend=2019, y=0, yend=4, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(d)")

Dd <-ggplot(data=Edata, aes(x=Year, y=Count)) +
  geom_point(aes(group=Year, size=3, alpha=0.25)) +
  theme_cowplot() + ylab("No. tiger") + theme(legend.position = "none") +
  annotate("segment", x=2019, xend=2019, y=0, yend=4, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(d)")

E <-ggplot(data=Fdata, aes(x=Year, y=Count)) +
  geom_boxplot(aes(group=Year)) +
  theme_cowplot() + ylab("No. china") +
  annotate("segment", x=2022, xend=2022, y=0, yend=16, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(e)")

Ed <-ggplot(data=Fdata, aes(x=Year, y=Count)) +
  geom_point(aes(group=Year, size=3, alpha=0.25)) +
  theme_cowplot() + ylab("No. china") + theme(legend.position = "none") +
  annotate("segment", x=2022, xend=2022, y=0, yend=16, color="red", linewidth=1.5, linetype="dashed", alpha=0.5) +
  ggtitle("(e)")

#Create multipaneled plot - dimensions 1000x650
ggarrange(Ad,Bd,Cd,Dd,Ed) #add F if you want to include panel of total adult rockfish

#Housekeeping
rm(A,Adata,B,Bdata,C,Cdata,D,Ddata,E,Edata,F,Fdata)

################################################################################
#NON-PARAMETRIC MEANS COMPARISON PRE/POST CHANGEPOINT(S)
################################################################################

#Steps: 
#1. Create pre/post vectors for analysis
#2. Run Mann-Whitney test
#Repeat for each species

#TOTAL ADULT ROCKFISH-----------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:13),names_to="Species", values_to="Count") %>% 
  filter(Species=="Rockfish" & Year < 2016) %>%
  pull(Count)

#mean=43.43

pos <- dat.avg %>%
  pivot_longer(cols=c(2:13),names_to="Species", values_to="Count") %>% 
  filter(Species=="Rockfish" & Year > 2016) %>%
  pull(Count)

#mean=74.48

wilcox.test(pre,pos)

#p-value=0.06157

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

#mean=0.79

wilcox.test(pre,pos)

#p-value=0.001747

#CANARY ROCKFISH ---------------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Canary" & Year < 2012) %>%
  pull(Count)

#mean=0.0286

pos <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Canary" & Year > 2012) %>%
  pull(Count)

#mean=1.47

wilcox.test(pre,pos)

#p-value=0.000801

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

#mean=0.97

wilcox.test(pre,pos)

#CHINA ROCKFISH ------------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="China" & Year < 2021) %>%
  pull(Count)

#mean=3.52

pos <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="China" & Year > 2021) %>%
  pull(Count)

#mean=9.5

wilcox.test(pre,pos)

#p-value=0.03025

#TIGER ROCKFISH ------------------------------------------------------------

pre <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Tiger" & Year < 2019) %>%
  pull(Count)

#mean=0.25

pos <- dat.avg %>%
  pivot_longer(cols=c(2:18),names_to="Species", values_to="Count") %>% 
  filter(Species=="Tiger" & Year > 2019) %>%
  pull(Count)

#mean=1.2

wilcox.test(pre,pos)

#p-value = 0.0219

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
change.pt <- function(col, Species){
  t1 <- dat.avg[, c(1, col)]
  t2 <- t1 %>% spread(Year, Species)
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
plot.changepts <- function(Species, full_Species, title){
  offset <- 0.003
  zero <- 2010
  x1 <- Species@cpts[[1]]
  x2 <- Species@cpts[[2]]
  y1 <- Species@param.est[["mean"]][1]
  y2 <- Species@param.est[["mean"]][2]
  
  p1 <- ggplot(dat.avg, aes(Year, full_Species)) + geom_path() + geom_point() + my.theme + x.lab + x.breaks +
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
