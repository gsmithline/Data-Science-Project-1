#These are the libraries needed to do the analysis and cleaning
#Make sure to upload in R and R terminal before use
library(dplyr)
library(stringr)
library(BHH2)

my_data                                         <- read.csv("combined.csv",na.strings = c("", "NA"),stringsAsFactors=FALSE)
#This adds the column names to the data
colnames(my_data)                               <- c("ID", "Animal_Type","Park","Abundance","Latitude","Longitude","Acreage")
#This colum replaces all the Unknowns and blanks with NA then deletes all those rows
my_data$Abundance[my_data$Abundance=="Unknown"] <- NA
my_data_clean           <- na.omit(my_data)
#Assigns integers values to the abundance amounts of animals 
#When we assign integers to the various amounts of animals 
#the integers are represented as characters so we need to convert
#the string representation to integers 
my_data_clean$Animal_Type <- str_replace_all(my_data_clean$Animal_Type, "/", "_")
my_data_clean$Abundance[my_data_clean$Abundance=="Abundant"]<-5
my_data_clean$Abundance[my_data_clean$Abundance=="Common"]<-4
my_data_clean$Abundance[my_data_clean$Abundance=="Occasional"]<-3
my_data_clean$Abundance[my_data_clean$Abundance=="Uncommon"]<-2
my_data_clean$Abundance[my_data_clean$Abundance=="Rare"]<-1
my_data_clean$Abundance <- as.numeric(my_data_clean$Abundance)
#This creates a data frame for each animal type
#Will do linear regression and plot the data using these individual data frames
my_data_clean_mammal<-filter(my_data_clean,Animal_Type=="Mammal")
my_data_clean_fish<-filter(my_data_clean,Animal_Type=="Fish")
my_data_clean_vascular_plant<-filter(my_data_clean,Animal_Type=="Vascular Plant")
my_data_clean_bird<-filter(my_data_clean,Animal_Type=="Bird")
my_data_clean_insect<-filter(my_data_clean,Animal_Type=="Insect")
my_data_clean_nonvascular_plant<-filter(my_data_clean,Animal_Type=="Nonvascular Plant")
my_data_clean_reptile<-filter(my_data_clean,Animal_Type=="Reptile")
my_data_clean_amphibian<-filter(my_data_clean,Animal_Type=="Amphibian")
my_data_clean_fungi<-filter(my_data_clean,Animal_Type=="Fungi")
my_data_clean_algae<-filter(my_data_clean,Animal_Type=="Algae")
my_data_clean_crab_lobster_shrimp<-filter(my_data_clean,Animal_Type== "Crab_Lobster_Shrimp")
my_data_clean_invertebrate<-filter(my_data_clean,Animal_Type=="Invertebrate")
my_data_clean_slug_snail<-filter(my_data_clean,Animal_Type=="Slug_Snail")
my_data_clean_spider_scorpion<-filter(my_data_clean,Animal_Type=="Spider_Scorpion")
#This code does the linear regression and visualize it as well
#It gives a statistical summary of the data
#The data frame in lm.fit changes each time to what category of animal it is running 
lm.fit <- lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_mammal)
print(summary(lm.fit))
with(my_data_clean_mammal,plot(Latitude,Abundance))
with(my_data_clean_mammal,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_mammal,lm(Abundance~Latitude)))
lm.fit <- lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_fish)
print(summary(lm.fit))
with(my_data_clean_fish,plot(Latitude,Abundance))
with(my_data_clean_fish,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_fish,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_vascular_plant)
print(summary(lm.fit))
with(my_data_clean_vascular_plant,plot(Latitude,Abundance))
with(my_data_clean_vascular_plant,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_vascular_plant,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_bird)
print(summary(lm.fit))
with(my_data_clean_bird,plot(Latitude,Abundance))
with(my_data_clean_bird,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_bird,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_insect)
print(summary(lm.fit))
with(my_data_clean_insect,plot(Latitude,Abundance))
with(my_data_clean_insect,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_insect,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean)
print(summary(lm.fit))
with(my_data_clean_nonvascular_plant,plot(Latitude,Abundance))
with(my_data_clean_nonvascular_plant,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_nonvascular_plant,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_reptile)
print(summary(lm.fit))
with(my_data_clean_reptile,plot(Latitude,Abundance))
with(my_data_clean_reptile,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_reptile,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_amphibian)
print(summary(lm.fit))
with(my_data_clean_amphibian,plot(Latitude,Abundance))
with(my_data_clean_amphibian,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_amphibian,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_fungi)
print(summary(lm.fit))
with(my_data_clean_fungi,plot(Latitude,Abundance))
with(my_data_clean_fungi,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_fungi,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_algae)
print(summary(lm.fit))
with(my_data_clean_algae,plot(Latitude,Abundance))
with(my_data_clean_algae,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_algae,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_crab_lobster_shrimp)
print(summary(lm.fit))
with(my_data_clean_crab_lobster_shrimp,plot(Latitude,Abundance))
with(my_data_clean_crab_lobster_shrimp,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_crab_lobster_shrimp,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_invertebrate)
print(summary(lm.fit))
with(my_data_clean_invertebrate,plot(Latitude,Abundance))
with(my_data_clean_invertebrate,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_invertebrate,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_slug_snail)
print(summary(lm.fit))
with(my_data_clean_slug_snail,plot(Latitude,Abundance))
with(my_data_clean_slug_snail,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_slug_snail,lm(Abundance~Latitude)))
lm.fit <-lm(Abundance~Latitude+Longitude+Acreage,data=my_data_clean_spider_scorpion)
print(summary(lm.fit))
with(my_data_clean_spider_scorpion,plot(Latitude,Abundance))
with(my_data_clean_spider_scorpion,abline(lm(Abundance~Latitude)))
summary(with(my_data_clean_spider_scorpion,lm(Abundance~Latitude)))
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
