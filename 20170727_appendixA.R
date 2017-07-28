#PURPOSE OF THIS SCRIPT IS TO SUMMARIZE LOADING AND CONSUMPTION RESULTS FOR TEXT IN JULY 30, 2017 REPORT.

#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(dplyr)
library(plyr)
library(data.table)

setwd("C:/usfs_wad_data_csv/intermediate/")


#Open file with tree species metadata:
consumption <- read.table("wad_20170727_AppendixA.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Changle column names to something shorter. Easier to write script.
colnames(consumption) <- c("site", "cat", "pre", "post", "consume", "percent", "fall_2", 
                           "fall_4", "spring_2", "spring_4")

#Create a table without total surface fuel loading category.
by_cat <- consumption[consumption$cat != "Total",]

#Sum loading for each category
preLoad_summary <- ddply(by_cat, "cat", summarise, 
                     sum = sum(pre, na.rm = T),
                     count = length(pre[!is.na(pre)]))

#Calculate the total fuel loading of all sites combined.
total <- sum(preLoad_summary$sum)

#calculate the percent contribution for total fuel loading for each fuel strata/
fractional_contribution <- round((preLoad_summary$sum/total)*100,2)

#Show the percent contribution for each fuel strata
data.frame(cat = preLoad_summary$cat, percent = fractional_contribution)

#Check totals
sum(fractional_contribution)


#Seperate total loading and turn it into a vector with the same length as the by_cat table.
by_total <- consumption[consumption$cat == "Total",]
by_total[order(by_total$pre),]
by_total[order(by_total$percent),]

#Order data frame by sites
by_cat_a <- by_cat[order(by_cat$site),]

#Calculate actual total loadings (exclude impited duff loading for Vulcan and Natapoc)
TotalLoading <- ddply(by_cat_a, "site", summarise,
                      total = sum(pre, na.rm = T))


total <- expand.grid(1:length(unique(by_cat_a$cat)), TotalLoading$total)[2]
by_cat_b <- data.frame(by_cat_a, total = total)


#Calculate percent of total loading taken up by each fuel strata.
perc_of_total <- round((by_cat_b$pre/by_cat_b$Var2)*100,2)
by_cat_c <- data.frame(by_cat_b, perc_of_total)

#Show duff
bcc <- by_cat_c[by_cat_c$cat == "duff",]
bcc[order(bcc$perc_of_total),]
bcc[order(bcc$Var2),]



