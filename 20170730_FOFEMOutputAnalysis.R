#PURPOSE OF THIS SCRIPT IS TO SUMMARIZE FOFEM OUTPUTS FOR TEXT IN JULY 30, 2017 REPORT.

#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(dplyr)
library(plyr)
library(data.table)

setwd("C:/Users/jcronan/Documents/GitHub/Forest_Resiliency_Burning_Pilot")

#Open file with tree species metadata:
fofem <- read.table("wad_20170730_FOFEM_outputs.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)


#Subset data by diamater class
diamClass_summary <- ddply(fofem, c("site_no", "measure"), summarise, 
                         sum_0_2 = sum(diam_0_2, na.rm = T), 
                         sum_2_4 = sum(diam_2_4, na.rm = T), 
                         sum_4_6 = sum(diam_4_6, na.rm = T), 
                         sum_6_8 = sum(diam_6_8, na.rm = T), 
                         sum_8_10 = sum(diam_8_10, na.rm = T), 
                         sum_10_12 = sum(diam_10_12, na.rm = T), 
                         sum_12_14 = sum(diam_12_14, na.rm = T), 
                         sum_14_26 = sum(diam_14_16, na.rm = T), 
                         sum_16_28 = sum(diam_16_18, na.rm = T), 
                         sum_18_20 = sum(diam_18_20, na.rm = T), 
                         sum_20_22 = sum(diam_20_22, na.rm = T), 
                         sum_22_24 = sum(diam_22_24, na.rm = T), 
                         sum_24_26 = sum(diam_24_26, na.rm = T), 
                         sum_26_28 = sum(diam_26_28, na.rm = T), 
                         sum_28_30 = sum(diam_28_30, na.rm = T), 
                         sum_30_32 = sum(diam_30_32, na.rm = T), 
                         sum_32_34 = sum(diam_32_34, na.rm = T), 
                         sum_34_36 = sum(diam_34_36, na.rm = T), 
                         sum_36_38 = sum(diam_36_38, na.rm = T), 
                         sum_38_40 = sum(diam_38_40, na.rm = T))

#Create a total column for all stems.
total_count <- vector(mode = "numeric", length = length(diamClass_summary[,1]))
for(i in 1:length(diamClass_summary[,1]))
{
total_count[i] <- sum(diamClass_summary[i,3:22])  
}

#Create a total column for all stems.
allTree_count <- vector(mode = "numeric", length = length(diamClass_summary[,1]))
for(i in 1:length(diamClass_summary[,1]))
{
  allTree_count[i] <- sum(diamClass_summary[i,4:22])  
}

#Create a total column for trees greater than 2 inches in diameter.
smallTree_count <- vector(mode = "numeric", length = length(diamClass_summary[,1]))
for(i in 1:length(diamClass_summary[,1]))
{
  smallTree_count[i] <- sum(diamClass_summary[i,4:6])  
}

#Create a total column for trees greater than 2 inches in diameter.
largeTree_count <- vector(mode = "numeric", length = length(diamClass_summary[,1]))
for(i in 1:length(diamClass_summary[,1]))
{
  largeTree_count[i] <- sum(diamClass_summary[i,8:22])  
}

#Create a total column for saplings
sapling_count <- vector(mode = "numeric", length = length(diamClass_summary[,1]))
for(i in 1:length(diamClass_summary[,1]))
{
  sapling_count[i] <- sum(diamClass_summary[i,3])  
}

#Add to data frame
diamClass_summary <- data.frame(diamClass_summary, sapling = sapling_count, 
                                tree_2_8 = smallTree_count, 
                                tree_8_40 = largeTree_count, 
                                all_trees = allTree_count, 
                                total = total_count)

#Calculate percent mortality.
#Subset pre data
diam_Class_pre <- diamClass_summary[diamClass_summary$measure == "pre",3:27]
diam_Class_pre_b <- data.frame(site_no = diamClass_summary[diamClass_summary$measure == "pre",1], 
                               diam_Class_pre[,21:25])

#Subset post data
diam_Class_post <- diamClass_summary[diamClass_summary$measure == "post",3:27]

#Calculate proprtion of trees that are predicted to die for each age class.
diamClass_percMortality <- (diam_Class_pre - diam_Class_post) / diam_Class_pre

#Replace NaN with zero (no mortality)
diamClass_percMortality[is.na(diamClass_percMortality) == T] <- 0

#Turn fractions into percents
diamClass_percMortality <- round(diamClass_percMortality * 100,2)

#Add NAs for cells where there were no trees in pre
diamClass_percMortality[diam_Class_pre == 0] <- NA

#Add site numbers
mortality <- data.frame(site_no = diamClass_summary[diamClass_summary$measure == "pre",1], 
                        diamClass_percMortality)
