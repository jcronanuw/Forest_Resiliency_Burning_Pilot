#PURPOSE OF THIS SCRIPT IS TO SUMMARIZE LOADING AND CONSUMPTION RESULTS FOR TEXT IN JULY 30, 2017 REPORT.

#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(dplyr)
library(plyr)
library(data.table)

setwd("C:/Users/jcronan/Documents/GitHub/Forest_Resiliency_Burning_Pilot")

#Open file with tree species metadata:
consumption <- read.table("wad_20170727_AppendixA.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Changle column names to something shorter. Easier to write script.
colnames(consumption) <- c("site", "cat", "pre", "post", "consume", "percent", "fm2", "fm4", 
                           "fall_2", "fall_4", "spring_2", "spring_4")

###########
#IMPUTE DUFF DATA INTO NATAPOC AND VULCAN FROM UNIT WITH CLOSEST MATCHING LITTER VALUE

#Seperate total loading from loading by fuel strata.
#litter_all_a <- consumption[consumption$cat == "litter",]
#litter_all_a[order(litter_all_a$pre),]
#Vulcan litter loading is between Hanlon and Paradise 90; use an average of these two values.
#Natapoc litter loading is between UR-1 and Sherman Creek; use an average of these two values.
#duff_all_a <- consumption[consumption$cat == "duff",]
#duff_all_a[order(duff_all_a$pre),]
#mean(c(14.27,29.71))#imputed duff loading for Vulcan
#mean(c(26.45,13.90))#imputed duff loading for Natapoc

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

#############
#TOTAL LOADING

#Seperate total loading from loading by fuel strata.
total_burned_a <- consumption[consumption$cat == "Total",]

#Calculate the percent deviation from measured value for each Consume run
perc_fm_2 <- -1*round((((total_burned_a$consume-total_burned_a$fm2)/total_burned_a$consume)*100),0)
perc_fm_4 <- -1*round((((total_burned_a$consume-total_burned_a$fm4)/total_burned_a$consume)*100),0)

#Calculate the percent of total pre-fire fuels predicted to consume
perc_fall_2 <- round(((total_burned_a$fall_2/total_burned_a$pre)*100),0)
perc_fall_4 <- round(((total_burned_a$fall_4/total_burned_a$pre)*100),0)
perc_spring_2 <- round(((total_burned_a$spring_2/total_burned_a$pre)*100),0)
perc_spring_4 <- round(((total_burned_a$spring_4/total_burned_a$pre)*100),0)

total_burned_b <- data.frame(total_burned_a, perc_fm_2, perc_fm_4, perc_fall_2, perc_fall_4, perc_spring_2, perc_spring_4)
range(total_burned_b$perc_fall_2)
range(total_burned_b$perc_fall_4)
range(total_burned_b$perc_spring_2)
range(total_burned_b$perc_spring_4)

total_burned_c <- total_burned_b[total_burned_b$site %in% c("Angel", "Chumstick", "Orion", 
                                                            "Paradise 90", "Sherman Creek", "Mile25"),c(1,2,3,4,5,6,7,8,13,14)]
#This will show loading, measured consumption, predicted consumption, percent differences for burned units.
total_burned_c
mean(c(36, 26, 18, 22, 34, 37))#mean of difference between measured and predicted for Consume v. 2.1
mean(c(26, 16, 17, 45, 24, 11))#mean of difference between measured and predicted for Consume v. 4.2

total_burned_d <- total_burned_b[,c(1,2,3,9,10,11,12,15,16,17,18)]
range(total_burned_d$perc_fall_2)
range(total_burned_d$perc_fall_4)
range(total_burned_d$perc_spring_2)
range(total_burned_d$perc_spring_4)

#############
#CONSUME PERFORMANCE BY CATEGORY

#Seperate total loading from loading by fuel strata.
burned_units <- c("Angel", "Chumstick", "Orion", "Paradise 90")
consume_perf_a <- consumption[consumption$site %in% burned_units,]
consume_perf_a$consume[consume_perf_a$consume == 0] <- 0.01

#Calculate the percent deviation from measured value for each Consume run
byCat_perc_fm_2 <- -1*round((((consume_perf_a$consume-consume_perf_a$fm2)/consume_perf_a$consume)*100),0)
byCat_perc_fm_4 <- -1*round((((consume_perf_a$consume-consume_perf_a$fm4)/consume_perf_a$consume)*100),0)


consume_perf_b <- data.frame(consume_perf_a, byCat_perc_fm_2, byCat_perc_fm_4)



Consume_Performance_xCat_fall <- ddply(consume_perf_b, "cat", summarise,
                                   pre = round(mean(pre),1),
                                   v2_mean = round(mean(byCat_perc_fm_2),1), 
                                  v2_min = round(min(byCat_perc_fm_2),1), 
                                  v2_max = round(max(byCat_perc_fm_2),1),
                                   v4_mean = round(mean(byCat_perc_fm_4),1), 
                                  v4_min = round(min(byCat_perc_fm_4),1), 
                                  v4_max = round(max(byCat_perc_fm_4),1))

#Seperate total loading from loading by fuel strata.
burned_units <- c("Sherman Creek", "Mile25")
consume_perf_a <- consumption[consumption$site %in% burned_units,]
consume_perf_a$consume[consume_perf_a$consume == 0] <- 0.01

#Calculate the percent deviation from measured value for each Consume run
byCat_perc_fm_2 <- -1*round((((consume_perf_a$consume-consume_perf_a$fm2)/consume_perf_a$consume)*100),0)
byCat_perc_fm_4 <- -1*round((((consume_perf_a$consume-consume_perf_a$fm4)/consume_perf_a$consume)*100),0)


consume_perf_b <- data.frame(consume_perf_a, byCat_perc_fm_2, byCat_perc_fm_4)



Consume_Performance_xCat_spring <- ddply(consume_perf_b, "cat", summarise,
                                  pre = round(mean(pre),1),
                                  v2_mean = round(mean(byCat_perc_fm_2),1), 
                                  v2_min = round(min(byCat_perc_fm_2),1), 
                                  v2_max = round(max(byCat_perc_fm_2),1),
                                  v4_mean = round(mean(byCat_perc_fm_4),1), 
                                  v4_min = round(min(byCat_perc_fm_4),1), 
                                  v4_max = round(max(byCat_perc_fm_4),1))

#Create new table, summarize Consume performance by fuel strata.
write.table(Consume_Performance_xCat_fall, file = "20170731_ConsumePerformance_Fall.csv", 
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", 
            dec = ".", row.names = TRUE,col.names = NA, qmethod = 
              c("escape", "double"))#

#Create new table, summarize Consume performance by fuel strata.
write.table(Consume_Performance_xCat_spring, file = "20170731_ConsumePerformance_Spring.csv", 
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", 
            dec = ".", row.names = TRUE,col.names = NA, qmethod = 
              c("escape", "double"))#

#############
#DUFF LOADING

#Seperate total loading from loading by fuel strata.
duff_burned_a <- consumption[consumption$cat == "duff",]

#Calculate the percent deviation from measured value for each Consume run
perc_fm_2 <- -1*round((((duff_burned_a$consume-duff_burned_a$fm2)/duff_burned_a$consume)*100),0)
perc_fm_4 <- -1*round((((duff_burned_a$consume-duff_burned_a$fm4)/duff_burned_a$consume)*100),0)
perc_fall_2 <- -1*round((((duff_burned_a$consume-duff_burned_a$fall_2)/duff_burned_a$consume)*100),0)
perc_fall_4 <- -1*round((((duff_burned_a$consume-duff_burned_a$fall_4)/duff_burned_a$consume)*100),0)
perc_spring_2 <- -1*round((((duff_burned_a$consume-duff_burned_a$spring_2)/duff_burned_a$consume)*100),0)
perc_spring_4 <- -1*round((((duff_burned_a$consume-duff_burned_a$spring_4)/duff_burned_a$consume)*100),0)

duff_burned_b <- data.frame(duff_burned_a, perc_fm_2, perc_fm_4, perc_fall_2, perc_fall_4, perc_spring_2, perc_spring_4)

duff_burned_c <- duff_burned_b[duff_burned_b$site %in% c("Angel", "Chumstick", "Orion", 
                                                            "Paradise 90", "Sherman Creek", "Mile25"),c(1,2,3,4,5,6,7,8,13,14)]

#This will show duff loading, measured consumption, predicted consumption, percent differences for burned units.
duff_burned_c
mean(18, 41, 21, 17, 35, 169)#mean of difference between measured and predicted for Consume v. 2.1
mean(44, 45, 46, 62, 100, 100)#mean of difference between measured and predicted for Consume v. 4.2

#Subset data by category (i.e. exclude totals) and order data frame by sites.
cats_a <- consumption[consumption$cat != "Total",]
cats_b <- cats_a[order(cats_a$site),]

#Calculate actual total loadings (exclude imputed duff loading for Vulcan and Natapoc)
TotalLoading <- ddply(cats_b, "site", summarise,
                      total = sum(pre, na.rm = T))


total <- expand.grid(1:length(unique(cats_b$cat)), TotalLoading$total)[2]
cats_c <- data.frame(cats_b, total = total)


#perc_ofTotal_fall2 <- cats_c


#Calculate percentage of total consumption for each fuel strata for spring and fall scenarion predictions.
TotalPredConsumption_xCat <- ddply(cats_c, "cat", summarise,
                              pre = sum(pre),
                              fall_2 = sum(fall_2),
                              fall_4 = sum(fall_4),
                              spring_2 = sum(spring_2),
                              spring_4 = sum(spring_4))
                              
TotalPredConsumption <- ddply(total_burned_a, "cat", summarise,
                              pre = sum(pre),
                              fall_2 = sum(fall_2),
                              fall_4 = sum(fall_4),
                              spring_2 = sum(spring_2),
                              spring_4 = sum(spring_4))

#Combine 1000-hr fuels and add them back into the data frame
hr1000 <- TotalPredConsumption_xCat[6,2:6] + TotalPredConsumption_xCat[7,2:6]

#
TotalPredConsumption_xCat <- TotalPredConsumption_xCat[-c(6,7),]
levels(TotalPredConsumption_xCat$cat) <- c(levels(TotalPredConsumption_xCat$cat), "hr_1000")
TotalPredConsumption_xCat[8,1] <- as.factor("hr_1000")
TotalPredConsumption_xCat[8,2:6] <- hr1000


by_cat <- TotalPredConsumption_xCat[,3:6]
  
by_cat <- as.matrix(by_cat,8,4)

by_total <- as.numeric(TotalPredConsumption[,3:6])

bta <- expand.grid(1:8, by_total)[2]
btb <- as.numeric(bta$Var2)
btc <- matrix(btb, 8,4)

TotalPredCons_PercentOfTotal_a <- round((by_cat/btc)*100,2)

colnames(TotalPredCons_PercentOfTotal_a) <- c("fall_2", "fall_4", "spring_2", "spring_4")

TotalPredCons_PercentOfTotal_b <- data.frame(cat = TotalPredConsumption_xCat$cat, TotalPredCons_PercentOfTotal_a)

#Create new table, summarize Consume performance by fuel strata.
write.table(TotalPredCons_PercentOfTotal_b, file = "20170731_PercentConsumption_byCat.csv", 
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", 
            dec = ".", row.names = TRUE,col.names = NA, qmethod = 
              c("escape", "double"))#

#Look at percentage predicted consumption by fuel strata for each seasonal fuel moisture scenario
perc_redFall_2 <- round(TotalPredConsumption_xCat$fall_2/TotalPredConsumption_xCat$pre*100,1)
perc_redFall_4 <- round(TotalPredConsumption_xCat$fall_4/TotalPredConsumption_xCat$pre*100,1)
perc_redSpring_2 <- round(TotalPredConsumption_xCat$spring_2/TotalPredConsumption_xCat$pre*100,1)
perc_redSpring_4 <- round(TotalPredConsumption_xCat$spring_4/TotalPredConsumption_xCat$pre*100,1)

num <- c(7,1,3,5,6,2,4,8)
TotalPredConsumption_a <- data.frame(num = num, 
                                     TotalPredCons_PercentOfTotal_b, perc_redFall_2, perc_redFall_4, 
                                     perc_redSpring_2, perc_redSpring_4)

TotalPredConsumption_b <- TotalPredConsumption_a[order(TotalPredConsumption_a$num),]


tpc <- data.matrix(TotalPredConsumption_b[,7:10])
rownames(tpc) = TotalPredConsumption_b[,2]



mp <- barplot(tpc) # default
tot <- colMeans(tpc)
text(mp, tot + 3, format(tot), xpd = TRUE, col = "blue")
barplot(tpc, beside = TRUE,
        col = rainbow(8),
        legend = rownames(tpc), ylim = c(0, 100))
title(main = "Percent Predicted Consumption", font.main = 4)









