#PURPOSE OF THIS SCRIPT IS TO CREATE INPUTS FOR FOFEM FROM OVERSTORY DATA COLLECTED
#AS PART OF THE WASHINGTON DEPARTMENT OF NATURAL RESOURCES FOREST RESILIENCY BURNING
#PILOT PROJECT

#DATA WILL BE USED FOR THE SPRING 2017 REPORT

#SCRIPT IS ADAPTED FROM CODE USED TO CACLULATE OVERSTORY CHARACTERISTICS IN FALL 2016.



#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(dplyr)
library(plyr)
library(data.table)


setwd("C:/users/jcronan/Documents/GitHub/Forest_Resiliency_Burning_Pilot/inputs/")
getwd()

#Open file with tree species metadata:
species_lut <- read.table("wad2017_species_lut.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Open file with site metadata:
site_lut <- read.table("wad2017_site_lut.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Open file with sampling episode metadata:
episode_lut <- read.table("wad2017_episode_lut.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Open file with plot metadata:
plot_lut <- read.table("wad2017_plot_lut.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Open file with stem count data:
stems <- read.table("wad2017_stems.csv", header=TRUE, 
                    sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Open file with overstory data:
trees <- read.table("wad2017_trees.csv", header=TRUE, 
                    sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Open file with plot radius data:
radius <- read.table("wad_plot_radius.csv", header=TRUE, 
                    sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Remove Notes column
radius <- radius[,-9]

#Calculate area of the circle for each plot
radius_area_meters <- radius[,3:7]^2 * 3.14159265359

#Convert square meters into acres
radius_area_acres <- radius_area_meters * 0.000247105 #(number of acres per square meter)

#Conversion factor (multiply number of trees per plot by this to get density per acre)
expansion_factor <- round(1/radius_area_acres,3)
expansion_factor <- data.frame(unitName = radius[,1], 
                                           unitID = radius[,2], 
                                           expansion_factor)


#List site names that burned.
unitName_burned <- c("Sherman Creek", "Paradise 90", "Hanlon", "Orion 2", 
  "Chumstick ZUI", "25 Mile", "Angel")

#List site names that burned.
unitName_burned <- c("Sherman Creek", "Paradise 90", "Hanlon", "Orion 2", 
                     "Chumstick ZUI", "25 Mile", "Angel", "8 Mile Bottom", 
                     "Canteen", "Goat", "Natapoc 35", "Oak Creek", 
                     "Upper Rendezvous 1","Vulcan")

#Isolate unit ID numbers for sites that burned
unitID_burned <- site_lut$unitID[site_lut$unitName %in% unitName_burned]

#Number of burned units
bux <- length(unitID_burned)

#Isolate sampling episode IDs for pre-fire episodes of sites that were burned.
pre_seID_burned_sites <- episode_lut$unitSamplingEpisodeID[episode_lut$unitID %in% unitID_burned & 
    episode_lut$samplingEpisodeTypeID == 1]

#Remove the "notes" column
trees <- trees[,-11]

#Isolate entries from pre-fire sampling of burned sites
all_trees_burned <- trees[trees$unitSamplingEpisodeID %in% pre_seID_burned_sites,]

###################################################################################################################
###################################################################################################################
#Add column to data showing expansion factor
unitID_burned_individual <- mapply(function(y)
{
  unitID_burned[pre_seID_burned_sites == all_trees_burned$unitSamplingEpisodeID[y]]
}, 1:length(all_trees_burned[,1]))

#Create a relational table for DBH ranges for nested plot radius
DBH_classes_plot_radius <- data.frame(dbh_class = c(1,2,3), lower_dbh = c(0,3,24), 
                                      upper_dbh = c(2.9, 23.9, 39.9))

#Create a relational table to apply expansions factor.
trees_expansion_factor <- mapply(function(y)
{
  expansion_factor[which(expansion_factor$unitID == unitID_burned_individual[y]),
                   4 + DBH_classes_plot_radius$dbh_class[all_trees_burned$dbh_in[y] >=
                                                           DBH_classes_plot_radius$lower_dbh &  
                                                           all_trees_burned$dbh_in[y] <=
                                                           DBH_classes_plot_radius$upper_dbh]]
}, 1:length(all_trees_burned[,1]))

#Determine the number of plots per site
n_plots <- mapply(function(y)
{
  length(unique(all_trees_burned$plotID[all_trees_burned$unitSamplingEpisodeID == y]))
}, pre_seID_burned_sites)

n_plots_individual <- mapply(function(y)
{
  n_plots[pre_seID_burned_sites == all_trees_burned$unitSamplingEpisodeID[y]]
}, 1:length(all_trees_burned[,1]))


#Add this to all_trees_burned
all_trees_burned <- data.frame(unitID = unitID_burned_individual, all_trees_burned, 
                               expansionFactor = trees_expansion_factor, 
                               n = n_plots_individual)

#Convert all NA values for min and max bole char to zero
#Procedure for data entry was to leave these field blank if tree was not affected by
#fire, but in regards to calculating average bole char they need to have a value of zero.

#It would probably be best to verify that these NAs are indeed trees unaffected by fire
#for each row, but there is no time to do this.

#Quick way to ID errors, look for entries where one field is numeric and one is NA,
#this should not occur under the data entry procdures.
#all_trees_burned[is.na(all_trees_burned$minBoleChar) == T,]
#all_trees_burned[is.na(all_trees_burned$maxBoleChar) == T,]

#Checked, this scenario does not occur

all_trees_burned$minBoleChar_ft[is.na(all_trees_burned$minBoleChar_ft) == T] <- 0
all_trees_burned$maxBoleChar_ft[is.na(all_trees_burned$maxBoleChar_ft) == T] <- 0

#Check again:
#trees_burned[is.na(trees_burned$minBoleChar) == T,]
#trees_burned[is.na(trees_burned$maxBoleChar) == T,]
#No NAs, problem resolved.

#Subset all live trees in burned units
live_trees_burned <- all_trees_burned[all_trees_burned$statusID == 1,]

#Calculate C/R for each tree
CR <- round((((1-(live_trees_burned$htlc_ft/live_trees_burned$ht_ft))*100)/10),2)
live_trees_burned <- data.frame(live_trees_burned, CR = CR)

#Look at how tree counts are distributed by species.
tree_dist_by_species <- vector()
for(i in 1:length(unique(live_trees_burned$speciesID)))
{
  tree_dist_by_species[i] <- length(live_trees_burned[,1][
    live_trees_burned$speciesID == sort(unique(live_trees_burned$speciesID))[i]])
}

#Show table of tree counts by species
#data.frame(speciesID = sort(unique(live_trees_burned$speciesID)), 
#           Name = mapply(function(y) 
#             {
#             species_lut$commonName[species_lut$speciesID == y]}, 
#             sort(unique(live_trees_burned$speciesID))), 
#           Count = tree_dist_by_species)

#Exclude hardwood species. Many of these are considered large shrubs.
#Even bigleaf maple grew as a large shrub where it occurred on Chumstick ZUI.
#Conifer species ID codes
conifer_speciesID <- c(858, 917, 936, 1029, 1030)
live_conifers_burned <- live_trees_burned[live_trees_burned$speciesID %in% conifer_speciesID,]

#Number of trees surveyed vs, number analyzed in this script
i1 <- paste("Total number of trees surveyed: ", length(trees[,1]), sep = "")
i2 <- paste("Total number of trees surveyed at seven burned sites: ", length(all_trees_burned[,1]), sep = "")
i3 <- paste("Total number of live trees surveyed at seven burned sites: ", length(live_trees_burned[,1]), sep = "")
i4 <- paste("Total number of conifers surveyed at seven burned sites: ", length(live_conifers_burned[,1]), sep = "")
ix <- data.frame(TreeCount = c(i1, i2, i3, i4))
ix

abgr <- live_conifers_burned[live_conifers_burned$speciesID == sort(unique(live_trees_burned$speciesID))[1],]
laoc <- live_conifers_burned[live_conifers_burned$speciesID == sort(unique(live_trees_burned$speciesID))[3],]
pico <- live_conifers_burned[live_conifers_burned$speciesID == sort(unique(live_trees_burned$speciesID))[4],]
pipo <- live_conifers_burned[live_conifers_burned$speciesID == sort(unique(live_trees_burned$speciesID))[6],]
psme <- live_conifers_burned[live_conifers_burned$speciesID == sort(unique(live_trees_burned$speciesID))[7],]

#What is the maximum DBH (so I can figure out DBH classes)
max(live_conifers_burned$dbh_in)#38.3
#largest class will be 35-40 inches


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


#WHAT DATA SUBSET WOULD YOU LIKE TO ANALYZE
#all_trees_burned: all individuals sampled in overstory plots. Includes hardwoods > 4.5 ft tall and dead trees.
#live_trees_burned: all live individuals sampled in overstory plots. Includes hardwoods > 4.5 ft tall.
#live_conifers_burned: all conifers sampled in overstory plots.
#abgr: all grand fir trees (> 4.5 ft tall) sampled in overstory plots
#laoc: all western larch trees (> 4.5 ft tall) sampled in overstory plots
#pico: all lodgepole pine trees (> 4.5 ft tall) sampled in overstory plots
#pipo: all ponderosa pine trees (> 4.5 ft tall) sampled in overstory plots
#psme: all Douglas-fir trees (> 4.5 ft tall) sampled in overstory plots
trees_burned <- live_conifers_burned
#Add column to data showing expansion factor
unitID_burned_individual <- mapply(function(y)
{
  unitID_burned[pre_seID_burned_sites == trees_burned$unitSamplingEpisodeID[y]]
}, 1:length(trees_burned[,1]))



###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
#Intermittant step. Run this no matter what you want to analyze.

#Figure out try TPA by 5 inch DBH classes from 0-5 to 35-40.

#Create data frame to accept tree count data by plot for each site
tree_density_template <- data.frame(plot_lut[plot_lut$unitID %in% unitID_burned & 
                                               plot_lut$plotID %in% sort(unique(trees_burned$plotID)),])

###
#0-4.9 inch DBH class
#Subset trees, need to do this twice because DBH class straddles
#nested plot cutoff.
#For 0-2.9"
trees_burned_0_2.9 <- trees_burned[trees_burned$dbh_in < 3,]
#For 3.0-4.9"
trees_burned_3.0_4.9 <- trees_burned[trees_burned$dbh_in >= 3 & 
                                     trees_burned$dbh_in <= 4.9,]

#Count the number of trees in each plot
#For 0-2.9"
trees_burned_0_2.9_count <- mapply(function(y) {
  length(trees_burned_0_2.9$treeID[trees_burned_0_2.9$plotID == y])
}, tree_density_template$plotID)

#For 3.0-4.9"
trees_burned_3.0_4.9_count <- mapply(function(y) {
  length(trees_burned_3.0_4.9$treeID[trees_burned_3.0_4.9$plotID == y])
}, tree_density_template$plotID)

###
#5.0-9.9 inch DBH class
#Subset trees.
trees_burned_5.0_9.9 <- trees_burned[trees_burned$dbh_in >= 5 & 
                                       trees_burned$dbh_in <= 9.9,]

#Count the number of trees in each plot
#For 5.0-9.9"
trees_burned_5.0_9.9_count <- mapply(function(y) {
  length(trees_burned_5.0_9.9$treeID[trees_burned_5.0_9.9$plotID == y])
}, tree_density_template$plotID)

###
#10.0-14.9 inch DBH class
#Subset trees.
trees_burned_10.0_14.9 <- trees_burned[trees_burned$dbh_in >= 10 & 
                                       trees_burned$dbh_in <= 14.9,]

#Count the number of trees in each plot
trees_burned_10.0_14.9_count <- mapply(function(y) {
  length(trees_burned_10.0_14.9$treeID[trees_burned_10.0_14.9$plotID == y])
}, tree_density_template$plotID)

###
#15.0-19.9 inch DBH class
#Subset trees.
trees_burned_15.0_19.9 <- trees_burned[trees_burned$dbh_in >= 15 & 
                                         trees_burned$dbh_in <= 19.9,]

#Count the number of trees in each plot
trees_burned_15.0_19.9_count <- mapply(function(y) {
  length(trees_burned_15.0_19.9$treeID[trees_burned_15.0_19.9$plotID == y])
}, tree_density_template$plotID)

###
#20.0-24.9 inch DBH class
#Subset trees.
trees_burned_20.0_23.9 <- trees_burned[trees_burned$dbh_in >= 20 & 
                                         trees_burned$dbh_in <= 23.9,]

trees_burned_24.0_24.9 <- trees_burned[trees_burned$dbh_in >= 24 & 
                                         trees_burned$dbh_in <= 24.9,]

#Count the number of trees in each plot
trees_burned_20.0_23.9_count <- mapply(function(y) {
  length(trees_burned_20.0_23.9$treeID[trees_burned_20.0_23.9$plotID == y])
}, tree_density_template$plotID)

trees_burned_24.0_24.9_count <- mapply(function(y) {
  length(trees_burned_24.0_24.9$treeID[trees_burned_24.0_24.9$plotID == y])
}, tree_density_template$plotID)

###
#25.0-29.9 inch DBH class
#Subset trees.
trees_burned_25.0_29.9 <- trees_burned[trees_burned$dbh_in >= 25 & 
                                         trees_burned$dbh_in <= 29.9,]

#Count the number of trees in each plot
trees_burned_25.0_29.9_count <- mapply(function(y) {
  length(trees_burned_25.0_29.9$treeID[trees_burned_25.0_29.9$plotID == y])
}, tree_density_template$plotID)

###
#30.0-34.9 inch DBH class
#Subset trees.
trees_burned_30.0_34.9 <- trees_burned[trees_burned$dbh_in >= 30 & 
                                         trees_burned$dbh_in <= 34.9,]

#Count the number of trees in each plot
trees_burned_30.0_34.9_count <- mapply(function(y) {
  length(trees_burned_30.0_34.9$treeID[trees_burned_30.0_34.9$plotID == y])
}, tree_density_template$plotID)

###
#35.0-39.9 inch DBH class
#Subset trees.
trees_burned_35.0_39.9 <- trees_burned[trees_burned$dbh_in >= 35 & 
                                         trees_burned$dbh_in <= 39.9,]

#Count the number of trees in each plot
trees_burned_35.0_39.9_count <- mapply(function(y) {
  length(trees_burned_35.0_39.9$treeID[trees_burned_35.0_39.9$plotID == y])
}, tree_density_template$plotID)


#Combine tree count data into a single data frame.
tree_count_plot <- data.frame(tree_density_template, 
                              count_0_2.9 = trees_burned_0_2.9_count,
                              count_3.0_4.9 = trees_burned_3.0_4.9_count,
                              count_5.0_9.9 = trees_burned_5.0_9.9_count,
                              count_10.0_14.9 = trees_burned_10.0_14.9_count,
                              count_15.0_19.9 = trees_burned_15.0_19.9_count,
                              count_20.0_23.9 = trees_burned_20.0_23.9_count,
                              count_24.0_24.9 = trees_burned_24.0_24.9_count,
                              count_25.0_29.9 = trees_burned_25.0_29.9_count,
                              count_30.0_34.9 = trees_burned_30.0_34.9_count,
                              count_35.0_39.9 = trees_burned_35.0_39.9_count)

###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


#NEXT STEPS CALCULATE SITE-LEVEL VARIABLES
#DATA IS REPORTED FOR ALL TREES
#I.E. OVERSTORY IS NOT STRATIFIED BY DBH CLASS OR OVERSTORY POSITION

###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
###################################################################################################################
#Calculate TPA by site

#Condense tree count data to nested plot dimensions
tree_count_plot_3.0_23.9 <- tree_count_plot[,6] + tree_count_plot[,7] + 
  tree_count_plot[,8] + tree_count_plot[,9] + tree_count_plot[,10]
tree_count_plot_24.0_39.9 <- tree_count_plot[,11] + tree_count_plot[,12] + 
  tree_count_plot[,13] + tree_count_plot[,14]

tree_count_plot_condensed <- data.frame(tree_count_plot[,1:5], 
                                        count_3.0_23.9 = tree_count_plot_3.0_23.9, 
                                        count_24.0_39.9 = tree_count_plot_24.0_39.9)

#Create a table that reference columns in the density conversion factor table.
conversion_factors_col_reference <- c(5, 6, 7)

#Apply conversion factors to calculate TPA for each plot and diameter class
tree_TPA_plot_condensed <- mapply(function(x)
{
  mapply(function(y)
  {
    tree_count_plot_condensed[y,4+x] * 
      (expansion_factor[,conversion_factors_col_reference[x]][
        expansion_factor$unitID == tree_count_plot_condensed$unitID[y]])
  }, 1:length(tree_count_plot_condensed$unitID))
}, 1:length(conversion_factors_col_reference))

#Create a new data frame
tree_TPA_plot_condensed_df <- data.frame(tree_count_plot_condensed[,1:4], tree_TPA_plot_condensed)

#The convert data frame into a data table. Note sure why, but I can't convert 
#tree_count_plot object directly to a data table.
tree_TPA_plot_condensed_dt <- data.table(tree_TPA_plot_condensed_df)

#Create a vector listing column names.
tree_cols_condensed <- c("X1","X2","X3")

#Summarize data by site
tree_TPA_site_mean_condensed <- tree_TPA_plot_condensed_dt[, lapply(.SD, mean, na.rm=TRUE), by=unitID, .SDcols=tree_cols_condensed] 
tree_TPA_site_sd_condensed <- tree_TPA_plot_condensed_dt[, lapply(.SD, sd, na.rm=TRUE), by=unitID, .SDcols=tree_cols_condensed] 

tree_TPA_site_mean_condensed <- as.data.frame(tree_TPA_site_mean_condensed)
tree_TPA_site_sd_condensed <- as.data.frame(tree_TPA_site_sd_condensed)

tree_TPA_site_mean_condensed[,2:4] <- round(tree_TPA_site_mean_condensed[,2:4],0)
tree_TPA_site_sd_condensed[,2:4] <- round(tree_TPA_site_sd_condensed[,2:4],0)

#Create column names
tree_TPA_dbhClass_condensed <- c("mean_0_2.9", "sd_0_2.9",
                       "mean_3.0_23.9", "sd_3.0_23.9",
                       "mean_24.0_39.9", "sd_24.0_39.9")

#Combine means and sds
tpas_condensed <- cbind(as.data.frame(tree_TPA_site_mean_condensed), as.data.frame(tree_TPA_site_sd_condensed)[,-1])

#Re-order means and sds so they are adjacent for each DBH class
tpas_ordered_condensed <- cbind(tpas_condensed[,1:2], tpas_condensed[,5],tpas_condensed[,3],
                                tpas_condensed[,6],tpas_condensed[,4],tpas_condensed[,7])

#Add column names
colnames(tpas_ordered_condensed) <- c("unitID", tree_TPA_dbhClass_condensed)

#Add site names
siteNames_TPA <- mapply(function(y)
{
  site_lut$unitName[site_lut$unitID == y]
}, tpas_ordered_condensed$unitID)

#Add site names
tree_TPAS_site_condensed <- data.frame(siteName = siteNames_TPA, 
                             as.data.frame(tpas_ordered_condensed))

tpa <- data.frame(tree_TPAS_site_condensed, 
               Total = (tree_TPAS_site_condensed[,3] + 
                          tree_TPAS_site_condensed[,5] + 
                          tree_TPAS_site_condensed[,7]))

tpa_summary <- data.frame(siteName = tree_TPAS_site_condensed[,1], 
                  unitID = tree_TPAS_site_condensed[,2], 
                  saplings = tree_TPAS_site_condensed[,3], 
                  trees = tree_TPAS_site_condensed[,5] + tree_TPAS_site_condensed[,7])
###################################################################################################################
###################################################################################################################
#Calculate basal area by site

#Calculate basal area for each tree
tree_BA <- trees_burned$dbh_in^2 * (3.14159265/(4*144))


#Calculate basal area per acre
tree_BA_perAcre <- (tree_BA * (trees_burned$expansionFactor/trees_burned$n))

#Create a new data frame
tree_BA_perAcre_df <- data.frame(unitID = trees_burned$unitID, 
                                 plotID = trees_burned$plotID,
                                 treeTagN0 = trees_burned$treeTagNo,
                                 expansionFactor = trees_burned$expansionFactor,
                                 n = trees_burned$n,
                                 dbh_in = trees_burned$dbh_in,
                                 BA_tree = tree_BA,
                                 BA_perAcre = tree_BA_perAcre)

#The convert data frame into a data table. Note sure why, but I can't convert 
#tree_count_plot object directly to a data table.
tree_BA_perAcre_dt <- data.table(tree_BA_perAcre_df)

#Column name to reference in lapply function
name_.SD <- c("unitID", "BA_perAcre")




#Basal area for Orion 2
sum(tree_BA_perAcre_df$BA_perAcre[tree_BA_perAcre_df$unitID == 12])


#Show TPA for Orion 2
Orion2_TPA <- tree_TPA_plot_condensed_df[tree_TPA_plot_condensed_df$unitID == 12,]
sum_OR2 <- apply(Orion2_TPA[,5:7],1,sum)
mean(sum_OR2)

#TPA for all (including dead trees) at Orion 2
#161     163     165     167     169     171     173     175     177     179 
#57.250  51.525  62.975 183.200  97.325 332.050 120.225 177.475 194.650 372.125 
#Average TPA
#164.88










#Summarize data by plot
tree_BA_perAcre_plot <- tree_BA_perAcre_dt[, lapply(.SD, sum, na.rm=TRUE), by=plotID, .SDcols=name_.SD[2]] 
tree_BA_unitID <- tree_BA_perAcre_dt[, lapply(.SD, unique, na.rm=TRUE), by=plotID, .SDcols=name_.SD[1]] 

#Create a data table to summarize data by site
plot_BA_perAcre_dt <- data.table(tree_BA_unitID, tree_BA_perAcre_plot)

#Summarize data by site
tree_BA_perAcre_site_mean <- plot_BA_perAcre_dt[, lapply(.SD, mean, na.rm=TRUE), by=unitID, .SDcols=name_.SD[2]] 
tree_BA_perAcre_site_sd <- plot_BA_perAcre_dt[, lapply(.SD, sd, na.rm=TRUE), by=unitID, .SDcols=name_.SD[2]] 

tree_BA_perAcre_site_mean <- as.data.frame(tree_BA_perAcre_site_mean)
tree_BA_perAcre_site_sd <- as.data.frame(tree_BA_perAcre_site_sd)

tree_BA_perAcre_site_mean[,2] <- round(tree_BA_perAcre_site_mean[,2],2)
tree_BA_perAcre_site_sd[,2] <- round(tree_BA_perAcre_site_sd[,2],2)

#Combine means and sds
BA_condensed <- cbind(as.data.frame(tree_BA_perAcre_site_mean), 
                                      as.data.frame(tree_BA_perAcre_site_sd)[,-1])

#Add column names
colnames(BA_condensed) <- c("unitID", "mean_BA_perAcre", "StdDev")

#Add site names
siteNames_BA <- mapply(function(y)
{
  site_lut$unitName[site_lut$unitID == y]
}, BA_condensed$unitID)


#Add site names
site_BA_perAcre <- data.frame(siteName = siteNames_BA, 
                                       as.data.frame(BA_condensed))




###################################################################################################################
test <- data.frame(unitID = trees_burned$unitID, dbh = trees_burned$dbh_in, ba_tree = tree_BA, 
                   ef = trees_burned$expansionFactor, ef10 = trees_burned$expansionFactor/10, 
                   ba_acre = tree_BA*(trees_burned$expansionFactor/10))
###################################################################################################################

###################################################################################################################
###################################################################################################################
#Calculate measured crown volume scorched for by site

#Calculate values for each tree multiplied by the expansion factor. Intermittent step for calculating
#weighted mean.
expansion_value_CVS <- trees_burned$crownScorch * trees_burned$expansionFactor

#Create a new data frame
tree_CVS_df <- data.frame(unitID = unitID_burned_individual, 
                          plotID = trees_burned$plotID, 
                          treeTagN0 = trees_burned$treeTagNo, 
                          crownScorch = trees_burned$crownScorch,
                          ExpansionValue = expansion_value_CVS, 
                          ExpansionFactor = trees_burned$expansionFactor)


#The convert data frame into a data table. Note sure why, but I can't convert 
#tree_count_plot object directly to a data table.
tree_CVS_dt <- data.table(tree_CVS_df)

#Column name to reference in lapply function
nameCVS_.SD <- c("unitID", "crownScorch", "ExpansionValue", "ExpansionFactor")

######################################################################################################################
#Uses weighted means to adjust for differences in overstory plot size
#NOTE: Only Hanlon and Angel have variable overstory radii and Hanlon has no burned trees.

#Calculate weighted mean crown volume scorched based on plot size.
weighted_sum_CVS_plot <- tree_CVS_dt[, lapply(.SD, sum, na.rm=TRUE), by=plotID, .SDcols=nameCVS_.SD[3]] 
weighted_n_CVS_plot <- tree_CVS_dt[, lapply(.SD, sum, na.rm=TRUE), by=plotID, .SDcols=nameCVS_.SD[4]] 
weighted_unitID_CVS_plot <- tree_CVS_dt[, lapply(.SD, unique, na.rm=TRUE), by=plotID, .SDcols=nameCVS_.SD[1]] 
weighted_mean_CVS_plot <- weighted_sum_CVS_plot$ExpansionValue / weighted_n_CVS_plot$ExpansionFactor

#Create a data table to summarize data by site
plot_weighted_CVS_dt <- data.table(weighted_unitID_CVS_plot, crownScorch = weighted_mean_CVS_plot)

#Summarize data by site
tree_weighted_CVS_site_mean <- plot_weighted_CVS_dt[, lapply(.SD, mean, na.rm=TRUE), by=unitID, .SDcols=nameCVS_.SD[2]] 
tree_weighted_CVS_site_sd <- plot_weighted_CVS_dt[, lapply(.SD, sd, na.rm=TRUE), by=unitID, .SDcols=nameCVS_.SD[2]] 

tree_weighted_CVS_site_mean <- as.data.frame(tree_weighted_CVS_site_mean)
tree_weighted_CVS_site_sd <- as.data.frame(tree_weighted_CVS_site_sd)

tree_weighted_CVS_site_mean[,2] <- round(tree_weighted_CVS_site_mean[,2],2)
tree_weighted_CVS_site_sd[,2] <- round(tree_weighted_CVS_site_sd[,2],2)

#Combine means and sds
CVS_weighted_condensed <- cbind(as.data.frame(tree_weighted_CVS_site_mean), 
                       as.data.frame(tree_weighted_CVS_site_sd)[,-1])

#Add column names
colnames(CVS_weighted_condensed) <- c("unitID", "mean_crownScorch", "StdDev")

#Add site names
siteNames_weighted_CVS <- mapply(function(y)
{
  site_lut$unitName[site_lut$unitID == y]
}, CVS_weighted_condensed$unitID)

site_weighted_CVS <- data.frame(siteName = siteNames_weighted_CVS, 
                       as.data.frame(CVS_weighted_condensed))

###################################################################################################################
###################################################################################################################
#Calculate bole char by site


#Calculate average bole char =  (min + max)/2
aveBoleChar <- ((trees_burned$minBoleChar_ft + trees_burned$maxBoleChar_ft)/2)

#Calculate values for each tree multiplied by the expansion factor. Intermittent step for calculating
#weighted mean.
expansion_value_aveBoleChar <- aveBoleChar * trees_burned$expansionFactor

#Create a new data frame
tree_boleChar_df <- data.frame(unitID = unitID_burned_individual, 
                               plotID = trees_burned$plotID, 
                               treeTagN0 = trees_burned$treeTagNo, 
                               minBoleChar = trees_burned$minBoleChar_ft, 
                               maxBoleChar = trees_burned$maxBoleChar_ft, 
                               aveBoleChar = aveBoleChar,
                               ExpansionValue = expansion_value_aveBoleChar, 
                               ExpansionFactor = trees_burned$expansionFactor)

#The convert data frame into a data table. Note sure why, but I can't convert 
#tree_count_plot object directly to a data table.
tree_boleChar_dt <- data.table(tree_boleChar_df)

#Column name to reference in lapply function
nameChar_.SD <- c("unitID", "aveBoleChar", "ExpansionValue", "ExpansionFactor")

######################################################################################################################
#Uses weighted means to adjust for differences in overstory plot size
#NOTE: Only Hanlon and Angel have variable overstory radii and Hanlon has no burned trees.

#Calculate weighted mean crown volume scorched based on plot size.
weighted_sum_boleChar_plot <- tree_boleChar_dt[, lapply(.SD, sum, na.rm=TRUE), by=plotID, .SDcols=nameChar_.SD[3]] 
weighted_n_boleChar_plot <- tree_boleChar_dt[, lapply(.SD, sum, na.rm=TRUE), by=plotID, .SDcols=nameChar_.SD[4]] 
weighted_unitID_boleChar_plot <- tree_boleChar_dt[, lapply(.SD, unique, na.rm=TRUE), by=plotID, .SDcols=nameChar_.SD[1]] 
weighted_mean_boleChar_plot <- weighted_sum_boleChar_plot$ExpansionValue / weighted_n_boleChar_plot$ExpansionFactor

#Create a data table to summarize data by site
plot_weighted_boleChar_dt <- data.table(weighted_unitID_boleChar_plot, aveBoleChar = weighted_mean_boleChar_plot)

#Summarize data by site
tree_weighted_boleChar_site_mean <- plot_weighted_boleChar_dt[, lapply(.SD, mean, na.rm=TRUE), by=unitID, .SDcols=nameChar_.SD[2]] 
tree_weighted_boleChar_site_sd <- plot_weighted_boleChar_dt[, lapply(.SD, sd, na.rm=TRUE), by=unitID, .SDcols=nameChar_.SD[2]] 

tree_weighted_boleChar_site_mean <- as.data.frame(tree_weighted_boleChar_site_mean)
tree_weighted_boleChar_site_sd <- as.data.frame(tree_weighted_boleChar_site_sd)

tree_weighted_boleChar_site_mean[,2] <- round(tree_weighted_boleChar_site_mean[,2],2)
tree_weighted_boleChar_site_sd[,2] <- round(tree_weighted_boleChar_site_sd[,2],2)

#Combine means and sds
boleChar_weighted_condensed <- cbind(as.data.frame(tree_weighted_boleChar_site_mean), 
                                     as.data.frame(tree_weighted_boleChar_site_sd)[,-1])

#Add column names
colnames(boleChar_weighted_condensed) <- c("unitID", "mean_boleChar", "StdDev")

#Add site names
siteNames_weighted_boleChar <- mapply(function(y)
{
  site_lut$unitName[site_lut$unitID == y]
}, boleChar_weighted_condensed$unitID)

site_weighted_boleChar <- data.frame(siteName = siteNames_weighted_boleChar, 
                                     as.data.frame(boleChar_weighted_condensed))

###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


#NEXT STEP IS FOR CALCULATING FOFEM INPUTS BY 5-INCH DBH CLASSES
#INCLUDES TREE DENSITY, DBH, HEIGHT, AND CR
#CROWN SCORCH IS REPORTED AS AN AVERAGE FOR ALL DBH CLASSES.


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
###################################################################################################################
#Calculate scorch height by site

#Create a table to hold scorch height values.
tree_SH_site_matrix <- matrix(data = 0, nrow = bux, ncol = 4)
colnames(tree_SH_site_matrix) <- c("unitID", "episodeID", "mean_crownScorch", "StdDev")
tree_SH_site_df <- data.frame(tree_SH_site_matrix)
tree_SH_site_df$unitID <- unitID_burned
tree_SH_site_df$episodeID <- pre_seID_burned_sites

#Run script for each site.
for(t in 1:bux)
{#A2 >> START---------------------------------------------------------------------------------------------------------------
  
  trees_burned_Xsite <- trees_burned[trees_burned$unitID == unitID_burned[t],]
  
  
  #Calculate values for each tree multiplied by the expansion factor. Intermittent step for calculating
  #weighted mean.
  expansion_value_scorchHt <- trees_burned_Xsite$maxScorchHt_ft * trees_burned_Xsite$expansionFactor
  expansion_value_scorchHt[is.na(expansion_value_scorchHt)] <- 0
  
  #Create a new data frame
  tree_scorchHt_df <- data.frame(unitSamplingEpisodeID = trees_burned_Xsite$unitSamplingEpisodeID, 
                                 plotID = trees_burned_Xsite$plotID, 
                                 treeTagN0 = trees_burned_Xsite$treeTagNo,
                                 scorchHt = trees_burned_Xsite$maxScorchHt_ft,
                                 ExpansionValue = expansion_value_scorchHt, 
                                 ExpansionFactor = trees_burned_Xsite$expansionFactor)
  
  #The convert data frame into a data table. Note sure why, but I can't convert 
  #tree_count_plot object directly to a data table.
  tree_scorchHt_dt <- data.table(tree_scorchHt_df)
  
  #Column name to reference in lapply function
  nameScorchHt_.SD <- c("unitSamplingEpisodeID", "scorchHt", "ExpansionValue", "ExpansionFactor")
  
  ######################################################################################################################
  #Uses weighted means to adjust for differences in overstory plot size
  #NOTE: Only Hanlon and Angel have variable overstory radii and Hanlon has no burned trees.
  
  #Calculate weighted mean crown volume scorched based on plot size.
  weighted_sum_scorchHt_plot <- tree_scorchHt_dt[, lapply(.SD, sum, na.rm=TRUE), by=plotID, .SDcols=nameScorchHt_.SD[3]] 
  weighted_n_scorchHt_plot <- tree_scorchHt_dt[, lapply(.SD, sum, na.rm=TRUE), by=plotID, .SDcols=nameScorchHt_.SD[4]] 
  weighted_unitID_scorchHt_plot <- tree_scorchHt_dt[, lapply(.SD, unique, na.rm=TRUE), by=plotID, .SDcols=nameScorchHt_.SD[1]] 
  weighted_mean_scorchHt_plot <- weighted_sum_scorchHt_plot$ExpansionValue / weighted_n_scorchHt_plot$ExpansionFactor
  
  #Create a data table to summarize data by site
  plot_weighted_scorchHt_dt <- data.table(weighted_unitID_scorchHt_plot, scorchHt = weighted_mean_scorchHt_plot)
  
  #Summarize data by site
  tree_weighted_scorchHt_site_mean <- plot_weighted_scorchHt_dt[, lapply(.SD, mean, na.rm=TRUE), by=unitSamplingEpisodeID, .SDcols=nameScorchHt_.SD[2]] 
  tree_weighted_scorchHt_site_sd <- plot_weighted_scorchHt_dt[, lapply(.SD, sd, na.rm=TRUE), by=unitSamplingEpisodeID, .SDcols=nameScorchHt_.SD[2]] 
  
  tree_weighted_scorchHt_site_mean <- as.data.frame(tree_weighted_scorchHt_site_mean)
  tree_weighted_scorchHt_site_sd <- as.data.frame(tree_weighted_scorchHt_site_sd)
  
  tree_weighted_scorchHt_site_mean[,2] <- round(tree_weighted_scorchHt_site_mean[,2],0)
  tree_weighted_scorchHt_site_sd[,2] <- round(tree_weighted_scorchHt_site_sd[,2],0)
  
  #Combine means and sds
  scorchHt_weighted_condensed <- cbind(as.data.frame(tree_weighted_scorchHt_site_mean), 
                                       as.data.frame(tree_weighted_scorchHt_site_sd)[,-1])
  
  #Add column names
  colnames(scorchHt_weighted_condensed) <- c("unitSamplingEpisodeID", "mean_crownScorch", "StdDev")
  
  for(e in 1:bux)
  {#B1 >> START-------------------------------------------------------------------------------------------------------
   if(any(scorchHt_weighted_condensed[,1] == pre_seID_burned_sites[e]) == T)
   {#C1 >> START------------------------------------------------------------------------------------------------------
    tree_SH_site_df[,3][
      tree_SH_site_df[,1] == unitID_burned[
        pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- scorchHt_weighted_condensed[,2][
          scorchHt_weighted_condensed[,1] == pre_seID_burned_sites[e]]
    tree_SH_site_df[,4][
      tree_SH_site_df[,1] == unitID_burned[
        pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- scorchHt_weighted_condensed[,3][
          scorchHt_weighted_condensed[,1] == pre_seID_burned_sites[e]]
   } else #C1 >> END---------------------------------------------------------------------------------------------------
{#C2 >> START----------------------------------------------------------------------------------------------------------
 #Nothing here
}#C2 >> END------------------------------------------------------------------------------------------------------------
  }#B1 >> END----------------------------------------------------------------------------------------------------------
}#A1 >> END------------------------------------------------------------------------------------------------------------

#WHAT DATA SUBSET WOULD YOU LIKE TO ANALYZE
#all_trees_burned: all individuals sampled in overstory plots. Includes hardwoods > 4.5 ft tall and dead trees.
#live_trees_burned: all live individuals sampled in overstory plots. Includes hardwoods > 4.5 ft tall.
#live_conifers_burned: all conifers sampled in overstory plots.
#abgr: all grand fir trees (> 4.5 ft tall) sampled in overstory plots
#laoc: all western larch trees (> 4.5 ft tall) sampled in overstory plots
#pico: all lodgepole pine trees (> 4.5 ft tall) sampled in overstory plots
#pipo: all ponderosa pine trees (> 4.5 ft tall) sampled in overstory plots
#psme: all Douglas-fir trees (> 4.5 ft tall) sampled in overstory plots

#Create a vector to reference overstory data subsetted by species
by_species <- c("abgr", "laoc", "pico", "pipo", "psme")

#Create a table to hold data
#FOFEM_inputs_A <- data.frame()

for(f in 1:5)
{
trees_burned <- get(by_species[f])


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
#Intermittant step. Run this no matter what you want to analyze.

#Figure out try TPA by 5 inch DBH classes from 0-5 to 35-40.

#Create data frame to accept tree count data by plot for each site
tree_density_template <- data.frame(plot_lut[plot_lut$unitID %in% unitID_burned,])

###
#0-4.9 inch DBH class
#Subset trees, need to do this twice because DBH class straddles
#nested plot cutoff.
#For 0-2.9"
trees_burned_0_2.9 <- trees_burned[trees_burned$dbh_in < 3,]
#For 3.0-4.9"
trees_burned_3.0_4.9 <- trees_burned[trees_burned$dbh_in >= 3 & 
                                       trees_burned$dbh_in <= 4.9,]

#Count the number of trees in each plot
#For 0-2.9"
trees_burned_0_2.9_count <- mapply(function(y) {
  length(trees_burned_0_2.9$treeID[trees_burned_0_2.9$plotID == y])
}, tree_density_template$plotID)

#For 3.0-4.9"
trees_burned_3.0_4.9_count <- mapply(function(y) {
  length(trees_burned_3.0_4.9$treeID[trees_burned_3.0_4.9$plotID == y])
}, tree_density_template$plotID)

###
#5.0-9.9 inch DBH class
#Subset trees.
trees_burned_5.0_9.9 <- trees_burned[trees_burned$dbh_in >= 5 & 
                                       trees_burned$dbh_in <= 9.9,]

#Count the number of trees in each plot
#For 5.0-9.9"
trees_burned_5.0_9.9_count <- mapply(function(y) {
  length(trees_burned_5.0_9.9$treeID[trees_burned_5.0_9.9$plotID == y])
}, tree_density_template$plotID)

###
#10.0-14.9 inch DBH class
#Subset trees.
trees_burned_10.0_14.9 <- trees_burned[trees_burned$dbh_in >= 10 & 
                                         trees_burned$dbh_in <= 14.9,]

#Count the number of trees in each plot
trees_burned_10.0_14.9_count <- mapply(function(y) {
  length(trees_burned_10.0_14.9$treeID[trees_burned_10.0_14.9$plotID == y])
}, tree_density_template$plotID)

###
#15.0-19.9 inch DBH class
#Subset trees.
trees_burned_15.0_19.9 <- trees_burned[trees_burned$dbh_in >= 15 & 
                                         trees_burned$dbh_in <= 19.9,]

#Count the number of trees in each plot
trees_burned_15.0_19.9_count <- mapply(function(y) {
  length(trees_burned_15.0_19.9$treeID[trees_burned_15.0_19.9$plotID == y])
}, tree_density_template$plotID)

###
#20.0-24.9 inch DBH class
#Subset trees.
trees_burned_20.0_23.9 <- trees_burned[trees_burned$dbh_in >= 20 & 
                                         trees_burned$dbh_in <= 23.9,]

trees_burned_24.0_24.9 <- trees_burned[trees_burned$dbh_in >= 24 & 
                                         trees_burned$dbh_in <= 24.9,]

#Count the number of trees in each plot
trees_burned_20.0_23.9_count <- mapply(function(y) {
  length(trees_burned_20.0_23.9$treeID[trees_burned_20.0_23.9$plotID == y])
}, tree_density_template$plotID)

trees_burned_24.0_24.9_count <- mapply(function(y) {
  length(trees_burned_24.0_24.9$treeID[trees_burned_24.0_24.9$plotID == y])
}, tree_density_template$plotID)

###
#25.0-29.9 inch DBH class
#Subset trees.
trees_burned_25.0_29.9 <- trees_burned[trees_burned$dbh_in >= 25 & 
                                         trees_burned$dbh_in <= 29.9,]

#Count the number of trees in each plot
trees_burned_25.0_29.9_count <- mapply(function(y) {
  length(trees_burned_25.0_29.9$treeID[trees_burned_25.0_29.9$plotID == y])
}, tree_density_template$plotID)

###
#30.0-34.9 inch DBH class
#Subset trees.
trees_burned_30.0_34.9 <- trees_burned[trees_burned$dbh_in >= 30 & 
                                         trees_burned$dbh_in <= 34.9,]

#Count the number of trees in each plot
trees_burned_30.0_34.9_count <- mapply(function(y) {
  length(trees_burned_30.0_34.9$treeID[trees_burned_30.0_34.9$plotID == y])
}, tree_density_template$plotID)

###
#35.0-39.9 inch DBH class
#Subset trees.
trees_burned_35.0_39.9 <- trees_burned[trees_burned$dbh_in >= 35 & 
                                         trees_burned$dbh_in <= 39.9,]

#Count the number of trees in each plot
trees_burned_35.0_39.9_count <- mapply(function(y) {
  length(trees_burned_35.0_39.9$treeID[trees_burned_35.0_39.9$plotID == y])
}, tree_density_template$plotID)


#Combine tree count data into a single data frame.
tree_count_plot <- data.frame(tree_density_template, 
                              count_0_2.9 = trees_burned_0_2.9_count,
                              count_3.0_4.9 = trees_burned_3.0_4.9_count,
                              count_5.0_9.9 = trees_burned_5.0_9.9_count,
                              count_10.0_14.9 = trees_burned_10.0_14.9_count,
                              count_15.0_19.9 = trees_burned_15.0_19.9_count,
                              count_20.0_23.9 = trees_burned_20.0_23.9_count,
                              count_24.0_24.9 = trees_burned_24.0_24.9_count,
                              count_25.0_29.9 = trees_burned_25.0_29.9_count,
                              count_30.0_34.9 = trees_burned_30.0_34.9_count,
                              count_35.0_39.9 = trees_burned_35.0_39.9_count)



###################################################################################################################
###################################################################################################################
#Calculate 
#Create a table that reference columns in the density conversion factor table.
conversion_factors_col_reference <- c(5, rep(6,5), rep(7,4))

#Apply conversion factors to calculate TPA for each plot and diamater class
tree_TPA_plot <- mapply(function(x)
  {
  mapply(function(y)
    {
    tree_count_plot[y,4+x] * 
      expansion_factor[,
                                conversion_factors_col_reference[x]][expansion_factor$unitID == tree_count_plot$unitID[y]]
    }, 1:length(tree_count_plot$unitID))
}, 1:length(conversion_factors_col_reference))

#Add TPA for DBH classes that straddle nested plot DBH cutoffs
mean_0_4.9 <- tree_TPA_plot[,1] + tree_TPA_plot[,2]
mean_20_24.9 <- tree_TPA_plot[,6] + tree_TPA_plot[,7]

#Remove old DBH classes
tree_TPA_plot <- tree_TPA_plot[,-c(1,2,6,7)]

#Add new DBH classes
tree_TPA_plot <- cbind(mean_0_4.9, tree_TPA_plot[,1:3], mean_20_24.9, tree_TPA_plot[,4:6])

#Return to a matrix with no col names
tree_TPA_plot <- matrix(as.vector(tree_TPA_plot),140,8)

#Create a new data frame
tree_TPA_plot_df <- data.frame(tree_count_plot[,1:4], tree_TPA_plot)
#The convert data frame into a data table. Note sure why, but I can't convert 
#tree_count_plot object directly to a data table.
tree_TPA_plot_dt <- data.table(tree_TPA_plot_df)

#Create a vector listing column names.
tree_cols <- c("X1","X2","X3","X4","X5","X6","X7","X8")

#Summarize data by site
tree_TPA_site_mean <- tree_TPA_plot_dt[, lapply(.SD, mean, na.rm=TRUE), by=unitID, .SDcols=tree_cols] 
tree_TPA_site_sd <- tree_TPA_plot_dt[, lapply(.SD, sd, na.rm=TRUE), by=unitID, .SDcols=tree_cols] 

tree_TPA_site_mean <- as.data.frame(tree_TPA_site_mean)
tree_TPA_site_sd <- as.data.frame(tree_TPA_site_sd)

tree_TPA_site_mean[,2:9] <- round(tree_TPA_site_mean[,2:9],0)
tree_TPA_site_sd[,2:9] <- round(tree_TPA_site_sd[,2:9],0)

#Create column names
tree_TPA_dbhClass <- c("mean_0_4.9", "sd_0_4.9",
                             "mean_5.0_9.9", "sd_5.0_9.9",
                             "mean_10.0_14.9", "sd_10.0_14.9",
                             "mean_15.0_19.9", "sd_15.0_19.9",
                             "mean_20.0_24.9", "sd_20.0_24.9",
                             "mean_25.0_29.9", "sd_25.0_29.9",
                             "mean_30.0_34.9", "sd_30.0_34.9",
                             "mean_35.0_39.9", "sd_35.0_39.9")

#Combine means and sds
tpas <- cbind(as.data.frame(tree_TPA_site_mean), as.data.frame(tree_TPA_site_sd)[,-1])

#Re-order means and sds so they are adjacent for each DBH class
tpas_ordered <- cbind(tpas[,1:2], tpas[,10],tpas[,3],tpas[,11],tpas[,4],tpas[,12],tpas[,5], 
                               tpas[,13],tpas[,6],tpas[,14],tpas[,7],tpas[,15],tpas[,8],tpas[,16], 
                               tpas[,9],tpas[,17])

#Add column names
colnames(tpas_ordered) <- c("unitID", tree_TPA_dbhClass)

#Create a data frame for larger table
tree_TPA_site <- data.frame(unitID = tpas_ordered[,1], 
                            mean_0_4.9 = tpas_ordered[,2], 
                            mean_5.0_9.9 = tpas_ordered[,4], 
                            mean_10.0_14.9 = tpas_ordered[,6], 
                            mean_15.0_19.9 = tpas_ordered[,8], 
                            mean_20.0_24.9 = tpas_ordered[,10], 
                            mean_25.0_29.9 = tpas_ordered[,12], 
                            mean_30.0_34.9 = tpas_ordered[,14], 
                            mean_35.0_39.9 = tpas_ordered[,16])


###################################################################################################################
###################################################################################################################
#Template objects used in sections below
tree_dbhClasses <- c("trees_burned_0_2.9",
                          "trees_burned_3.0_4.9",
                          "trees_burned_5.0_9.9",
                          "trees_burned_10.0_14.9",
                          "trees_burned_15.0_19.9",
                          "trees_burned_20.0_23.9",
                          "trees_burned_24.0_24.9",
                          "trees_burned_25.0_29.9",
                          "trees_burned_30.0_34.9",
                          "trees_burned_35.0_39.9")

tree_Var_site_colnames <- c("unitID", "episodeID", 
                            "mean_0_2.9", "ef_0_2.9", "n_0_2.9", 
                            "mean_3.0_4.9", "ef_3.0_4.9", "n_3.0_4.9",
                            "mean_5.0_9.9", "ef_5.0_9.9", "n_5.0_9.9",
                            "mean_10.0_14.9", "ef_10.0_14.9", "n_10.0_14.9", 
                            "mean_15.0_19.9", "ef_15.0_19.9", "n_15.0_19.9", 
                            "mean_20.0_23.9", "ef_20.0_23.9", "n_20.0_23.9", 
                            "mean_24.0_24.9", "ef_24.0_24.9", "n_24.0_24.9", 
                            "mean_25.0_29.9", "ef_25.0_29.9", "n_25.0_29.9", 
                            "mean_30.0_34.9", "ef_30.0_34.9", "n_30.0_34.9", 
                            "mean_35.0_39.9", "ef_35.0_39.9", "n_35.0_39.9")

###################################################################################################################
###################################################################################################################
#Calculate average height by 5 inch DBH classes from 0-5 to 35-40.
tree_Height_site_matrix <- matrix(data = 0, nrow = bux, ncol = 32)
colnames(tree_Height_site_matrix) <- tree_Var_site_colnames
tree_Height_site_df <- data.frame(tree_Height_site_matrix)
tree_Height_site_df$unitID <- unitID_burned
tree_Height_site_df$episodeID <- pre_seID_burned_sites
for(i in 1:length(tree_dbhClasses))
{#A1 >> START-------------------------------------------------------------------
 dply_temp <- ddply(get(tree_dbhClasses[i]), ~unitSamplingEpisodeID, 
                    summarise, 
                    mean_ht = round(mean(ht_ft),0), 
                    expansionFactor = unique(expansionFactor), 
                    n = sum(statusID))

for(e in 1:bux)
 {#B1 >> START-----------------------------------------------------------------
  if(any(dply_temp[,1] == pre_seID_burned_sites[e]) == T)
  {#C1 >> START---------------------------------------------------------------
   tree_Height_site_df[,(i*3)][
     tree_Height_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,2][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_Height_site_df[,((i*3)+1)][
     tree_Height_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,3][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_Height_site_df[,((i*3)+2)][
     tree_Height_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,4][
         dply_temp[,1] == pre_seID_burned_sites[e]]
  } else #C1 >> END-----------------------------------------------------------
{#C2 >> START---------------------------------------------------------------
 #Nothing here
}#C2 >> END-----------------------------------------------------------------
 }#B1 >> END---------------------------------------------------------------------
}#A1 >> END-----------------------------------------------------------------------

weighted_mean_0_4.9 <- round(((tree_Height_site_df$mean_0_2.9 * (tree_Height_site_df$ef_0_2.9 * tree_Height_site_df$n_0_2.9)) + 
  (tree_Height_site_df$mean_3.0_4.9 * (tree_Height_site_df$ef_3.0_4.9 * tree_Height_site_df$n_3.0_4.9)))/
  ((tree_Height_site_df$ef_0_2.9 * tree_Height_site_df$n_0_2.9) + 
     (tree_Height_site_df$ef_3.0_4.9 * tree_Height_site_df$n_3.0_4.9)),0)
weighted_mean_0_4.9[is.nan(weighted_mean_0_4.9) == T] <- 0

weighted_mean_20_24.9 <- round(((tree_Height_site_df$mean_20.0_23.9 * (tree_Height_site_df$ef_20.0_23.9 * 
                                                                         tree_Height_site_df$n_20.0_23.9)) + 
                                (tree_Height_site_df$mean_24.0_24.9 * (tree_Height_site_df$ef_24.0_24.9 * 
                                                                         tree_Height_site_df$n_24.0_24.9)))/
                               ((tree_Height_site_df$ef_20.0_23.9 * tree_Height_site_df$n_20.0_23.9) + 
                                  (tree_Height_site_df$ef_24.0_24.9 * tree_Height_site_df$n_24.0_24.9)),0)
weighted_mean_20_24.9[is.nan(weighted_mean_20_24.9) == T] <- 0
  
weighted_tree_Height_site <- data.frame(unitID = tree_Height_site_df$unitID, mean_0_4.9 = weighted_mean_0_4.9, 
                                        mean_5.0_9.9 = tree_Height_site_df$mean_5.0_9.9, 
                                        mean_10.0_14.9= tree_Height_site_df$mean_10.0_14.9, 
                                        mean_15.0_19.9= tree_Height_site_df$mean_15.0_19.9,
                                        mean_20_24.9 = weighted_mean_20_24.9,
                                        mean_25.0_29.9 = tree_Height_site_df$mean_25.0_29.9,
                                        mean_30.0_34.9 = tree_Height_site_df$mean_30.0_34.9,
                                        mean_35.0_39.9 = tree_Height_site_df$mean_35.0_39.9)

###################################################################################################################
###################################################################################################################
#Calculate average DBH by 5 inch DBH classes from 0-5 to 35-40.
tree_DBH_site_matrix <- matrix(data = 0, nrow = length(pre_seID_burned_sites), ncol = 32)
colnames(tree_DBH_site_matrix) <- tree_Var_site_colnames
tree_DBH_site_df <- data.frame(tree_DBH_site_matrix)
tree_DBH_site_df$unitID <- unitID_burned
tree_DBH_site_df$episodeID <- pre_seID_burned_sites
for(i in 1:length(tree_dbhClasses))
{#A1 >> START-------------------------------------------------------------------
 dply_temp <- ddply(get(tree_dbhClasses[i]), ~unitSamplingEpisodeID, 
                    summarise, 
                    mean_dbh = round(mean(dbh_in),1), 
                    expansionFactor = unique(expansionFactor), 
                    n = sum(statusID))
 
 for(e in 1:bux)
 {#B1 >> START-----------------------------------------------------------------
  if(any(dply_temp[,1] == pre_seID_burned_sites[e]) == T)
  {#C1 >> START---------------------------------------------------------------
   tree_DBH_site_df[,(i*3)][
     tree_DBH_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,2][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_DBH_site_df[,((i*3)+1)][
     tree_DBH_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,3][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_DBH_site_df[,((i*3)+2)][
     tree_DBH_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,4][
         dply_temp[,1] == pre_seID_burned_sites[e]]
  } else #C1 >> END-----------------------------------------------------------
{#C2 >> START---------------------------------------------------------------
 #Nothing here
}#C2 >> END-----------------------------------------------------------------
 }#B1 >> END---------------------------------------------------------------------
}#A1 >> END-----------------------------------------------------------------------

weighted_mean_0_4.9 <- round(((tree_DBH_site_df$mean_0_2.9 * (tree_DBH_site_df$ef_0_2.9 * tree_DBH_site_df$n_0_2.9)) + 
                                (tree_DBH_site_df$mean_3.0_4.9 * (tree_DBH_site_df$ef_3.0_4.9 * tree_DBH_site_df$n_3.0_4.9)))/
                               ((tree_DBH_site_df$ef_0_2.9 * tree_DBH_site_df$n_0_2.9) + 
                                  (tree_DBH_site_df$ef_3.0_4.9 * tree_DBH_site_df$n_3.0_4.9)),1)
weighted_mean_0_4.9[is.nan(weighted_mean_0_4.9) == T] <- 0

weighted_mean_20_24.9 <- round(((tree_DBH_site_df$mean_20.0_23.9 * (tree_DBH_site_df$ef_20.0_23.9 * 
                                                                      tree_DBH_site_df$n_20.0_23.9)) + 
                                  (tree_DBH_site_df$mean_24.0_24.9 * (tree_DBH_site_df$ef_24.0_24.9 * 
                                                                        tree_DBH_site_df$n_24.0_24.9)))/
                                 ((tree_DBH_site_df$ef_20.0_23.9 * tree_DBH_site_df$n_20.0_23.9) + 
                                    (tree_DBH_site_df$ef_24.0_24.9 * tree_DBH_site_df$n_24.0_24.9)),1)
weighted_mean_20_24.9[is.nan(weighted_mean_20_24.9) == T] <- 0

weighted_tree_DBH_site <- data.frame(unitID = tree_DBH_site_df$unitID, mean_0_4.9 = weighted_mean_0_4.9, 
                                     mean_5.0_9.9 = tree_DBH_site_df$mean_5.0_9.9, 
                                     mean_10.0_14.9= tree_DBH_site_df$mean_10.0_14.9, 
                                     mean_15.0_19.9= tree_DBH_site_df$mean_15.0_19.9,
                                     mean_20_24.9 = weighted_mean_20_24.9,
                                     mean_25.0_29.9 = tree_DBH_site_df$mean_25.0_29.9,
                                     mean_30.0_34.9 = tree_DBH_site_df$mean_30.0_34.9,
                                     mean_35.0_39.9 = tree_DBH_site_df$mean_35.0_39.9)

###################################################################################################################
###################################################################################################################
#Calculate average CR by 5 inch DBH classes from 0-5 to 35-40.
tree_CR_site_matrix <- matrix(data = 0, nrow = bux, ncol = 32)
colnames(tree_CR_site_matrix) <- tree_Var_site_colnames
tree_CR_site_df <- data.frame(tree_CR_site_matrix)
tree_CR_site_df$unitID <- unitID_burned
tree_CR_site_df$episodeID <- pre_seID_burned_sites
for(i in 1:length(tree_dbhClasses))
{#A1 >> START-------------------------------------------------------------------
 dply_temp <- ddply(get(tree_dbhClasses[i]), ~unitSamplingEpisodeID, 
                    summarise, 
                    mean_CR = round(mean(CR),0), 
                    expansionFactor = unique(expansionFactor), 
                    n = sum(statusID))
 
 for(e in 1:bux)
 {#B1 >> START-----------------------------------------------------------------
  if(any(dply_temp[,1] == pre_seID_burned_sites[e]) == T)
  {#C1 >> START---------------------------------------------------------------
   tree_CR_site_df[,(i*3)][
     tree_CR_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,2][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_CR_site_df[,((i*3)+1)][
     tree_CR_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,3][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_CR_site_df[,((i*3)+2)][
     tree_CR_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,4][
         dply_temp[,1] == pre_seID_burned_sites[e]]
  } else #C1 >> END-----------------------------------------------------------
{#C2 >> START---------------------------------------------------------------
 #Nothing here
}#C2 >> END-----------------------------------------------------------------
 }#B1 >> END---------------------------------------------------------------------
}#A1 >> END-----------------------------------------------------------------------

weighted_mean_0_4.9 <- round(((tree_CR_site_df$mean_0_2.9 * (tree_CR_site_df$ef_0_2.9 * tree_CR_site_df$n_0_2.9)) + 
                                (tree_CR_site_df$mean_3.0_4.9 * (tree_CR_site_df$ef_3.0_4.9 * tree_CR_site_df$n_3.0_4.9)))/
                               ((tree_CR_site_df$ef_0_2.9 * tree_CR_site_df$n_0_2.9) + 
                                  (tree_CR_site_df$ef_3.0_4.9 * tree_CR_site_df$n_3.0_4.9)),0)
weighted_mean_0_4.9[is.nan(weighted_mean_0_4.9) == T] <- 0

weighted_mean_20_24.9 <- round(((tree_CR_site_df$mean_20.0_23.9 * (tree_CR_site_df$ef_20.0_23.9 * 
                                                                     tree_CR_site_df$n_20.0_23.9)) + 
                                  (tree_CR_site_df$mean_24.0_24.9 * (tree_CR_site_df$ef_24.0_24.9 * 
                                                                       tree_CR_site_df$n_24.0_24.9)))/
                                 ((tree_CR_site_df$ef_20.0_23.9 * tree_CR_site_df$n_20.0_23.9) + 
                                    (tree_CR_site_df$ef_24.0_24.9 * tree_CR_site_df$n_24.0_24.9)),0)
weighted_mean_20_24.9[is.nan(weighted_mean_20_24.9) == T] <- 0

weighted_tree_CR_site <- data.frame(unitID = tree_CR_site_df$unitID, mean_0_4.9 = weighted_mean_0_4.9, 
                                    mean_5.0_9.9 = tree_CR_site_df$mean_5.0_9.9, 
                                    mean_10.0_14.9= tree_CR_site_df$mean_10.0_14.9, 
                                    mean_15.0_19.9= tree_CR_site_df$mean_15.0_19.9,
                                    mean_20_24.9 = weighted_mean_20_24.9,
                                    mean_25.0_29.9 = tree_CR_site_df$mean_25.0_29.9,
                                    mean_30.0_34.9 = tree_CR_site_df$mean_30.0_34.9,
                                    mean_35.0_39.9 = tree_CR_site_df$mean_35.0_39.9)


#Columns in table that contains mean values
mean_cols <- seq(2,9,1)

#Number of columns in FOFEM input table.
col_length <- length(unitID_burned) * length(mean_cols)

#Create a dbh class column
DBH_class <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5)

temp_df <- data.frame(Species = rep(by_species[f], col_length), 
                      unitID = unname(unlist(expand.grid(mean_cols, unitID_burned)[2])),
                      DBH_Class = rep(DBH_class, length(unitID_burned)), 
                      scorchHt = unname(unlist(expand.grid(1:8, tree_SH_site_df$mean_crownScorch)[2])), 
                      Density =  rep(0, col_length),
                      DBH = rep(0, col_length),
                      Height = rep(0, col_length),
                      CR = rep(0, col_length))                      

for(j in 1:length(unitID_burned)) 
{
  if(j == 1)
  {
    temp_df_5 <- unname(unlist(tree_TPA_site[j,]))[-1]
    temp_df_6 <- unname(unlist(weighted_tree_DBH_site[j,]))[-1]
    temp_df_7 <- unname(unlist(weighted_tree_Height_site[j,]))[-1]
    temp_df_8 <- unname(unlist(weighted_tree_CR_site[j,]))[-1]
  } else
  {
    temp_df_5 <- c(temp_df_5, unname(unlist(tree_TPA_site[j,]))[-1])
    temp_df_6 <- c(temp_df_6, unname(unlist(weighted_tree_DBH_site[j,]))[-1])
    temp_df_7 <- c(temp_df_7, unname(unlist(weighted_tree_Height_site[j,]))[-1])
    temp_df_8 <- c(temp_df_8, unname(unlist(weighted_tree_CR_site[j,]))[-1])
  }
}

temp_df[,5] <- temp_df_5
temp_df[,6] <- temp_df_6 
temp_df[,7] <- temp_df_7
temp_df[,8] <- temp_df_8

rm(temp_df_5)
rm(temp_df_6)
rm(temp_df_7)
rm(temp_df_8)

if(f == 1)
{
  FOFEM_df <- temp_df
} else
{
  FOFEM_df <- rbind(FOFEM_df, temp_df)
}
}

#Look for errors in the data. Locations where TPA is zero, but other measures are positive.
test_a <- FOFEM_df[,6] + FOFEM_df[,7] + FOFEM_df[,8]
test_b <- FOFEM_df[,5]
test_c <- data.frame(test_a, test_b)
test_c[test_c[,2] == 0 & test_c[,1] > 0,]
#Shows four locations. I checked these, they are all instances where there is just one tree
#for the diameter class and TPA is rounded down to zero. I will drop these from the FOFEM runs

#Create an input table for FOFEM

#Start by dropping zero values
FOFEM_df_a <- FOFEM_df[FOFEM_df$Density > 0,]

#Subset by site

#Sherman Creek
FOFEM_df_a[FOFEM_df_a$unitID == 4,]

#25 Mile
FOFEM_df_a[FOFEM_df_a$unitID == 14,]

#Orion 2
FOFEM_df_a[FOFEM_df_a$unitID == 12,]

#Angel
FOFEM_df_a[FOFEM_df_a$unitID == 13,]

#Hanlon
FOFEM_df_a[FOFEM_df_a$unitID == 6,]

#Paradise 90
FOFEM_df_a[FOFEM_df_a$unitID == 5,]

#Chumstick ZUI
FOFEM_df_a[FOFEM_df_a$unitID == 16,]



#Post-fire values
###################################################################################################################
###################################################################################################################
#Calculate average CVS by 5 inch CVS classes from 0-5 to 35-40.
tree_CVS_site_matrix <- matrix(data = 0, nrow = bux, ncol = 32)
colnames(tree_CVS_site_matrix) <- tree_Var_site_colnames
tree_CVS_site_df <- data.frame(tree_CVS_site_matrix)
tree_CVS_site_df$unitID <- unitID_burned
tree_CVS_site_df$episodeID <- pre_seID_burned_sites
for(i in 1:10)
{#A1 >> START-------------------------------------------------------------------
 dply_temp <- ddply(get(tree_dbhClasses[i]), ~unitSamplingEpisodeID, 
                    summarise, 
                    mean_CVS = round(mean(crownScorch),1), 
                    expansionFactor = unique(expansionFactor), 
                    n = sum(statusID))
 
 for(e in 1:bux)
 {#B1 >> START-----------------------------------------------------------------
  if(any(dply_temp[,1] == pre_seID_burned_sites[e]) == T)
  {#C1 >> START---------------------------------------------------------------
   tree_CVS_site_df[,(i*3)][
     tree_CVS_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,2][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_CVS_site_df[,((i*3)+1)][
     tree_CVS_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,3][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_CVS_site_df[,((i*3)+2)][
     tree_CVS_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,4][
         dply_temp[,1] == pre_seID_burned_sites[e]]
  } else #C1 >> END-----------------------------------------------------------
{#C2 >> START---------------------------------------------------------------
 #Nothing here
}#C2 >> END-----------------------------------------------------------------
 }#B1 >> END---------------------------------------------------------------------
}#A1 >> END-----------------------------------------------------------------------

weighted_mean_0_4.9 <- round(((tree_CVS_site_df$mean_0_2.9 * (tree_CVS_site_df$ef_0_2.9 * tree_CVS_site_df$n_0_2.9)) + 
                                (tree_CVS_site_df$mean_3.0_4.9 * (tree_CVS_site_df$ef_3.0_4.9 * tree_CVS_site_df$n_3.0_4.9)))/
                               ((tree_CVS_site_df$ef_0_2.9 * tree_CVS_site_df$n_0_2.9) + 
                                  (tree_CVS_site_df$ef_3.0_4.9 * tree_CVS_site_df$n_3.0_4.9)),1)
weighted_mean_0_4.9[is.nan(weighted_mean_0_4.9) == T] <- 0

weighted_mean_20_24.9 <- round(((tree_CVS_site_df$mean_20.0_23.9 * (tree_CVS_site_df$ef_20.0_23.9 * 
                                                                      tree_CVS_site_df$n_20.0_23.9)) + 
                                  (tree_CVS_site_df$mean_24.0_24.9 * (tree_CVS_site_df$ef_24.0_24.9 * 
                                                                        tree_CVS_site_df$n_24.0_24.9)))/
                                 ((tree_CVS_site_df$ef_20.0_23.9 * tree_CVS_site_df$n_20.0_23.9) + 
                                    (tree_CVS_site_df$ef_24.0_24.9 * tree_CVS_site_df$n_24.0_24.9)),1)
weighted_mean_20_24.9[is.nan(weighted_mean_20_24.9) == T] <- 0

weighted_tree_CVS_site <- data.frame(unitID = tree_CVS_site_df$unitID, mean_0_4.9 = weighted_mean_0_4.9, 
                                     mean_5.0_9.9 = tree_CVS_site_df$mean_5.0_9.9, 
                                     mean_10.0_14.9= tree_CVS_site_df$mean_10.0_14.9, 
                                     mean_15.0_19.9= tree_CVS_site_df$mean_15.0_19.9,
                                     mean_20_24.9 = weighted_mean_20_24.9,
                                     mean_25.0_29.9 = tree_CVS_site_df$mean_25.0_29.9,
                                     mean_30.0_34.9 = tree_CVS_site_df$mean_30.0_34.9,
                                     mean_35.0_39.9 = tree_CVS_site_df$mean_35.0_39.9)

###################################################################################################################
###################################################################################################################
#Calculate average Bole Char by 5 inch char classes from 0-5 to 35-40.
tree_char_site_matrix <- matrix(data = 0, nrow = bux, ncol = 32)
colnames(tree_char_site_matrix) <- tree_Var_site_colnames
tree_char_site_df <- data.frame(tree_char_site_matrix)
tree_char_site_df$unitID <- unitID_burned
tree_char_site_df$episodeID <- pre_seID_burned_sites
for(i in 1:10)
{#A1 >> START-------------------------------------------------------------------
 dply_temp <- ddply(get(tree_dbhClasses[i]), ~unitSamplingEpisodeID, 
                    summarise, 
                    mean_char = round(mean(maxBoleChar_ft),1), 
                    expansionFactor = unique(expansionFactor), 
                    n = sum(statusID))
 
 for(e in 1:bux)
 {#B1 >> START-----------------------------------------------------------------
  if(any(dply_temp[,1] == pre_seID_burned_sites[e]) == T)
  {#C1 >> START---------------------------------------------------------------
   tree_char_site_df[,(i*3)][
     tree_char_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,2][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_char_site_df[,((i*3)+1)][
     tree_char_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,3][
         dply_temp[,1] == pre_seID_burned_sites[e]]
   tree_char_site_df[,((i*3)+2)][
     tree_char_site_df[,1] == unitID_burned[
       pre_seID_burned_sites == pre_seID_burned_sites[e]]] <- dply_temp[,4][
         dply_temp[,1] == pre_seID_burned_sites[e]]
  } else #C1 >> END-----------------------------------------------------------
{#C2 >> START---------------------------------------------------------------
 #Nothing here
}#C2 >> END-----------------------------------------------------------------
 }#B1 >> END---------------------------------------------------------------------
}#A1 >> END-----------------------------------------------------------------------

weighted_mean_0_4.9 <- round(((tree_char_site_df$mean_0_2.9 * (tree_char_site_df$ef_0_2.9 * tree_char_site_df$n_0_2.9)) + 
                                (tree_char_site_df$mean_3.0_4.9 * (tree_char_site_df$ef_3.0_4.9 * tree_char_site_df$n_3.0_4.9)))/
                               ((tree_char_site_df$ef_0_2.9 * tree_char_site_df$n_0_2.9) + 
                                  (tree_char_site_df$ef_3.0_4.9 * tree_char_site_df$n_3.0_4.9)),1)
weighted_mean_0_4.9[is.nan(weighted_mean_0_4.9) == T] <- 0

weighted_mean_20_24.9 <- round(((tree_char_site_df$mean_20.0_23.9 * (tree_char_site_df$ef_20.0_23.9 * 
                                                                       tree_char_site_df$n_20.0_23.9)) + 
                                  (tree_char_site_df$mean_24.0_24.9 * (tree_char_site_df$ef_24.0_24.9 * 
                                                                         tree_char_site_df$n_24.0_24.9)))/
                                 ((tree_char_site_df$ef_20.0_23.9 * tree_char_site_df$n_20.0_23.9) + 
                                    (tree_char_site_df$ef_24.0_24.9 * tree_char_site_df$n_24.0_24.9)),1)
weighted_mean_20_24.9[is.nan(weighted_mean_20_24.9) == T] <- 0

weighted_tree_char_site <- data.frame(unitID = tree_char_site_df$unitID, mean_0_4.9 = weighted_mean_0_4.9, 
                                      mean_5.0_9.9 = tree_char_site_df$mean_5.0_9.9, 
                                      mean_10.0_14.9= tree_char_site_df$mean_10.0_14.9, 
                                      mean_15.0_19.9= tree_char_site_df$mean_15.0_19.9,
                                      mean_20_24.9 = weighted_mean_20_24.9,
                                      mean_25.0_29.9 = tree_char_site_df$mean_25.0_29.9,
                                      mean_30.0_34.9 = tree_char_site_df$mean_30.0_34.9,
                                      mean_35.0_39.9 = tree_char_site_df$mean_35.0_39.9)
