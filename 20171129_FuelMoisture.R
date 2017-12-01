#PURPOSE OF THIS SCRIPT IS TO REPORT AND VISUALIZE FUEL MOISTURE DATA

#Reset functions
rm(list=ls())
dev.off()

#Libraries
library(ggplot2)

#Set working directory
setwd("C:/Users/jcronan/Documents/GitHub/Forest_Resiliency_Burning_Pilot/sql_flat_files")

#Open file with tree species metadata:
fm <- read.table("FM_flat_file.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)


#Calculate fuel moisture
net_wet <- fm$grossWetWt_g - fm$wetContWt_g
net_dry <- fm$grossDryWt_g - fm$dryContWt_g
fuel_moisture <- round(((net_wet - net_dry)/net_dry) * 100, 1)

#Create a new data frame with fuel moisture
new_fm <- data.frame(site = fm$unitName, fuel = fm$SampleMaterial, fm = fuel_moisture)

########################################################################################################
#Summarize fuel moisture by site
hanlon <- data.frame(fuel = new_fm$fuel[new_fm$site == "Hanlon"], fm = new_fm$fm[fm$unitName == "Hanlon"])

fuel <- "Downed Woody: 1000-hr"
mean(hanlon$fm[hanlon$fuel == fuel], na.rm = T)
median(hanlon$fm[hanlon$fuel == fuel], na.rm = T)
range(hanlon$fm[hanlon$fuel == fuel], na.rm = T)

########################################################################################################
#Create a box plot comparing duff, 100-hr, and 1000-hr fuels at Hanlon to other sites.
#Subset data for coarse fuels
coarse <- new_fm[new_fm$fuel %in% c("Downed Woody: 100-hr", "Downed Woody: 1000-hr", "Duff"),] 

#Identify and remove outliers
#100-hr
fuel <- "Downed Woody: 100-hr"
plot(coarse$fm[coarse$fuel == fuel], coarse$fm[coarse$fuel == fuel])
range(coarse$fm[coarse$fuel == fuel], na.rm = T)
boxplot(fm ~ site, data = coarse[coarse$fuel == fuel,])
#Looks okay, no outliers

#1000-hr
fuel <- "Downed Woody: 1000-hr"
plot(coarse$fm[coarse$fuel == fuel], coarse$fm[coarse$fuel == fuel])
range(coarse$fm[coarse$fuel == fuel], na.rm = T)
boxplot(fm ~ site, data = coarse[coarse$fuel == fuel,])
#Looks like there are some outliers
#1) One sample is very high, fuel moisture is over 1000%
#Other than that everything looks good
#Remove outlier
coarse <- coarse[!coarse$fm > 1000,]

#Duff
fuel <- "Duff"
plot(coarse$fm[coarse$fuel == fuel], coarse$fm[coarse$fuel == fuel])
range(coarse$fm[coarse$fuel == fuel], na.rm = T)
boxplot(fm ~ site, data = coarse[coarse$fuel == fuel,])
#Looks like there are some outliers
#1) At least one sample has a negative fuel moisture value.
#Other than that everything looks good
#Remove outlier
coarse <- coarse[!coarse$fm < 0,]


boxplot(fm ~ site, data = coarse, 
        boxwex = 0.25, at = 1:7 - 0.3, 
        subset = fuel == "Downed Woody: 100-hr", col = "yellow", 
        main = "Fuel Moisture for Coarse Fuels", 
        xlab = "Prescribed Burn Units", 
        ylab = "Percent Fuel Moisture",
        xlim = c(0.5, 7.5), ylim = c(0, 300), axes = F, yaxs = "i")
boxplot(fm ~ site, data = coarse, add = TRUE, 
        boxwex = 0.25, at = 1:7 + 0,
        subset = fuel == "Downed Woody: 1000-hr", col = "red")
boxplot(fm ~ site, data = coarse, add = T, 
        boxwex = 0.25, at = 1.:7 + 0.3, 
        subset = fuel == "Duff", col = "dark green", axes = F)
legend(2,180, c("100-hr", "1000-hr", "Duff"), fill = c("yellow", "red", "dark green"))


########################################################################################################
#CREATE COLOR-WEIGHTED MATRIX FOR PERCENTAGE CONTRIBUTION OF EACH STRATA TO TOTAL PRE-FIRE SURFACE
#FUEL LOADING

#Create a matrix for the heatmap
pctl_5 <- as.matrix(pctl_4[2:(length(pctl_4[1,])-1)])
rownames(pctl_5) <- pctl_4$site

#Create heatmap
dim_y <- 1:length(pctl_5[,1])#dimensions for transposed y axis (sites)
dim_x <- 1:length(pctl_5[1,])#dimensions for transposed y axis (fuel strata)
par(mai = c(1,2,0.5,0.5))#plotting area for matrix, provides room on y axis for site names.
#Create a color-weighted map of percentage contribution for pre-fire loading
image(dim_x, dim_y, t(pctl_5), axes = F, xlab = "", ylab ="", col = rev(heat.colors(9)), 
      breaks = c(0,1,5,10,20,30,40,50,60,70))
axis(1, dim_x, colnames(pctl_5))#label x axis
axis(2, dim_y, rownames(pctl_5), las = 1)#label y axis
#Add values in color-weighted image.
text(expand.grid(dim_x, dim_y), sprintf("%0.1f", t(pctl_5)), cex =0.6)

########################################################################################################
#Create a summarry table showing measured consumption (tons/acre) by site (rows) and fuel strata (cols)
con_summary <- t(mapply(function(y) 
{
  consumption_mod3$consume[consumption_mod3$site == y]
}, 
sort(unique(consumption_mod3$site))))
colnames(con_summary) <- unique(consumption_mod3$cat)
cs_2 <- data.frame(site = sort(unique(consumption_mod3$site)), con_summary)

#Create a summary table showing percent contribution to total consumption by each fuel stratum.
pctc_1 <- t(mapply(function(y) #pctc = percent contribution to consumption
{
  round((cs_2[,2:length(cs_2[1,])][cs_2$site == y,]/cs_2$Total[cs_2$site == y]) * 100, 1)
}, cs_2$site))
pctc_2 <- matrix(unlist(pctc_1),14,11)
pctc_3 <- as.data.frame(pctc_2)
colnames(pctc_3) <- colnames(ps_2[2:length(ps_2[1,])])
pctc_4 <- data.frame(site = ps_2$site, hr_1 = pctc_3$hr_1, hr_10 = pctc_3$hr_10, 
                     hr_100 = pctc_3$hr_100, hr_1000 = pctc_3$hr_1000, litter = pctc_3$litter, 
                     duff = pctc_3$duff, herb = pctc_3$herb, shrub = pctc_3$shrub, 
                     Total = pctc_3$Total)

#END
########################################################################################################
########################################################################################################










