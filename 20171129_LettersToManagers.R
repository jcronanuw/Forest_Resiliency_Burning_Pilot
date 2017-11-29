#PURPOSE OF THIS SCRIPT IS TO PROVIDE STATS FOR LETTERS TO MANAGERS SENT IN NOVEMBER 2017.

#Reset functions
rm(list=ls())
dev.off()

#Libraries

#Set working directory
setwd("C:/Users/jcronan/Documents/GitHub/Forest_Resiliency_Burning_Pilot")

#Open file with tree species metadata:
consumption <- read.table("wad_20170727_AppendixA.csv", header=TRUE, 
                          sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Changle column names to something shorter. Easier to write script.
colnames(consumption) <- c("site", "cat", "pre", "post", "consume", "percent", "fm2", "fm4", 
                           "fall_2", "fall_4", "spring_2", "spring_4")


########################################################################################################
########################################################################################################
#Statistics included in letters to managers (November, 2017).

#First add together 1000-hr sound and rotten columns
consumption_mod1 <- t(mapply(function(y) 
{
  xx <- consumption[grepl("1000", consumption$cat) == T & consumption$site == y,]
  apply(xx[sapply(xx, is.numeric)], 2, sum)
}, unique(consumption$site)))
consumption_mod2 <- data.frame(site = unique(consumption$site), cat = "hr_1000", consumption_mod1)
consumption_mod3 <- rbind(consumption, consumption_mod2)

########################################################################################################
#Create a summarry table showing pre-fire loading (tons/acre) by site (rows) and fuel strata (cols)
pre_summary <- t(mapply(function(y) 
{
  consumption_mod3$pre[consumption_mod3$site == y]
}, 
sort(unique(consumption_mod3$site))))
colnames(pre_summary) <- unique(consumption_mod3$cat)
ps_2 <- data.frame(site = sort(unique(consumption_mod3$site)), pre_summary)

#Create a summary table showing percent contribution to total loading by each fuel stratum.
pctl_1 <- t(mapply(function(y) #pctl = percent contribution to loading
{
  round((ps_2[,2:length(ps_2[1,])][ps_2$site == y,]/ps_2$Total[ps_2$site == y]) * 100, 1)
}, ps_2$site))
pctl_2 <- matrix(unlist(pctl_1),14,11)
pctl_3 <- as.data.frame(pctl_2)
colnames(pctl_3) <- colnames(ps_2[2:length(ps_2[1,])])
pctl_4 <- data.frame(site = ps_2$site, hr_1 = pctl_3$hr_1, hr_10 = pctl_3$hr_10, 
                     hr_100 = pctl_3$hr_100, hr_1000 = pctl_3$hr_1000, litter = pctl_3$litter, 
                     duff = pctl_3$duff, herb = pctl_3$herb, shrub = pctl_3$shrub, 
                     Total = pctl_3$Total)

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










