# read in raw phenology data
#
# Dan Fortin
# 9/25/14

library(plyr)
library(dplyr)

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

year <- 2003

dat <- read.table(file.path("Data_Raw", 
"ChlIndex_Level1_Data", 
paste("subset_50by50_level1_dropout_removed_india_", year, ".csv", sep = "")), sep = ",")


na_row_index <- which(is.na(dat[,1]))
dat3 <- dat[-na_row_index,]
n_time <- nrow(dat3)/50
dat3$time <- rep(1:n_time, each = 50)
time_list <- dlply(dat3, .(time))

#

# creating data frame 
mat <- matrix(0, nrow = 50*50*length(time_list), ncol = 4)
count <- 1
for( tm in 1:length(time_list)){
	for(j in 1:50){
		for( k in 1:50){
			mat[count,]  <- c(k , j, tm, time_list[[tm]][j, k])
			count <- count + 1
		}
	}
}

new_dat <- data.frame(locx = mat[,1], locy = mat[,2], Time = mat[,3], X = mat[,4])

dat3 <- new_dat
########

# sort data
dat3 <- arrange(dat3, locx, locy)

# give each curve an integer ID
dat3$ID <- rep(1:50^2, each = 46)

### include Land Cover 
land_cover <- read.table(file.path("Data_Raw", "LandCover", "LC_data_subset_50by50.csv"), sep = ",")

### read in proportion of land cover for class "Tripical Evergreen (1)" and "Tropical Semievergreen (5)"
prop_tEvergreen <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class1_proportion_south_50by50.csv"), sep = ",")
prop_tSemievergreen <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class5_proportion_south_50by50.csv"), sep = ",")
prop_coastal <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class20_proportion_south_50by50.csv"), sep = ",")
prop_moist_decid <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class8_proportion_south_50by50.csv"), sep = ",")
prop_irrig_intensive_ag <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class32_proportion_south_50by50.csv"), sep = ",")
prop_irrig_ag <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class33_proportion_south_50by50.csv"), sep = ",")
prop_slope_ag <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class34_proportion_south_50by50.csv"), sep = ",")
prop_rainfed_ag <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class35_proportion_south_50by50.csv"), sep = ",")
prop_barren <- read.table(file.path("Data_Raw", "LandCover","Proportions", "class41_proportion_south_50by50.csv"), sep = ",")
### add variables to data frame
dat3$LC <- -1
dat3$prop_tEvergreen <-  -1
dat3$prop_tSemievergreen <- -1
dat3$prop_coastal <- -1
dat3$prop_moist_decid <- -1
dat3$prop_irrig_intensive_ag <- -1
dat3$prop_irrig_ag <- -1
dat3$prop_slope_ag <- -1
dat3$prop_rainfed_ag <- -1
dat3$prop_barren <- -1
dat3$prop_ag <- -1
for(i in 1:nrow(dat3)){
	dat3$LC[i] <- land_cover[dat3$locy[i], dat3$locx[i]]
	dat3$prop_tEvergreen[i] <- prop_tEvergreen[dat3$locy[i], dat3$locx[i]]
	dat3$prop_tSemievergreen[i] <- prop_tSemievergreen[dat3$locy[i], dat3$locx[i]]
	dat3$prop_coastal[i] <- prop_coastal[dat3$locy[i], dat3$locx[i]]
	dat3$prop_moist_decid[i] <- prop_moist_decid[dat3$locy[i], dat3$locx[i]]
	dat3$prop_irrig_intensive_ag[i] <- prop_irrig_intensive_ag[dat3$locy[i], dat3$locx[i]]
	dat3$prop_irrig_ag[i] <- prop_irrig_ag[dat3$locy[i], dat3$locx[i]]
	dat3$prop_slope_ag[i] <- prop_slope_ag[dat3$locy[i], dat3$locx[i]]
	dat3$prop_rainfed_ag[i] <- prop_rainfed_ag[dat3$locy[i], dat3$locx[i]]
	dat3$prop_barren[i] <- prop_barren[dat3$locy[i], dat3$locx[i]]
  dat3$prop_ag[i] <- with(dat3, sum(c(prop_irrig_intensive_ag[i], prop_irrig_ag[i], prop_slope_ag[i], prop_rainfed_ag[i] )))
}

# the orientation is up-side-down, this will give correct orientation
dat3$locy <- dat3$locy - 25
dat3$locy <- -1*dat3$locy + 26

# read in data frame with land cover info (e.g., names, plotting colors)
LC_info <- read.csv(file.path("Data_Raw", "LandCover", "LANDCOVER_CLASSES_names_50by50.csv"), header=TRUE, stringsAsFactors = FALSE)

dat3$LC <- factor(dat3$LC, levels <- LC_info$Value, labels = LC_info$Class_name)
dat3$year <- year

###
year <- 2007
dat3 <- readRDS(paste("Data/formatted_50by50_", year, ".rds", sep = ''))

dat3$prop_tSemievergreen <- dat2003$prop_tSemievergreen
dat3$prop_veg <- with(dat3, sum(c(prop_tEvergreen, prop_tSemievergreen, prop_coastal, prop_moist_decid)))

ag_labels <- c("Rainfed Agriculture", "Slope Agriculture", "Irrigated Agriculture", "Irrigated Intensive Agriculture")
tropical_labels <- c("Tropical Evergreen", "Subtropical Evergreen", "Tropical Semievergreen", "Tropical Moist Deciduous", "Coastal vegetation")

dat3$LC2 <- "other"
dat3$LC2[dat3$LC %in% ag_labels] <- "Agriculture"
dat3$LC2[dat3$LC %in% tropical_labels] <- "Vegetation"
dat3$LC2 <- factor(dat3$LC2, levels = c("Agriculture", "Vegetation", "other"))

saveRDS(dat3, paste("Data/formatted_50by50_", year, ".rds", sep = ""))







