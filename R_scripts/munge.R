# read in raw phenology data
#
# Dan Fortin
# 9/25/14

library(plyr)
library(dplyr)

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

year <- 2007

dat <- read.table(file.path("Data_Raw", 
"ChlIndex_Level1_Data", 
paste("subset_50by50_level1_dropout_removed_india_", year, ".csv", sep = "")), sep = ",")


na_row_index <- which(is.na(dat[,1]))
dat3 <- dat[-na_row_index,]
n_time <- nrow(dat3)/50
dat3$time <- rep(1:n_time, each = 50)
time_list <- dlply(dat3, .(time))

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

dat3$LC <- -1
for(i in 1:nrow(dat3)){
	dat3$LC[i] <- land_cover[dat3$locy[i], dat3$locx[i]]
}

# the orientation is up-side-down, this will give correct orientation
dat3$locy <- dat3$locy - 25
dat3$locy <- -1*dat3$locy + 26

# read in data frame with land cover info (e.g., names, plotting colors)
LC_info <- read.csv(file.path("Data_Raw", "LandCover", "LANDCOVER_CLASSES_names_50by50.csv"), header=TRUE, stringsAsFactors = FALSE)

dat3$LC <- factor(dat3$LC, levels <- LC_info$Value, labels = LC_info$Class_name)
dat3$year <- year

saveRDS(dat3, paste("Data/formatted_50by50_", year, ".rds", sep = ""))


# # creating data frame
# new_dat <- data.frame(Time = rep(0, 50*50*length(time_list)), X = 0, locx = 0, locy=0)
# for( tm in 1:length(time_list)){
# 	for(j in 1:50){
# 		for( k in 1:50){
# 			val <- NULL
# 			val$locx <- k
# 			val$locy <- j
# 			val$Time <- tm
# 			val$X <- time_list[[tm]][j, k]
# 			new_dat <- rbind(new_dat, val)
# 		}
# 	}
# }






