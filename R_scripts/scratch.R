
library(plyr)
library(dplyr)
library(ggplot2)

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

LC_info <- read.csv(file.path("Data_Raw", "LandCover", "LANDCOVER_CLASSES_names_50by50.csv"), header=TRUE, stringsAsFactors = FALSE)
dat <- readRDS("Data/formatted_50by50_2003.rds")

ggplot(dat, aes(x = locx, y = locy) ) + 
geom_tile(aes(fill = LC)) + 
scale_fill_manual(values = LC_info$color )

dat2 <- subset(dat, !(LC %in% c("Sea", "Water Bodies", "Barren", "Settlement")))
ggplot(dat2, aes(x = Time, y = X)) +
geom_point(aes(color = LC), alpha = 0.8, size = 0.5) + 
geom_smooth(se = FALSE) +
scale_color_manual(values = LC_info$color) +
facet_wrap(~LC) + 
theme(legend.position = 'none')























