############################################################################
### Investigating land cover with homogeneous class
############################################################################

library(plyr)
library(dplyr)
library(ggplot2)

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

dat <- readRDS("Data/formatted_50by50_2007.rds")
LC_info <- read.csv(file.path("Data_Raw", "LandCover", "LANDCOVER_CLASSES_names_50by50.csv"), header=TRUE, stringsAsFactors = FALSE)
############################################################################
### plot 
############################################################################
dat_LC_only <- dat[!duplicated(dat$ID), ] # pull off one row for each curve to look at land cover info

### subset with homogeneous cells for tropical evergreen and tropical semievergreen
dat_hom_LC <- filter(dat_LC_only,  prop_tEvergreen > 80 | prop_tSemievergreen > 80)

ggplot(dat_LC_only, aes(x = locx, y = locy) ) + 
geom_tile(aes(fill = LC), alpha = 0.3) + 
scale_fill_manual(values = LC_info$color ) +
geom_point(data = dat_hom_LC, aes(x = locx, y = locy, color = LC), size = 4, shape = 22)+
scale_color_manual(values = c("chartreuse4", "chartreuse3"))


### pull out the homogeneous data for tropical evergreen vegetation
dat_hom_tEvergreen <- subset(dat, prop_tEvergreen > 80)

### Look for outliers
ggplot(dat_hom_tEvergreen, aes(x = Time, y = X, group = ID, label = as.character(ID))) + 
geom_text(size = 2)+
facet_wrap(~year) + 
theme(legend.position = 'none')

### 2007 Outliers ###
outlier_2007 <- c(1172, 412, 410, 1171, 156)

dat_outlier_2007 <- subset(dat, ID %in% outlier_2007)

ggplot(dat, aes(x = locx, y = locy) ) + 
geom_tile(aes(fill = LC)) + 
scale_fill_manual(values = LC_info$color ) +
geom_point(data = dat_outlier_2007, aes(x = locx, y = locy), color = "maroon4", size = 2)

############################################################################
### Identify clusters of curves for comparison of spatial effects
############################################################################

ggplot(dat_LC_only, aes(x = locx, y = locy, label = ID) ) + 
geom_tile(aes(fill = LC), alpha = 0.3) + 
scale_fill_manual(values = LC_info$color ) +
geom_point(data = dat_hom_LC, aes(x = locx, y = locy, color = LC), size = 4, shape = 22)+
scale_color_manual(values = c("white", "yellow"))+
geom_text(data = dat_hom_LC, aes(x = locx, y = locy), size = 1.5)

c1 <- data.frame(ID = c(507, 509, 510, 559, 512, 513, 563), cluster = 1)
c2 <- data.frame(ID = c(812, 813, 865, 866, 818, 963, 965), cluster = 2)
c3 <- data.frame(ID = c(419, 421, 422, 519, 521, 522, 524, 525, 527, 569, 571, 572, 574, 575, 577,  578, 672, 674, 675, 677, 678 ), cluster = 3)
c4 <- data.frame( ID = c(830, 831, 833, 835, 836, 881, 883, 885, 886), cluster = 4)
cluster_tSemievergreen <- rbind(c1, c2, c3, c4)

dat_cluster <- subset(dat, ID %in% cluster_tSemievergreen$ID)
dat_cluster <- merge(dat_cluster, cluster_tSemievergreen, by = 'ID')

### plot the clusters
ggplot(dat_cluster, aes(x = locx, y=locy, color = as.factor(cluster)))+
geom_point()

### plot the curves colored by cluster 
ggplot(dat_cluster, aes(x = Time, y=X, group = ID, color = as.factor(cluster)))+
geom_line()








