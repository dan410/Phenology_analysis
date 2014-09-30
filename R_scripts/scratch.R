
library(plyr)
library(dplyr)
library(ggplot2)

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

LC_info <- read.csv(file.path("Data_Raw", "LandCover", "LANDCOVER_CLASSES_names_50by50.csv"), header=TRUE, stringsAsFactors = FALSE)
dat <- readRDS("Data/formatted_50by50_2003.rds")


ggplot(dat, aes(x = locx, y = locy) ) + 
geom_tile(aes(fill = LC)) + 
scale_fill_manual(values = LC_info$color )+
geom_point(data = filter(dat, prop_tEvergreen > 80), aes(x = locx, y=locy), color = 'red', size = 0.8)+
geom_point(data = filter(dat, prop_tSemievergreen > 80), aes(x = locx, y=locy), color = 'blue', size = 0.8)

ggplot(dat, aes(x = locx, y = locy) ) + 
geom_tile(aes(color = 'black', fill = LC, alpha = prop_tEvergreen)) + 
scale_color_manual(values = LC_info$color)+
#scale_alpha_continuous()
scale_fill_manual(values = LC_info$color )


geom_point(data = filter(dat, prop_tEvergreen > 80), aes(x = locx, y=locy), color = 'red', size = 1)+
geom_point(data = filter(dat, prop_tSemievergreen > 80), aes(x = locx, y=locy), color = 'blue', size = 1)

############################################################################
### try to use alpha to show proportions of class 
############################################################################
dat_bob <- dat[!duplicated(dat$ID), ]
dat_tEvergreen <- subset(dat_bob, LC == "Tropical Evergreen")

ggplot(dat_bob, aes(x = locx, y = locy) ) + 
geom_tile(aes(alpha = prop_tEvergreen/100), color = "black")  +
geom_point(data = dat_tEvergreen, aes(x = locx, y = locy), color = "green", size = 1.5)+
geom_point(data = filter(dat_tEvergreen, prop_tEvergreen > 80), aes(x = locx, y = locy), color = "red", size = 1)
# ggsave("Plots/map_prop_evergreen.pdf")

# plot proportion of agriculture
ggplot(dat_bob, aes(x = locx, y = locy) ) + 
  geom_tile(aes(alpha = prop_ag/100), color = "black")  +
  geom_point(data = filter(dat_bob, prop_ag > 99), aes(x = locx, y = locy), color = "red", size = 1)+
  labs(x='', y='', alpha = "proportion\nagriculture")


dat2 <- subset(dat, !(LC %in% c("Sea", "Water Bodies", "Barren", "Settlement")))
ggplot(dat2, aes(x = Time, y = X)) +
geom_point(aes(color = LC), alpha = 0.8, size = 0.5) + 
geom_smooth(se = FALSE) +
scale_color_manual(values = LC_info$color) +
facet_wrap(~LC) + 
theme(legend.position = 'none')

### all years combined ###

all_years <- rbind(readRDS("Data/formatted_50by50_2003.rds"),
									readRDS("Data/formatted_50by50_2004.rds"),
									readRDS("Data/formatted_50by50_2005.rds"),
									readRDS("Data/formatted_50by50_2006.rds"),
									readRDS("Data/formatted_50by50_2007.rds"))


all_years <- rbind(readRDS("Data/formatted_50by50_2004.rds"),
                   readRDS("Data/formatted_50by50_2005.rds"),
                   readRDS("Data/formatted_50by50_2006.rds"),
                   readRDS("Data/formatted_50by50_2007.rds"))
junk <- readRDS("Data/formatted_50by50_2003.rds")

junk$prop_coastal <- NULL
junk$prop_moist_decid <- NULL
all_years <- rbind(junk, all_years)

dat2 <- subset(all_years, !(LC %in% c("Sea", "Water Bodies", "Barren", "Settlement")))
ggplot(dat2, aes(x = Time, y = X)) +
geom_point(aes(color = LC), alpha = 0.5, size = 0.1) + 
geom_smooth(se = FALSE) +
scale_color_manual(values = LC_info$color) +
facet_wrap(~LC) + 
theme(legend.position = 'none')


dat2 <- subset(all_years, !(LC %in% c("Sea", "Water Bodies", "Barren", "Settlement")))
ggplot(dat2, aes(x = Time, y = X)) +
#geom_point(aes(color = LC), alpha = 0.5, size = 0.1) + 
geom_smooth(aes(color = LC),se = FALSE) +
scale_color_manual(values = LC_info$color) +
facet_grid(LC~year, scales = 'free') + 
#ylim(c(1.5, 3)) +
theme(legend.position = 'none') 
# ggsave("Plots/allyears_mean.pdf", height = 10, width = 10)

dat2 <- all_years
#dat2 <- subset(all_years, !(LC %in% c("Sea", "Water Bodies", "Barren", "Settlement")))


ggplot(dat2, aes(x = Time, y = X)) +
  #geom_point(aes(color = LC), alpha = 0.5, size = 0.1) + 
  geom_smooth(aes(color = LC),se = FALSE) +
  scale_color_manual(values = LC_info$color) +
  facet_wrap(~year, scales = 'free') 
  #theme(legend.position = "bottom")+
  #ylim(c(1.5, 3.5))

# ggsave("Plots/mean_by_LC_facet_year.pdf", width = 10, height = 6)

####################

### 2007 Outliers ###
# outlier_2007 <- c(1172, 412, 410, 1171, 156)
# dat_hom <- subset(dat_hom, !(ID %in% outlier_2007))







