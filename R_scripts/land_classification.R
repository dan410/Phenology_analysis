############################################################################
### Land cover classification analysis for agricultural and non-agricuture 
###
### Author: Daniel Fortin
### last updated: 10/3/14
############################################################################

library(plyr)
library(dplyr)
library(ggplot2)
library(sseigfun)
library(MASS)
library(mda)

# read in spatialfda functions
library(dansRfunctions)
sourceDir("/Users/fort002/Google Drive/Research/R_packages/spatialfda/R") 

source('~/Google Drive/Research/Projects/SimStudy_weighted_cov_est/R/est_intensity.R')

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

year <- 2003
dat <- readRDS(paste("Data/formatted_50by50_", year, ".rds", sep = ""))
LC_info <- read.csv(file.path("Data_Raw", "LandCover", "LANDCOVER_CLASSES_names_50by50.csv"), header=TRUE, stringsAsFactors = FALSE)
############################################################################
### plot land cover map colored by agriculture and tropical vegetation
############################################################################

## pull off one row for each curve to look at land cover info.
## Each row of the data frame contains info for a specific grid cell.
dat_LC_only <- dat[!duplicated(dat$ID), ] 

# ggplot(dat_LC_only, aes(x = locx, y = locy))+
#   geom_tile(aes(fill = LC2))+
#   labs(x = "", y = "")
# # ggsave("Plots/ag_veg_land_cover.pdf", width = 8, height = 5)

############################################################################
### plot homogeneous agriculture gric cells
############################################################################

### subset with homogeneous cells for agriculture
dat_hom_LC <- filter(dat_LC_only,  prop_ag >99)

# ggplot(dat_LC_only, aes(x = locx, y = locy) ) +
#   geom_tile(aes(fill = LC2), alpha = 0.5) +
#   geom_point(data = dat_hom_LC, aes(x = locx, y = locy),color = "red", size = 2, shape = 22)+
# 	labs(x = "", y = "", fill = "land cover")
#
# # ggsave("Plots/ag_hom_cells.pdf", width = 8, height = 5)

##################################################################
### subset MTCI data to extract homogeneous agriculture grid cells 
##################################################################
dat_hom <- subset(dat, prop_ag > 99)

# ### plot curves
# ggplot(dat_hom, aes(x = Time, y = X, group = ID))+
#   geom_line()+
#   facet_wrap(~year) +
#   ggtitle("Homogeneous Agriculture cells")
# # ggsave("Plots/hom_curves_agriculture.pdf", height = 5, width = 8)
#
# ### split east west
# df <- mutate(dat_hom, West = locx < 20)
# ggplot(df, aes(x = Time, y = X, group = ID))+
#   geom_line(aes(color = West))+
#   facet_wrap(~year) +
#   ggtitle("")+
# 	labs(x = 'time', y = 'X(t)') +
# 	theme(legend.position = "none")
# # ggsave("Plots/hom_curves_agriculture_east_west_2003.pdf", height = 5, width = 8)
#
# ### Look for outliers
# ggplot(dat_hom, aes(x = Time, y = X, group = ID, label = as.character(ID))) +
#   geom_text(size = 2)+
#   facet_wrap(~year) +
#   theme(legend.position = 'none')

############################################################################
### extract locations
############################################################################
ag_dat <- filter(dat,  prop_ag >99 & locx > 20)

############################################################################
### get locations and calculate weights based on location point intensity
############################################################################

ag_info <- ag_dat[!duplicated(ag_dat$ID), ]
ag_info <- est_intensity(ag_info, 5)

## merge location intensity back with the MTCI data
ag_dat <- merge(ag_dat, ag_info[,c("ID", "intensity")], by = "ID")

## calculate a weight column based on the intensity values
ag_dat <- mutate(ag_dat, wt = (1/intensity)^(1/1))

# ## plot grid points
# ggplot(ag_dat, aes(x = locx, y = locy, size = wt))+
# geom_point()+
# xlim(c(0,50)) + ylim(c(0,50))+
# scale_size(range = c(1,4))+
# theme_bw()+
# labs(x = '', y = '', size = "weight")
#
# # ggsave(file.path("Plots", "india_wt_locs.pdf"), width = 6, height = 5)

### shift curves to the orgin
mean_X <- mean(ag_dat$X)
ag_dat$X <- ag_dat$X - mean_X

### scale time axis to [0,1]
timepts <- ag_dat$Time[ag_dat$ID == unique(ag_dat$ID)[1]]
time <- (timepts-min(timepts))/(max(timepts) - min(timepts))
ag_dat$Time <- rep(time, length(unique(ag_dat$ID)))

############################################################################
### estimate FPCs
############################################################################

# arrange data to use with fda package
nfuns <- length(unique(ag_dat$ID))
y <- matrix(ag_dat$X, ncol = nfuns, byrow = FALSE)
argvals <- unique(ag_dat$Time)

# extract the locations
locs <- ddply(ag_dat, .(ID), function(x){x[1, c("locx", "locy")]})
locs$ID <- NULL # drop the ID column

# estimate the covariance function
# n.marginal.knots = 7
# cfit <- estimate_cov_function(ag_dat, n.marginal.knots = n.marginal.knots)
# save(cfit, ag_dat, n.marginal.knots, file = paste("Data/covfit_ag_", year, ".Rdata", sep = ""))
 load(paste("Data/covfit_ag_wt", year, ".Rdata", sep=""))

# estimate the eigenfunctins of the covariance function
eigen.fit <- estimate_eigenfunctions(cfit)

# create a list from estimated eigenfunctions
funlist <- list(f0 <- function(x){rep(1, length(x))},
                f1 = extract_pcf(1, eigen.fit),
                f2 = extract_pcf(2, eigen.fit),
                f3 = extract_pcf(3, eigen.fit)
) 


# ### plot PCFs
# xs <- seq(0,1, length = 1000)
# PCF1 <- data.frame(Time = xs, ys = funlist$f1(xs), PCF = 1)
# PCF2 <- data.frame(Time = xs, ys = funlist$f2(xs), PCF = 2)
# PCF3 <- data.frame(Time = xs, ys = funlist$f3(xs), PCF = 3)
# PCF <- rbind(PCF1, PCF2, PCF3)
# PCF$PCF <- as.factor(PCF$PCF)

############################################################################
### plot PCFs for all years
############################################################################
# PCF <- readRDS("Data/pcf_all_years.rds")
# ggplot(PCF, aes(x = Time, y = ys, group = PCF)) +
# geom_line(aes(color = PCF, linetype = PCF))+
# facet_wrap(~year, ncol = 2)+
# labs(x = "time", y = "")
#
# # ggsave(file.path("Plots", "PCF_all_years.pdf"), width = 5, height = 6)
############################################################################
### create an empirical basis object
############################################################################

emp.basis <- create.empirical.basis(basisfuns = funlist, rangeval=c(0,1))
# plot(emp.basis)

# fit.emp <- smooth.basis(argvals = argvals, y = y, fdParobj = emp.basis)$fd
# plot(fit.emp)

############################################################################
### project other land cover classes onto the empirical basis
############################################################################

dat_LC_cluster <- subset(dat, LC2 %in% c("Agriculture",
                                         "Vegetation"))

## project data onto set of basis functions
compute_coefs <- function(x, argvals, basis){
  nfuns <- length(unique(x$ID))
  y <- matrix(x$X, ncol = nfuns, byrow = FALSE)
  fit <- smooth.basis(argvals = argvals, y = y, fdParobj = emp.basis)$fd
  coef.fields <- as.data.frame(t(fit$coefs))
  coef.fields$ID <- unique(x$ID)
  coef.fields$LC2 <- x$LC2[1]
  return(coef.fields)
}

coefs <- ddply(dat_LC_cluster, .(LC2), .fun = compute_coefs, argvals = argvals, basis = emp.basis)

############################################################################
### plot relationship between fitted coefficients
############################################################################

# ggplot(coefs, aes(x = V1, y = V2)) +
#   geom_point(aes(color = LC2), size =1)
#
# ggplot(coefs, aes(x = V2, y = V3)) +
#   geom_point(aes(color = LC2))

############################################################################
### Discriminant Analysis
############################################################################

## pull off IDs of homogeneous cell to use as a merging variable
ag_hom_ID <- unique(ag_dat$ID)

## get homogeneous vegetation locations
veg_dat <- filter(dat, prop_tEvergreen + prop_tSemievergreen + prop_moist_decid > 99 & locx < 20)
veg_dat_ID <- unique(veg_dat$ID)
#veg_ID <- sample(veg_dat_ID, 80)
veg_ID <- readRDS("Data/veg_training_ID.rds")

# ### plot training data locations
# ggplot(dat_LC_only, aes(x = locx, y = locy) ) +
#   geom_tile(aes(fill = LC2), alpha = 0.5) +
#   geom_point(data = dat_hom_LC, aes(x = locx, y = locy),color = "red", size = 2, shape = 22)+
# 	geom_point(data = subset(dat_LC_only, ID %in% veg_ID), aes(x = locx, y = locy),color = "blue", size = 2, shape = 22)+
# 	labs(x = "", y = "", fill = "land cover")

## create training data set and testing data set
training <- subset(coefs, ID %in% ag_hom_ID | ID %in% veg_ID)
testing <- subset(coefs, !(ID %in% ag_hom_ID | ID %in% veg_ID) )
testing <- coefs

## fit using linear discriminant analysis
fit <- lda(LC2 ~ V1 + V2 + V3 + V4, data=training)

## classify the testing data using the fitted model
pred <- predict(fit, newdata=testing)
testing$pred <- pred$class

pred_lc <- merge(testing, dat_LC_cluster, by='ID')

pred_lc_2003$year = 2003
pred_lc_2004$year = 2004
pred_lc_2005$year = 2005
pred_lc_2006$year = 2006
pred_lc_2007$year = 2007


# pred_lc <- rbind(pred_lc_2003, pred_lc_2004, pred_lc_2005, pred_lc_2006, pred_lc_2007)
# saveRDS(pred_lc, "Data/landcover_pred_wt.rds")
pred_lc <- readRDS("Data/landcover_pred_wt.rds")
pred_lc <- readRDS("Data/landcover_pred.rds")

ggplot(pred_lc, aes(x = locx, y = locy))+
  geom_tile(aes(fill = LC2.x))+
  geom_point(aes(color = pred), size = 0.1)+
	facet_wrap(~year, ncol=2)+
	labs(color = "predicted\nlandcover", fill = "GLC2000\nlandcover")

# ggsave(paste("Plots/land_classification_", year, ".pdf", sep = ""), width = 8, height = 5)

ggplot(pred_lc, aes(x = locx, y = locy))+
  geom_tile(aes(fill = LC2.x))+
	#geom_tile(data = dat_LC_only, aes(x = locx, y = locy, fill = LC))+
  geom_tile(data = subset(pred_lc, LC2.x == "Vegetation" & pred == "Agriculture"), aes(x = locx, y = locy), fill = 'red')+
	geom_tile(data = subset(pred_lc, LC2.x == "Agriculture" & pred == "Vegetation"), aes(x = locx, y = locy), fill = 'green')+
	facet_wrap(~year, ncol=2)+
	labs(x='',y='', color = "predicted\nlandcover", fill = "GLC2000\nlandcover")

#	ggsave(file.path("Plots/landcover_misclass_wt.pdf"), width = 8, height = 10)
#	ggsave(file.path("Plots/landcover_misclass.pdf"), width = 8, height = 10)

	ggplot(pred_lc, aes(x = locx, y = locy))+
	  geom_tile(aes(fill = LC2.x))+
	  geom_point(data = subset(pred_lc, LC2.x == "Agriculture" & pred == "Vegetation"), aes(x = locx, y = locy))+
		facet_wrap(~year, ncol=2)+
		labs(color = "predicted\nlandcover", fill = "GLC2000\nlandcover")


ggplot(dat_LC_only, aes(x = locx, y = locy))+
  geom_tile(aes(fill = LC))+
  scale_fill_manual(values = LC_info$color )+
  geom_point(data = junk, aes(color = pred), size = 1)

ggsave(paste("Plots/land_classification_all_classes_", year, ".pdf", sep = ""), width = 8, height = 5)



### use mda package for classification
fit <- fda(LC2 ~ ., data = training)

confusion(fit, training)







