### create grid of agriculture locations for weighted covariance estimation simulation study

# compute PCFs 

library(plyr)
library(dplyr)
library(ggplot2)
library(sseigfun)


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
### plot land cover map 
############################################################################
dat_LC_only <- dat[!duplicated(dat$ID), ] # pull off one row for each curve to look at land cover info

ggplot(dat_LC_only, aes(x = locx, y = locy, label = ID))+
  geom_tile(aes(fill = LC))+
  scale_fill_manual(values = LC_info$color ) +
  labs(x = "", y = "", fill = "land cover")

# ggsave("Plots/land_cover.pdf", width = 8, height = 5)

############################################################################
### plot land cover by vegetation and agriculture
############################################################################

ggplot(dat_LC_only, aes(x = locx, y = locy))+
  geom_tile(aes(fill = LC2))+
  #  scale_fill_manual(values = c("orange", "green", "blue") ) +
  labs(x = "", y = "", fill = 'land cover')
	
# ggsave("Plots/land_cover_ag_vs_veg.pdf", width = 8, height = 5)

############################################################################
### plot proportion of agriculture over all grid cells
############################################################################

ggplot(dat_LC_only, aes(x = locx, y = locy)) + 
  geom_tile(aes( alpha = prop_ag/100))+
  theme_bw()+
  labs(x = "", y = "", alpha = "proportion\nagriculture" )
	
# ggsave("Plots/proportion_agriculture.pdf", width = 8, height = 5)

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
ag_dat <- mutate(ag_dat, wt = (1/intensity)^0.5)

############################################################################
### Center curves at the x-axis and re-scale to [0,1]
############################################################################

### shift curves to the orgin
mean_X <- mean(ag_dat$X)
ag_dat$X <- ag_dat$X - mean_X

### scale time axis to [0,1]
timepts <- dat$Time[dat$ID == 1]
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
n.marginal.knots = 5
cfit <- estimate_cov_function(ag_dat, n.marginal.knots = n.marginal.knots)
# save(cfit, ag_dat, n.marginal.knots, file = "Data/covfit_ag_2003_wt.Rdata")
# load("Data/covfit_ag_2003_wt.Rdata")

# estimate the eigenfunctins of the covariance function
eigen.fit <- estimate_eigenfunctions(cfit)

# look at the eigenvalues and scree plot
eig_vals <- eigen.fit$values
eig_vals <- eig_vals[eig_vals > 0]
sum_vals <- sum(eig_vals)
n_vals <- length(eig_vals)
ratio_vals <- eig_vals/sum_vals
plot(1:n_vals, cumsum(ratio_vals), ylim = c(0,1))

# create a list from estimated eigenfunctions
funlist <- list(f0 <- function(x){rep(1, length(x))},
                f1 = extract_pcf(1, eigen.fit),
                f2 = extract_pcf(2, eigen.fit),
                f3 = extract_pcf(3, eigen.fit)
) 


### plot PCFs
xs <- seq(0,1, length = 1000)
PCF1 <- data.frame(Time = xs, ys = funlist$f1(xs), PCF = 1)
PCF2 <- data.frame(Time = xs, ys = funlist$f2(xs), PCF = 2)
PCF3 <- data.frame(Time = xs, ys = funlist$f3(xs), PCF = 3)
PCF <- rbind(PCF1, PCF2, PCF3)
PCF$PCF <- as.factor(PCF$PCF)

ggplot(PCF, aes(x = Time, y = ys, group = PCF)) + geom_line(aes(color = PCF, linetype = PCF))



