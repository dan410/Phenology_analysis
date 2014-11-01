############################################################################
### compute projection coefficients 
### Author: Daniel Fortin
### last updated: 10/26/14
############################################################################


library(plyr)
library(dplyr)
library(MASS)

# read in spatialfda functions
library(dansRfunctions)
sourceDir("/Users/fort002/Google Drive/Research/R_packages/spatialfda/R") 

source('~/Google Drive/Research/Projects/SimStudy_weighted_cov_est/R/est_intensity.R')

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

## function to project data onto set of basis functions
compute_coefs <- function(x, argvals, basis){
  nfuns <- length(unique(x$ID))
  y <- matrix(x$X, ncol = nfuns, byrow = FALSE)
  fit <- smooth.basis(argvals = argvals, y = y, fdParobj = emp.basis)$fd
  coef.fields <- as.data.frame(t(fit$coefs))
  coef.fields$ID <- unique(x$ID)
  coef.fields$LC2 <- x$LC2[1]
  return(coef.fields)
}

## 
year <- 2007
dat <- readRDS(paste("Data/formatted_50by50_", year, ".rds", sep = ""))

############################################################################
### get locations and calculate weights based on location point intensity
############################################################################

load(paste("Data/covfit_ag_wt", year, ".Rdata", sep=""))

# estimate the eigenfunctins of the covariance function
eigen.fit <- estimate_eigenfunctions(cfit)

# create a list from estimated eigenfunctions
funlist <- list(f0 <- function(x){rep(1, length(x))},
                f1 = extract_pcf(1, eigen.fit),
                f2 = extract_pcf(2, eigen.fit),
                f3 = extract_pcf(3, eigen.fit)
) 

############################################################################
### create an empirical basis object
############################################################################

emp.basis <- create.empirical.basis(basisfuns = funlist, rangeval=c(0,1))

############################################################################
### project other land cover classes onto the empirical basis
############################################################################

dat_LC_cluster <- subset(dat, LC2 %in% c("Agriculture", "Vegetation"))

coefs <- ddply(dat_LC_cluster, .(LC2), .fun = compute_coefs, argvals = unique(ag_dat$Time), basis = emp.basis)
names(coefs) <- c("const", "fpc1", "fpc2","fpc3","ID","LC2")

save(coefs, ag_dat, dat, emp.basis, file = paste("Data/projection_coefs_", year,".Rdata", sep=""))





