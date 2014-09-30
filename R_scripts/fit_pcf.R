# compute PCFs 

library(plyr)
library(dplyr)
library(ggplot2)
library(sseigfun)

# read in spatialfda functions
library(dansRfunctions)
sourceDir("/Users/fort002/Google Drive/Research/R_packages/spatialfda/R") 

# set working directory
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

dat <- readRDS("Data/formatted_50by50_2003.rds")
LC_info <- read.csv(file.path("Data_Raw", "LandCover", "LANDCOVER_CLASSES_names_50by50.csv"), header=TRUE, stringsAsFactors = FALSE)
############################################################################
### plot land cover map 
############################################################################
dat_LC_only <- dat[!duplicated(dat$ID), ] # pull off one row for each curve to look at land cover info

ggplot(dat_LC_only, aes(x = locx, y = locy))+
  geom_tile(aes(fill = LC))+
  scale_fill_manual(values = LC_info$color )

############################################################################
### plot homogeneous agriculture gric cells
############################################################################

### subset with homogeneous cells for tropical evergreen and tropical semievergreen
dat_hom_LC <- filter(dat_LC_only,  prop_ag >99)

ggplot(dat_LC_only, aes(x = locx, y = locy) ) + 
geom_tile(aes(fill = LC), alpha = 0.3) + 
scale_fill_manual(values = LC_info$color ) +
geom_point(data = dat_hom_LC, aes(x = locx, y = locy, color = LC), size = 4, shape = 22)

##################################################################
### subset data for homogeneous agriculture grid cell
##################################################################
dat_hom <- subset(dat, prop_ag > 99)

### plot curves
ggplot(dat_hom, aes(x = Time, y = X, group = ID))+
geom_line()+
facet_wrap(~year) +
ggtitle("Homogeneous Agriculture cells")
#ggsave("Plots/hom_curves_agriculture.pdf", height = 5, width = 8)

### Look for outliers
ggplot(dat_hom, aes(x = Time, y = X, group = ID, label = as.character(ID))) + 
geom_text(size = 2)+
facet_wrap(~year) + 
theme(legend.position = 'none')

############################################################################
### Center curves at the x-axis and re-scale to [0,1]
############################################################################

### shift curves to the orgin
mean_X <- mean(dat_hom$X)
dat_hom$X <- dat_hom$X - mean_X

### scale time axis to [0,1]
timepts <- dat$Time[dat$ID == 1]
time <- (timepts-min(timepts))/(max(timepts) - min(timepts))
dat_hom$Time <- rep(time, length(unique(dat_hom$ID)))

############################################################################
### estimate FPCs
############################################################################

# arrange data to use with fda package
nfuns <- length(unique(dat_hom$ID))
y <- matrix(dat_hom$X, ncol = nfuns, byrow = FALSE)
argvals <- unique(dat_hom$Time)

# extract the locations
locs <- ddply(dat_hom, .(ID), function(x){x[1, c("locx", "locy")]})
locs$ID <- NULL # drop the ID column

# estimate the covariance function
n.marginal.knots = 7
cfit <- estimate_cov_function(dat_hom, n.marginal.knots = n.marginal.knots)
save(cfit, dat_hom, n.marginal.knots, file = "Data/covfit_ag_2003.Rdata")

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

############################################################################
### create an empirical basis object
############################################################################

emp.basis <- create.empirical.basis(basisfuns = funlist, rangeval=c(0,1))
plot(emp.basis)

fit.emp <- smooth.basis(argvals = argvals, y = y, fdParobj = emp.basis)$fd
plot(fit.emp, ylim=c(-2,2))

############################################################################
### project other land cover classes onto the empirical basis
############################################################################
dat_LC_cluster <- subset(dat, LC %in% c("Tropical Evergreen", 
																				"Tropical Moist Deciduous", 
																				"Tropical Semievergreen",
																				"Coastal vegetation",
																				"Irrigated Agriculture"))

dat_LC_cluster <- subset(dat, LC %in% c("Coastal vegetation",
                                        "Irrigated Agriculture"))
																				
compute_coefs <- function(x, argvals, basis){
	nfuns <- length(unique(x$ID))
	y <- matrix(x$X, ncol = nfuns, byrow = FALSE)
	fit <- smooth.basis(argvals = argvals, y = y, fdParobj = emp.basis)$fd
	coef.fields <- as.data.frame(t(fit$coefs))
  coef.fields$ID <- unique(x$ID)
	coef.fields$LC <- x$LC[1]
	return(coef.fields)
}

coefs <- ddply(dat_LC_cluster, .(LC), .fun = compute_coefs, argvals = argvals, basis = emp.basis)

# create scatter plot of coefficients to identify clusters
ggplot(subset(coefs, LC == "Coastal vegetation"), aes(x = V1, y = V2, label = ID)) + 
  geom_point(aes(color = LC), size = 0.5) +
  geom_text()

############################################################################
### plot possible misclassified points
############################################################################

ggplot(dat_LC_only, aes(x = locx, y = locy)) +
  geom_tile(aes(fill = LC))+
  scale_fill_manual(values = LC_info$color )+
  geom_point(data = subset(dat_LC_only, ID %in% c(68, 27, 766, 1139, 1369, 1034, 1372, 1395, 1193, 1143)), 
             aes(x = locx, y = locy), color = "red", shape = 7, size = 5)+
  
  geom_text(data = subset(dat_LC_only, ID %in% c(68, 27, 766, 1139, 1369, 1034, 1372, 1395, 1193, 1143)), 
           aes(x = locx, y = locy, group = ID, label = as.character(ID)), size = 5)

 
ggplot(subset(dat, LC == 'Coastal vegetation'), aes(x = Time, y = X, group = ID))+
  geom_line()+
  geom_line(data = subset(dat, ID %in% c(68, 27, 766, 1139, 1369, 1034, 1372, 1395, 1193, 1143)), aes(x = Time, y = X), color = "red")

############################################################################
### variogram analysis
############################################################################


# empirical basis
dat.geo.emp <- list()
for(i in 1:emp.basis$nbasis){
	dat.geo.emp[[i]] <- as.geodata(cbind(locs, coef.fields.emp), coords.col=1:2, data.col=(2 + i))
}

### fit variograms to each of the coefficient fields.
variograms.emp <- llply(dat.geo.emp, geoR:::variog, estimator.type="modulus", max.dist = 0.6, messages=FALSE)
variograms.fit.emp <- llply(variograms.emp, geoR:::variofit, ini.cov.pars = c(.04,0.2), cov.model="exponential", fix.nugget=FALSE, nugget = 0.0)

ids <- sample(1:nlocs, 2)
ids %in% samp.id
pred.locs <- mycurves$LOCS[ids ,]
preds.emp <- list()
for( i in 1:emp.basis$nbasis){
	preds.emp[[i]] <- krige.conv(dat.geo.emp[[i]], locations = pred.locs, krige = krige.control(obj.model = variograms.fit.emp[[i]]))
}

coef.emp <- ldply(preds.emp, function(x){x$predict})

emp.obj <- fd(coef=coef.emp, emp.basis)
plot_curves(coef=mycurves$COEF[ids,], basis.fns=mycurves$basis.fns, ylim=c(-1,1)); abline(h=0, lty=2)
plot(emp.obj, ylim=c(-1,1), add=TRUE, col="blue")
plot_curves(coef=mycurves$COEF[ids,], basis.fns=mycurves$basis.fns, ylim=c(-1,1)); abline(h=0, lty=2)
plot(b.obj, ylim=c(-1,1), add=TRUE, col="blue")
par(op)

plot(locs.obs)
points(mycurves$LOCS[ids,], col="red", pch=16)


junk <- fd(coef=t(j), basisobj=emp.basis)
plot(junk, ylim=c(-1,1))




