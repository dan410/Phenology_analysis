############################################################################
### Classify land cover using FPC coefficients
###
### Author: Daniel Fortin
### Last update: 10/26/14
############################################################################
library(ggplot2)
library(plyr)
library(mda)
library(klaR)
setwd("~/Google Drive/Research/Projects/Phenology_analysis")

year <- 2007
fitted_coefs <- load(paste("Data/projection_coefs_",year,".Rdata", sep=''))

############################################################################
### plot coefs
############################################################################
df <- melt(coefs)
df <- subset(df, variable != 'ID')
qplot(data = df, x = variable, y = value, geom= 'boxplot')
ggplot(data = df, aes(x = variable, y = value, color = LC2, shape = LC2))+
geom_jitter(size = 0.7)

df <- melt(coefs)
df <- subset(df, variable != 'ID')
ggplot(data = df, aes(x = value, color = LC2, fill = LC2))+
geom_histogram()+
facet_wrap(~variable, scales = 'free')

############################################################################
### plot 2-d partitions of classification methods
############################################################################
lapply(2003:2007, FUN = function(year){
	pdf(paste("Plots/partimat_", year, ".pdf", sep=''), width = 8, height  =11)
	ag_hom_ID <- readRDS("Data/ag_hom_ID.rds")
	veg_ID <- readRDS("Data/veg_training_ID.rds")
	load(paste("Data/projection_coefs_",year,".Rdata", sep=''))
	training <- subset(coefs, ID %in% ag_hom_ID | ID %in% veg_ID)
	partimat(LC2 ~ -1 + const + fpc1 + fpc2 + fpc3, data = training, method = 'lda', main=year, nplots.hor =2)
	dev.off()
	})


############################################################################
### classification using logistic regression
############################################################################
## pull off IDs of homogeneous cell to use as a merging variable
ag_hom_ID <- readRDS("Data/ag_hom_ID.rds")
veg_ID <- readRDS("Data/veg_training_ID.rds")

est_land_cover <- function(year){
	
	load(paste("Data/projection_coefs_",year,".Rdata", sep=''))
	## create training data set and testing data set
	training <- subset(coefs, ID %in% ag_hom_ID | ID %in% veg_ID)
	testing <- coefs

	### LDA
  lda_fit <- fda(LC2 ~ -1 + const + fpc1 + fpc2 + fpc3, data = training)
	lda_pred <- predict(lda_fit, newdata = testing[, 1:4])
	lda_probs <- predict(lda_fit, newdata = testing[,1:4], type = 'posterior')
	
	### Logistic Regression
	logistic_fit <- glm(LC2 ~ -1 + const + fpc1 + fpc2 + fpc3, family = binomial(logit), data = training)
	logistic_probs <- predict(logistic_fit, newdata = testing[, 1:4], type = 'response')
	logistic_pred <- rep("Agriculture", length = length(logistic_probs))
	logistic_pred[logistic_pred >=0.5] <- "Vegetation"

	testing$pred_logistic <- logistic_pred
	testing$logistic_probs <- logistic_probs
	testing$lda_probs <- lda_probs[,1] # agriculture probs
	testing$pred_lda <- lda_pred

	pred_lc <- merge(dat, testing, by = "ID")
	pred_lc$year <- year

	return(pred_lc)
}

pred_lc <- ldply(2003:2007, .fun = est_land_cover)

pred_lc$pred_cut <- cut(pred_lc$lda_probs, breaks = c(0,0.1,.3,0.5,0.7,0.9,1), include.lowest = TRUE)

loc_info <- ddply(pred_lc, .(year), .fun = function(x){
	subset(x, duplicated(x$ID)==FALSE)
	})

############################################################################
### plot lanc cover predictions for LDA
############################################################################
ggplot(pred_lc, aes(x = locx, y = locy, fill = pred_lda))+ 
geom_tile()+
scale_fill_manual( values = c('orange3','chartreuse4'))+
facet_wrap(~year, ncol = 2)+
labs(x = '', y = '', fill = "Land Cover")

############################################################################
### plot lanc cover predictions for probabilities
############################################################################
ggplot(pred_lc, aes(x = locx, y = locy, fill = pred_cut))+ 
geom_tile()+
scale_fill_manual( values = c('orange3', 'orange2', 'orange1','chartreuse2','chartreuse3','chartreuse4'))+
facet_wrap(~year, ncol = 2)+
labs(x = '', y = '', fill = "Land Cover\n Probability")

############################################################################
### plot lanc cover predictions for probabilities
############################################################################
ggplot(loc_info, aes(x = locx, y = locy))+ 
geom_tile(aes(alpha = lda_probs))+
facet_wrap(~year, ncol = 2)+
theme_bw()+
scale_alpha_continuous(range = c(0,1), breaks = c(0,.25, .5 , .75, 1))+
labs(x = '', y = '', alpha = "probability\nAgriculture")

ggsave("Plots/prob_ag.pdf", width = 8, height = 11)
############################################################################
### plot lanc cover ag proportions
############################################################################
ggplot(subset(loc_info, year==2003), aes(x = locx, y = locy))+ 
geom_tile(aes(alpha = prop_ag/100))+
theme_bw()+
scale_alpha_continuous(range = c(0,1), breaks = c(.25, .5 , .75))+
labs(x = '', y = '', alpha = "proportion\nagriculture")

ggsave("Plots/proportion_agriculture.pdf", width = 5, height = 4)



ggplot(pred_lc, aes(x = locx, y = locy))+
  geom_tile(aes(fill = LC2.x))+
  geom_tile(data = subset(pred_lc, LC2.x == "Vegetation" & pred == "Agriculture"), aes(x = locx, y = locy), fill = 'red')+
	geom_tile(data = subset(pred_lc, LC2.x == "Agriculture" & pred == "Vegetation"), aes(x = locx, y = locy), fill = 'green')+
	labs(x='',y='', color = "predicted\nlandcover", fill = "GLC2000\nlandcover")
	
ggplot(pred_lc, aes(x = locx, y = locy))+
geom_tile(aes(fill = LC2.x))+
geom_tile(data = subset(pred_lc, LC2.x == "Vegetation" & pred_logistic == "Agriculture"), aes(x = locx, y = locy), fill = 'red')+
geom_tile(data = subset(pred_lc, LC2.x == "Agriculture" & pred_logistic == "Vegetation"), aes(x = locx, y = locy), fill = 'green')+
labs(x='',y='', color = "predicted\nlandcover", fill = "GLC2000\nlandcover")

ggplot(pred_lc, aes(x = locx, y = locy))+
  geom_tile(aes(fill = pred_cut))+
	labs(x='',y='')



