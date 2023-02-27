# load packages

library(ggplot2)
library(car)
library(pastecs)
library(psych)
library(grid)
library(gridExtra)
library(DescTools)
library(Hmisc)
library(multcomp)


# set working directory
setwd("C:\\Datin\\Vrije Universiteit\\Master's\\Year 1\\Internship\\Statistics")

# read in file
library(readr)
Covariates_OCD <- read_csv("Covariates_OCD.csv")

# run normality tests
stat.desc(Covariates_OCD$Sev, basic=F, norm=T)
stat.desc(Covariates_OCD$MADRS, basic=F, norm=T)

# make histogram
hist.sev <- ggplot(Covariates_OCD, aes(Sev)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "YBOCS score", y = "Frequency")  + theme_classic() +   
  stat_function(fun = dnorm, args = list(mean = mean(Covariates_OCD$Sev, na.rm = TRUE), sd = sd(Covariates_OCD$Sev, na.rm = TRUE)), colour = "cornflowerblue", size = 1)

hist.madrs <- ggplot(Covariates_OCD, aes(MADRS)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "MADRS score", y = "Frequency")  + theme_classic() +      
  stat_function(fun = dnorm, args = list(mean = mean(Covariates_OCD$MADRS, na.rm = TRUE), sd = sd(Covariates_OCD$MADRS, na.rm = TRUE)), colour = "chocolate1", size = 1)


hist.all <- arrangeGrob(hist.sev, hist.madrs, ncol=2, nrow=1)

grid.draw(hist.all) # interactive device

ggsave("hist_sev_madrs_OCD.pdf", hist.all) # save image file

# simple regression 
regr1 <- lm(Sev ~ MADRS, data=Covariates_OCD, na.action=na.exclude)
summary(regr1)

