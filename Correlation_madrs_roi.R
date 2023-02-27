# install packages
install.packages("ggplot2", dependencies=TRUE)
install.packages("car", dependencies=TRUE)
install.packages("pastecs", dependencies=TRUE)
install.packages("psych", dependencies=TRUE)
install.packages("gridExtra", dependencies=TRUE)
install.packages("DescTools", dependencies=TRUE)
install.packages("Hmisc", dependencies=TRUE)
install.packages("multcomp", dependencies=TRUE)

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
Contrast_estimates_OCD <- read_csv("Contrast_estimates_OCD.csv")
View(Contrast_estimates_OCD)

# correlation between madrs and ROIs
cor.test(Contrast_estimates_OCD$MADRS, Contrast_estimates_OCD$Amygdala, alternative="two.sided", method="pearson", conf.level=.95)

cor.test(Contrast_estimates_OCD$MADRS, Contrast_estimates_OCD$vlPFC, alternative="two.sided", method="pearson", conf.level=.95)

cor.test(Contrast_estimates_OCD$MADRS, Contrast_estimates_OCD$vmPFC, alternative="two.sided", method="pearson", conf.level=.95)

cor.test(Contrast_estimates_OCD$MADRS, Contrast_estimates_OCD$dlPFC, alternative="two.sided", method="pearson", conf.level=.95)

cor.test(Contrast_estimates_OCD$MADRS, Contrast_estimates_OCD$dmPFC, alternative="two.sided", method="pearson", conf.level=.95)

cor.test(Contrast_estimates_OCD$MADRS, Contrast_estimates_OCD$`Occipital cortex`, alternative="two.sided", method="pearson", conf.level=.95)

# scatterplot 

scat.madrs.amyg <- ggplot(Contrast_estimates_OCD, aes(MADRS, Amygdala)) +
  ggtitle("Amygdala") +
  geom_point() + labs(x="MADRS score", y="Mean beta weight") +
  geom_smooth(method="lm", colour="blue3") +theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title=element_text(face="bold"))

scat.madrs.vlPFC <- ggplot(Contrast_estimates_OCD, aes(MADRS, vlPFC)) +
  ggtitle("vlPFC") +
  geom_point() + labs(x="MADRS score", y="") +
  geom_smooth(method="lm", colour="chartreuse1") +theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title=element_text(face="bold"))

scat.madrs.vmPFC <- ggplot(Contrast_estimates_OCD, aes(MADRS, vmPFC)) +
  ggtitle("vmPFC") +
  geom_point() + labs(x="MADRS score", y="") +
  geom_smooth(method="lm", colour="darkorchid3") +theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title=element_text(face="bold"))

scat.madrs.dlPFC <- ggplot(Contrast_estimates_OCD, aes(MADRS, dlPFC)) +
  ggtitle("dlPFC") +
  geom_point() + labs(x="MADRS score", y="Mean beta weight") +
  geom_smooth(method="lm", colour="yellow1") +theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title=element_text(face="bold"))

scat.madrs.dmPFC <- ggplot(Contrast_estimates_OCD, aes(MADRS, dmPFC)) +
  ggtitle("dmPFC") +
  geom_point() + labs(x="MADRS score", y="") +
  geom_smooth(method="lm", colour="cyan1") +theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title=element_text(face="bold"))

scat.madrs.occip <- ggplot(Contrast_estimates_OCD, aes(MADRS,`Occipital cortex`)) +
  ggtitle("Occipital Cortex") +
  geom_point() + labs(x="MADRS score", y="") +
  geom_smooth(method="lm", colour="firebrick2") +theme_classic() + theme(plot.title=element_text(hjust=0.5)) + theme(plot.title=element_text(face="bold"))


scat.all.new <-  arrangeGrob(scat.madrs.amyg, scat.madrs.vlPFC, scat.madrs.vmPFC,scat.madrs.dlPFC, scat.madrs.dmPFC, scat.madrs.occip, ncol=3, nrow=2)
grid.draw(scat.all.new) 
ggsave("scat.all.new_OCD.jpg", scat.all.new) 
