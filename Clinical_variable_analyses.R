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
Covariates_OCD_HC <- read_csv("Covariates_OCD_HC.csv")

# change gender from numerical to categorical 
Covariates_OCD_HC$Gender <- factor(Covariates_OCD_HC$Gender,levels=c(1,2), labels = c("male","female"))

# CHECK NORMALITY
## plot histograms 

hist.age <- ggplot(Covariates_OCD_HC, aes(Age)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=5) + 
  labs(x = "Age", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(Covariates_OCD_HC$Age, na.rm = TRUE), sd = sd(Covariates_OCD_HC$Age, na.rm = TRUE)), colour = "red", size = 1)

hist.sev <- ggplot(Covariates_OCD_HC, aes(Sev)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=5) + 
  labs(x = "YBOCS score", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(Covariates_OCD_HC$Sev, na.rm = TRUE), sd = sd(Covariates_OCD_HC$Sev, na.rm = TRUE)), colour = "red", size = 1)

hist.madrs <- ggplot(Covariates_OCD_HC, aes(MADRS)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=5) + 
  labs(x = "MADRS score", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(Covariates_OCD_HC$MADRS, na.rm = TRUE), sd = sd(Covariates_OCD_HC$MADRS, na.rm = TRUE)), colour = "red", size = 1)

hist.all <- arrangeGrob(hist.age, hist.sev, hist.madrs, ncol=3, nrow=1)
grid.draw(hist.all) 
ggsave("hist_all_covariates_OCD_HC.pdf", hist.all) 

## statistical test for normality 
stat.desc(Covariates_OCD_HC$Age, basic=F, norm=T)
stat.desc(Covariates_OCD_HC$Sev, basic=F, norm=T)
stat.desc(Covariates_OCD_HC$MADRS, basic=F, norm=T)

# Levene test for equal variances
leveneTest(Covariates_OCD_HC$Age, Covariates_OCD_HC$Group,center=mean)
leveneTest(Covariates_OCD_HC$Sev, Covariates_OCD_HC$Group, center=mean)
leveneTest(Covariates_OCD_HC$MADRS,Covariates_OCD_HC$Group, center=mean)

# T-test for Age
tt.age <-t.test(Age~Group, data=Covariates_OCD_HC, alternative="two.sided", var.equal=T, paired=F)

tt.age


# Non-parametric Wilcoxon rank sum test/Mann Whitney for MADRS score
# assign ranks to madrs scores across the two groups
rank.madrs <- rank(Covariates_OCD_HC$MADRS, ties="average")


# add to the data-set
Covariates_OCD_HC <- cbind(Covariates_OCD_HC, rank.madrs) 
#check
head(Covariates_OCD_HC)

# get descriptive statistics of the rank scores for the two groups separately
by(Covariates_OCD_HC$rank.madrs, Covariates_OCD_HC$Group, stat.desc)

# Wilcoxon rank sum test 
wilcox.madrs <- wilcox.test(MADRS ~ Group, data=Covariates_OCD_HC, alternative = "two.sided", exact=F, correct=T, paired=F)

wilcox.madrs

# effect size 
table(Covariates_OCD_HC$Group)

# highest sum of ranks = 2192, group 1 = 41, group 0 = 34

Ws <- 2182
N1 <- 41
N0 <- 34
Wsmean <- (N1*(N1+N0+1))/2
SEws <- sqrt((N1*N0*(N1+N0+1))/12)
Z <- (Ws-Wsmean)/SEws
r <- Z/sqrt(N1+N0)
r

# Chi-squared test for gender 
table(Covariates_OCD_HC$Gender, Covariates_OCD_HC$Group)

prop.table(table(Covariates_OCD_HC$Gender, Covariates_OCD_HC$Group), margin = 1)

chisq.test(table(Covariates_OCD_HC$Gender, Covariates_OCD_HC$Group))

