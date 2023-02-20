####################################################################################################################################
####################################################################################################################################
                                                            # POSTER #
####################################################################################################################################
####################################################################################################################################

# Clear existing workspace objects
rm(list = ls())

# Set working directory
setwd("C:\\Datin\\Vrije Universiteit\\Master's\\Year 1\\Statistics in Neuroscience\\Data portofolio")

# Install packages
install.packages("ggplot2", dependencies=TRUE)
install.packages("Hmisc", dependencies=TRUE)
install.packages("ggm", dependecies=TRUE)
install.packages("polycor",dependencies = TRUE)
install.packages("psych",dependencies = TRUE)
install.packages("gridExtra",dependencies = TRUE)
install.packages("arules",dependencies = TRUE)
install.packages("pracma",dependencies = TRUE)
install.packages("pastecs",dependencies = TRUE)
install.packages("QuantPsyc",dependencies = TRUE)
install.packages("lmtest",dependencies = TRUE)
install.packages("car",dependencies = TRUE)
install.packages("grid", dependencies=TRUE)
install.packages("DescTools", dependencies=TRUE)
install.packages("reshape2", dependecies=TRUE)
install.packages("ez",dependencies = TRUE)
install.packages("multcomp",dependencies = TRUE)
install.packages("nlme",dependencies = TRUE)
install.packages("MASS",dependencies = TRUE)
install.packages("lme4",dependencies = TRUE)
install.packages("emmeans",dependencies = TRUE)
install.packages("afex",dependencies = TRUE)
install.packages("ggbeeswarm",dependencies = TRUE)
install.packages("gmodels",dependencies = TRUE)
install.packages("reshape",dependencies = TRUE)

# Load packages
library(ggplot2)
library(Hmisc)
library(ggm)
library(polycor)
library(psych)
library(gridExtra)
library(grid)
library(arules)
library(pracma)
library(pastecs)
library(QuantPsyc)
library(lmtest)
library(car)
library(DescTools)
library(reshape2)
library(ez)
library(multcomp)
library(nlme)
library(MASS)
library(lme4)
library(emmeans)
library(afex)
library(ggbeeswarm)
library(gmodels)
library(reshape)

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

###################################################################################################

# CHECK DATA INTEGRITY

head(glucose)
tail(glucose)
dim(glucose)
names(glucose)
summary(glucose)
psych::describe(glucose)

# Define variables that are factors
glucose$exercise <- factor(glucose$exercise,levels=c(0,1,2), labels = c("none","weekly","daily"))

# Print levels 
print(levels(glucose$exercise))

#################################################################################################

# CHECK FOR OUTLIERS

## Frequency table
table(glucose$glucose_fasting)
table(glucose$glucose_meal)
table(glucose$glucose_bed)
table(glucose$bmi)
table(glucose$exercise)

## Box plot
bmi.boxplot <- boxplot(glucose$bmi, xlab="BMI (kg/m2)")
glucose_fasting.boxplot <- boxplot(glucose$glucose_fasting, xlab="Fasting glucose levels (mg/dL)")
glucose_meal.boxplot <- boxplot(glucose$glucose_meal, xlab="Postprandial glucose levels (mg/dL)") 
glucose_bed.boxplot <- boxplot(glucose$glucose_bed, xlab="Bedtime glucose levels (mg/dL}")

## Determine numeric values of outliers 
glucose_fasting.boxplot$out 
glucose_meal.boxplot$out

##################################################################################################

# TEST FOR NORMALITY

## Histogram
hist.bmi <- ggplot(glucose, aes(bmi)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "BMI (kg/m2)", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$bmi, na.rm = TRUE), sd = sd(glucose$bmi, na.rm = TRUE)), colour = "blue", size = 1)+ theme_bw()+labs(title="Histogram showing distribution of BMI")

hist.glucose_fasting <- ggplot(glucose, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Fasting glucose levels (mg/dL", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_fasting, na.rm = TRUE), sd = sd(glucose$glucose_fasting, na.rm = TRUE)), colour = "purple", size = 1) + theme_bw()+labs(title="Histogram showing distribution of fasting glucose levels")

hist.glucose_meal <- ggplot(glucose, aes(glucose_meal)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Postprandial glucose levels (mg/dL)", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_meal, na.rm = TRUE), sd = sd(glucose$glucose_meal, na.rm = TRUE)), colour = "orange", size = 1) + theme_bw()+labs(title="Histogram showing distribution of postprandial glucose levels")

hist.glucose_bed <- ggplot(glucose, aes(glucose_bed)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Bedtime glucose levels (mg/dL)", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_bed, na.rm = TRUE), sd = sd(glucose$glucose_bed, na.rm = TRUE)), colour = "green", size = 1) + theme_bw()+labs(title="Histogram showing distribution of bedtime glucose levels")


hist.all <- arrangeGrob(hist.bmi, hist.glucose_fasting, hist.glucose_meal,hist.glucose_bed, ncol=2, nrow=2)

grid.draw(hist.all) # interactive device

ggsave("hist_glucose.pdf", hist.all) # save image file

## Shapiro Wilk's test
stat.desc(glucose$bmi, basic=F, norm=T)
stat.desc(glucose$glucose_fasting, basic=F, norm=T)
stat.desc(glucose$glucose_meal, basic=F, norm=T)
stat.desc(glucose$glucose_bed, basic=F, norm=T)

####################################################################################################################################
####################################################################################################################################

# 1) SIMPLE REGRESSION ANALYSIS - BMI and fasting glucose levels (Week 2)

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Scatterplot of BMI and fasting glucose
scat.bmi_glucose_fasting <- ggplot(glucose, aes(glucose_fasting, bmi)) + geom_point() + labs(x = "Fasting glucose levels (mg/dL)", y = "BMI (kg/m2)") +
  geom_smooth(method="lm", colour="450") + theme_light()

scat.bmi_glucose_fasting

ggsave("scat.bmi_glucose_fasting.pdf", scat.bmi_glucose_fasting)

regr1 <- lm(glucose_fasting ~ bmi, data=glucose, na.action=na.exclude)
summary(regr1)

################################################################################

# CHECKING REGRESSION ASSUMPTIONS 

glucose$fitted <- regr1$fitted.values     	# predicted values
glucose$resid <- regr1$residuals           	# raw residuals 
glucose$stand.resid <- rstandard(regr1)   	# standardized residuals
head(glucose)

# Standardized residuals versus observed values bmi (heteroscedasticity)
scat.obs.resid <- ggplot(glucose, aes(bmi, stand.resid)) +
  geom_point() + labs(x="Observed BMI values", y="Standardized residuals") +
  geom_smooth(method="lm", colour="474")

# Standardized residuals versus ID (independence)
scat.resid.ID <- ggplot(glucose, aes(ID, stand.resid)) +
  geom_point() + labs(x="ID", y="Standardized residuals") +
  geom_smooth(method="lm", colour="474")

# Distribution of standardized residuals (normality)
hist.resid <- ggplot(glucose, aes(stand.resid)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Residuals", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$stand.resid, na.rm = TRUE), sd = sd(glucose$stand.resid, na.rm = TRUE)), colour = "474", size = 1) + theme_bw() + labs(title="Histogram showing distribution of standardised residuals")

# Put all the assumption plots together in one window
scat.all <- arrangeGrob(scat.obs.resid, scat.resid.ID, hist.resid, ncol=3, nrow=1)
grid.draw(scat.all) # interactive device
ggsave("scat_residuals.pdf", scat.all) # save image file

# Breusch-Pagan test for homoscedasticity
bptest(regr1)

# Durbin-Watson test for dependency/autocorrelations
dwtest(regr1, alternative="two.sided")

# Testing normality of the residuals
stat.desc(glucose$stand.resid, basic=F, norm=T)

psych::describe(glucose$stand.resid)

# Check how many standardized residuals are within the ranges of |3.3|, |2.58|, and |1.96|
aux3 <- as.vector(glucose$stand.resid)
aux3
y1 <- c(aux3[aux3< -3.3], aux3[aux3>3.3])	# > |3.3|
y2 <- c(aux3[aux3< -2.58], aux3[aux3>2.58])	# > |2.58|
y3 <- c(aux3[aux3< -1.96], aux3[aux3>1.96])	# > |1.96|
y1
y2
y3

####################################################################################################################################
####################################################################################################################################

# 2) FACTORIAL REPEATED MEASURES ANOVA (Week 4)

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Define variables that are factors
glucose$exercise <- factor(glucose$exercise,levels=c(0,1,2), labels = c("none","weekly","daily"))

# Select relevant variables
glucose3 <- glucose[,c(1,2:4,9)]

head(glucose3)

# Concert to long format
glucose3long <- melt(glucose3, id = c("ID","exercise"), measured =c("glucose_fasting","glucose_meal","glucose_bed"))

head(glucose3long)

# Sort data
glucose3long <- glucose3long[order(glucose3long$exercise),]

head(glucose3long)

names(glucose3long)[3] <- "time"
names(glucose3long)[4] <- "glucose_levels"

head(glucose3long)

print(levels(as.factor(glucose3long$exercise)))

# Plot means and SEs
line.glucose3long <- ggplot(glucose3long,aes(time,glucose_levels, colour=exercise)) +
  stat_summary(fun=mean,geom="point", size=3) + theme_light() +
  stat_summary(fun=mean, geom="line", aes(group=exercise), linetype="solid", size=1.5) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=.2) +
  labs(x="Time", y="Mean glucose levels (mg/dL)")
grid.draw(line.glucose3long)  
ggsave("linegraph_glucose3long.pdf", glucose3long)

# Descriptive statistics for each group in each condition
by(glucose3long$glucose_levels, list(glucose3long$time,glucose3long$exercise), stat.desc,basic=F)

# Normality

noneDat <-subset(glucose3long, glucose3long$exercise=="none")
weeklyDat <-subset(glucose3long, glucose3long$exercise=="weekly")
dailyDat <-subset(glucose3long, glucose3long$exercise=="daily")

by(noneDat$glucose_levels,noneDat$time,shapiro.test)
by(weeklyDat$glucose_levels,weeklyDat$time,shapiro.test)
by(dailyDat$glucose_levels,dailyDat$time,shapiro.test)

# Means per group per glucose measurement 
describeBy(noneDat$glucose_levels,list(noneDat$time), mat=TRUE,digits=2)  [,1:6]

describeBy(weeklyDat$glucose_levels,list(weeklyDat$time), mat=TRUE,digits=2)  [,1:6]

describeBy(dailyDat$glucose_levels,list(dailyDat$time), mat=TRUE,digits=2)  [,1:6]

# Levene's test
leveneTest(glucose3$glucose_fasting, as.factor(glucose3$exercise), center=mean)

# Factorial ANOVA
glucose_mod <-aov_ez("ID","glucose_levels", glucose3long, 
                     between = c("exercise"), within = c("time"), 
                     anova_table=list(correction = "GG", es = "pes"), print.formula=T, 
                     type=c("3"), check_contrasts=T, return = c("afex_aov"))

summary(glucose_mod)
anova(glucose_mod)

################################################################################

# POST HOC TEST: REPEATED CONTRAST 

# Marginal means
emm <- emmeans(glucose_mod, ~ exercise)
emm

# Repeated contrasts
contrastR1 <- c(1,-1,0)
contrastR2 <- c(0,1,-1)
contrastR3 <- c(-1,0,1)

conRepeated <- cbind(contrastR1,contrastR2,contrastR3)

# output Repeated contrasts
contrast(emm, list(conRepeated))

# Calculate effect size
rcontrast <- function(t, df) {
  r <- sqrt(t^2 / (t^2 + df))
  print(paste("r = ", r))
}

rcontrast(-67.683, 147)
rcontrast(15.108, 147)
rcontrast(59.011, 147)

####################################################################################################################################
####################################################################################################################################
                                                   # WEEKLY OVERVIEW #
####################################################################################################################################
####################################################################################################################################

# WEEK 1 - MEAN, SD, SKEW, KURTOSIS, NORMALITY, CHI-SQUARED

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# CHECK DATA INTEGRITY 

head(glucose)
tail(glucose)
dim(glucose)
names(glucose)
summary(glucose)
psych::describe(glucose)

# Define variables that are factors
glucose$alc <- factor(glucose$alc,levels=c(0,1,2), labels = c("non-drinker", "light drinker", "heavy drinker"))
glucose$sex <- factor(glucose$sex,levels=c(0,1), labels = c("female","male"))
glucose$exercise <- factor(glucose$exercise,levels=c(0,1,2), labels = c("none","weekly","daily"))

# Print levels 
print(levels(glucose$sex))
print(levels(glucose$alc))
print(levels(glucose$exercise))

################################################################################

# CHECK FOR OUTLIERS

## Frequency table
table(glucose$glucose_fasting)
table(glucose$glucose_meal)
table(glucose$glucose_bed)
table(glucose$bmi)
table(glucose$sex)
table(glucose$exercise)
table(glucose$kcal)

## Box plot
bmi.boxplot <- boxplot(glucose$bmi, xlab="BMI (kg/m2)")
kcal.boxplot <- boxplot(glucose$kcal, xlab="Average daily caloric intake (kcal)")
glucose_fasting.boxplot <- boxplot(glucose$glucose_fasting, xlab="Fasting glucose levels (mg/dL)")
glucose_meal.boxplot <- boxplot(glucose$glucose_meal, xlab="Postprandial glucose levels (mg/dL)") 
glucose_bed.boxplot <- boxplot(glucose$glucose_bed, xlab="Bedtime glucose levels (mg/dL}")


## Determine numeric values of outliers 
glucose_fasting.boxplot$out 
glucose_meal.boxplot$out
kcal.boxplot$out

####################################################################################################################################

# TEST FOR NORMALITY

## Histogram
hist.bmi <- ggplot(glucose, aes(bmi)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "BMI (kg/m2)", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$bmi, na.rm = TRUE), sd = sd(glucose$bmi, na.rm = TRUE)), colour = "blue", size = 1)+ theme_bw()+labs(title="Histogram showing distribution of BMI")

hist.kcal <- ggplot(glucose, aes(kcal)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Average daily caloric intake (kcal)", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$kcal, na.rm = TRUE), sd = sd(glucose$kcal, na.rm = TRUE)), colour = "pink", size = 1)+ theme_bw()+labs(title="Histogram showing distribution of average daily caloric intake")

hist.age <- ggplot(glucose, aes(age)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Age (years)", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$age, na.rm = TRUE), sd = sd(glucose$age, na.rm = TRUE)), colour = "435", size = 1)+ theme_bw()+labs(title="Histogram showing distribution of age")

hist.glucose_fasting <- ggplot(glucose, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Fasting glucose levels (mg/dL", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_fasting, na.rm = TRUE), sd = sd(glucose$glucose_fasting, na.rm = TRUE)), colour = "purple", size = 1) + theme_bw()+labs(title="Histogram showing distribution of fasting glucose levels")

hist.glucose_meal <- ggplot(glucose, aes(glucose_meal)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Postprandial glucose levels (mg/dL", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_meal, na.rm = TRUE), sd = sd(glucose$glucose_meal, na.rm = TRUE)), colour = "orange", size = 1) + theme_bw()+labs(title="Histogram showing distribution of postprandial glucose levels")

hist.glucose_bed <- ggplot(glucose, aes(glucose_bed)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Bedtime glucose levels (mg/dL", y = "Frequency")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_bed, na.rm = TRUE), sd = sd(glucose$glucose_bed, na.rm = TRUE)), colour = "green", size = 1) + theme_bw()+labs(title="Histogram showing distribution of bedtime glucose levels")


hist.all <- arrangeGrob(hist.bmi, hist.kcal, hist.age, hist.glucose_fasting, hist.glucose_meal,hist.glucose_bed, ncol=2, nrow=3)

grid.draw(hist.all) # interactive device

ggsave("hist_glucose.pdf", hist.all) # save image file

## Shapiro Wilk's test
stat.desc(glucose$bmi, basic=F, norm=T)
stat.desc(glucose$kcal, basic=F, norm=T)
stat.desc(glucose$glucose_fasting, basic=F, norm=T)
stat.desc(glucose$glucose_meal, basic=F, norm=T)
stat.desc(glucose$glucose_bed, basic=F, norm=T)

#####################################################################################################################################

# TESTING RELATIONSHIP BETWEEN NOMINAL VARIABLES SEX AND EXERCISE

## Chi-squared test
table(glucose$sex, glucose$exercise)
prop.table(table(glucose$sex, glucose$exercise), margin=1)
chisq.test(table(glucose$sex, glucose$exercise)) 

## Bar plot of sex and exercise
ggplot(glucose,aes(exercise)) + geom_bar(position="dodge",aes(fill=sex)) + 
  scale_fill_manual(values=c("orange","skyblue","darkgrey")) + 
  theme_minimal() + 
  ggtitle("Bar plot showing differences in exercise frequency in females and males") + xlab("Exercise") +
  ylab("Frequency")

####################################################################################################################################
####################################################################################################################################

# WEEK 2 - CORRELATION AND REGRESSION 

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# CORRELATION - BMI and kcal
scat.kcal.bmi <- ggplot(glucose, aes(kcal,bmi)) +
  ggtitle("Scatterplot showing the relationship between BMI and average daily caloric intake") +
  geom_point(shape=20, size=3) + labs(x="Average daily caloric intake (kcal)", y="BMI (kg/m2)") +
  geom_smooth(method="lm", colour="35")

scat.kcal.bmi

ggsave("scat_kcal.bmi.pdf", scat.kcal.bmi)

cor.test(glucose$kcal, glucose$bmi, alternative="two.sided", method="spearman", conf.level=.95)

################################################################################

# REGRESSION - BMI and postprandial glucose levels 

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Scatterplot of BMI and postprandial glucose levels 
scat.bmi_glucose_meal <- ggplot(glucose, aes(glucose_meal, bmi)) + geom_point() + labs(x = "Postprandial glucose levels (mg/dL)", y = "BMI (kg/m2)", title = "Scatterplot showing the relationship between BMI and postprandial glucose levels") +
  geom_smooth(method="lm", colour="87")

scat.bmi_glucose_meal

ggsave("scat.bmi_glucose_meal.pdf", scat.bmi_glucose_meal)

regr2 <- lm(glucose_meal ~ bmi, data=glucose, na.action=na.exclude)
summary(regr2)

# Assumption testing - BP test for homoscedasticity, DW test for autocorrelation, normality of residuals, examine how many standardized residuals are within the ranges of |3.3|, |2.58|, and |1.96|

################################################################################

# REGRESSION - BMI and bedtime glucose levels

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Scatterplot of BMI and bedtime glucose levels 
scat.bmi_glucose_bed <- ggplot(glucose, aes(glucose_bed, bmi)) + geom_point() + labs(x = "Bedtime glucose levels mg/dL", y = "BMI (kg/m2)", title = "Scatterplot showing the relationship between BMI and bedtime glucose levels") +
  geom_smooth(method="lm", colour="44")

scat.bmi_glucose_bed

ggsave("scat.bmi_glucose_bed.pdf", scat.bmi_glucose_bed)

regr3 <- lm(glucose_bed ~ bmi, data=glucose, na.action=na.exclude)
summary(regr3)

# Assumption testing - BP test for homoscedasticity, DW test for autocorrelation, normality of residuals, examine how many standardized residuals are within the ranges of |3.3|, |2.58|, and |1.96|

####################################################################################################################################
####################################################################################################################################

# WEEK 3 - WILCOXON RANK SUM TEST AND ONE-ANOVA

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Define variables that are factors
glucose$sex <- factor(glucose$sex,levels=c(0,1), labels = c("female","male"))

# WILCOXON RANK SUM TEST

# Create separate gender data objects
F.gluc_fasting <- subset(glucose, glucose$sex=="female")
M.gluc_fasting <- subset(glucose, glucose$sex=="male")

# CHECK ASSUMPTIONS

# Create histograms 
hist.F.gluc_fasting <- ggplot(F.gluc_fasting, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey",bins=5) + 
  labs(x = "Fasting glucose levels in females (mg/dL)", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(F.gluc_fasting$glucose_fasting, na.rm = TRUE), sd = sd(F.gluc_fasting$glucose_fasting, na.rm = TRUE)), colour = "orange", size = 1)+ theme_bw() + labs(title="Histogram showing fasting glucose levels in females")

hist.M.gluc_fasting <- ggplot(M.gluc_fasting, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey",bins=5) + 
  labs(x = "Fasting glucose levels in males (mg/dL)", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(M.gluc_fasting$glucose_fasting, na.rm = TRUE), sd = sd(M.gluc_fasting$glucose_fasting, na.rm = TRUE)), colour = "purple", size = 1)+ theme_bw() + labs(title="Histogram showing fasting glucose levels in males")

hist.all <- arrangeGrob(hist.F.gluc_fasting, hist.M.gluc_fasting, ncol=2, nrow=1)
grid.draw(hist.all) 
ggsave("hist_all_gender.gluc_fasting.pdf", hist.all) 

# Shapiro Wilk's test for normality
stat.desc(F.gluc_fasting$glucose_fasting, basic=F, norm=T)
stat.desc(M.gluc_fasting$glucose_fasting, basic=F, norm=T)

# Levene test for equal variances
leveneTest(glucose$glucose_fasting, glucose$sex, center=mean)

# Assign ranks to the fasting glucose variable across the two genders
rank.glucose_fasting <- rank(glucose$glucose_fasting, ties="average")

# Add to the data-set
glucose2 <- cbind(glucose, rank.glucose_fasting)

# Check
head(glucose2)

# get descriptive statistics of the fasting glucose levels for the two genders separately
by(glucose2$glucose_fasting, glucose$sex, stat.desc)

# Wilcoxon rank sum test
wilcox.glucose_fasting <- wilcox.test(glucose_fasting ~ sex, data=glucose2, alternative = "two.sided", exact=F, correct=T, paired=F)

wilcox.glucose_fasting

table(glucose2$sex)

Ws <- 5637
Nm <-  63
Nf <- 87
Wsmean <- (Nm*(Nm+Nf+1))/2
SEws <- sqrt((Nm*Nf*(Nm+Nf+1))/12)
Z <- (Ws-Wsmean)/SEws
r <- Z/sqrt(Nm+Nf)
r

################################################################################

# ONE-WAY ANOVA

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Define variables that are factors
glucose$exercise <- factor(glucose$exercise,levels=c(0,1,2), labels = c("none","weekly","daily"))

# Subset the data
exercise.none <- subset(glucose, glucose$exercise=="none")
exercise.weekly <- subset(glucose, glucose$exercise=="weekly")
exercise.daily <- subset(glucose, glucose$exercise=="daily")

# CHECK ASSUMPTIONS

# Histograms
hist.exercise.none <- ggplot(exercise.none, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey",bins=10) + 
  labs(x = "No exercise", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(exercise.none$glucose_fasting, na.rm = TRUE), sd = sd(exercise.none$glucose_fasting, na.rm = TRUE)), colour = "698", size = 1)+ theme_bw() + labs(title="Histogram showing fasting glucose levels of no exercise group")

hist.exercise.weekly <- ggplot(exercise.weekly, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey",bins=10) + 
  labs(x = "Weekly exercise", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(exercise.weekly$glucose_fasting, na.rm = TRUE), sd = sd(exercise.weekly$glucose_fasting, na.rm = TRUE)), colour = "351", size = 1)+ theme_bw() + labs(title="Histogram showing fasting glucose levels of weekly exercise group")

hist.exercise.daily <- ggplot(exercise.daily, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="grey",bins=10) + 
  labs(x = "Daily exercise", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(exercise.daily$glucose_fasting, na.rm = TRUE), sd = sd(exercise.daily$glucose_fasting, na.rm = TRUE)), colour = "172", size = 1)+ theme_bw() + labs(title="Histogram showing fasting glucose levels of daily exercise group")

hist.all.exercise <- arrangeGrob(hist.exercise.none, hist.exercise.weekly,hist.exercise.daily , ncol=3, nrow=1)
grid.draw(hist.all.exercise) 
ggsave("hist_all_exercise.pdf", hist.all)

# Normal distribution - Shapiro Wilk's test
by(glucose$glucose_fasting, glucose$exercise, shapiro.test)

# Equal variances - Levene's test
leveneTest(glucose$glucose_fasting, glucose$exercise, center=mean)

# Descriptives 
by(glucose$glucose_fasting, glucose$exercise, stat.desc)

# ANOVA
exercise_model <- aov(glucose_fasting ~ exercise, data=glucose, na.action=na.exclude)

summary(exercise_model)

print(levels(as.factor(glucose$exercise)))

# POST HOC TEST 

# Pairwise comparison with Bonferroni correction
pairwise.t.test(glucose$glucose_fasting, glucose$exercise, p.adjust.method="bonferroni")

####################################################################################################################################
##############################################################################################################################################

# WEEK 4 - REPEATED MEASURES ANOVA, NONPARAMETRIC POSTHOC (WILCOXON)

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Select relevant variables 
glucose2 <- glucose[,c(1,2:4)]

head(glucose2)

# Concert to long format
glucose2long <- melt(glucose2, id = c("ID"), measured =c("glucose_fasting","glucose_meal","glucose_bed"))

head(glucose2long)

# change name column 2 for convenience
colnames(glucose2long)[2]<- c("condition")
colnames(glucose2long)[3]<- c("value")

################################################################################
# Plot trend

trend.glucose <- ggplot(glucose2long,aes(condition, value)) +
  stat_summary(fun=mean,geom="point") +
  stat_summary(fun=mean, geom="line", aes(group=1), colour= "Blue", linetype="dashed") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=.2) +
  labs(x="Glucose measurements", y="Mean glucose levels (mg/dL)")

grid.draw(trend.glucose) 
ggsave("line_glucose.pdf", trend.glucose)

################################################################################

# Histograms
hist.glucose1 <- ggplot(glucose, aes(glucose_fasting)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=7) + 
  labs(x = "Fasting glucose levels (mg/dL)", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_fasting, na.rm = TRUE), sd = sd(glucose$glucose_fasting, na.rm = TRUE)), colour = "412", size = 1)+
  theme_bw() + labs(title="Histogram showing distribution of  fasting glucose levels")

hist.glucose2 <- ggplot(glucose, aes(glucose_meal)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=7) + 
  labs(x = "Postprandial glucose levels (mg/dL)", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_meal, na.rm = TRUE), sd = sd(glucose$glucose_meal, na.rm = TRUE)), colour = "412", size = 1)+
  theme_bw() + labs(title="Histogram showing distribution of postprandial glucose levels")

hist.glucose3 <- ggplot(glucose, aes(glucose_bed)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=7) + 
  labs(x = "Postprandial glucose levels (mg/dL)", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$glucose_bed, na.rm = TRUE), sd = sd(glucose$glucose_bed, na.rm = TRUE)), colour = "412", size = 1)+
  theme_bw() + labs(title="Histogram showing distribution of bedtime glucose levels")

hist.all <- arrangeGrob(hist.glucose1,hist.glucose2,hist.glucose3,ncol=3,nrow=1)
grid.draw(hist.all) 
ggsave("hist_all_SBP.pdf", hist.all) 

by(glucose2long$value,glucose2long$condition,shapiro.test)

###############################################################################
# Check Mauchly test

glucosemod <-aov_ez("ID", "value",glucose2long, between = NULL, within = c("condition"), 
                anova_table=list(correction = "GG", es = "pes"), print.formula=T, 
                type=c("3"), check_contrasts=T,
                return = c("afex_aov"))

summary(glucosemod)

# Run Friedman's ANOVA

glucose3 <- glucose2[,c(-1)]
head(glucose3)

friedman.test(as.matrix(glucose3))

###############################################################################

# Non-parametric posthoc: the Wilcoxon test

wilcoxDiff1 <- wilcox.test(glucose3$glucose_fasting, glucose3$glucose_meal, paired=T)  
wilcoxDiff1

wilcoxDiff2 <- wilcox.test(glucose3$glucose_meal, glucose3$glucose_bed, paired=T)  
wilcoxDiff2

wilcoxDiff3 <- wilcox.test(glucose3$glucose_bed, glucose3$glucose_fasting, paired=T)  
wilcoxDiff3


rFromWilcox<-function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(wilcoxDiff1, 24)
rFromWilcox(wilcoxDiff2, 24)
rFromWilcox(wilcoxDiff3, 24)

####################################################################################################################################
####################################################################################################################################

# WEEK 5 - MULTIPLE REGRESSION, MULTIPLE WITH INTERACTION

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Multiple regression 

# Scatterplot of BMI and fasting glucose
scat.bmi_glucose_fasting <- ggplot(glucose, aes(glucose_fasting, bmi)) + geom_point() + labs(x = "Fasting glucose levels (mg/dL)", y = "BMI (kg/m2)") +
  geom_smooth(method="lm", colour="450") + theme_light()

scat.bmi_glucose_fasting

ggsave("scat.bmi_glucose_fasting.pdf", scat.bmi_glucose_fasting)

# Regression model 1 with only BMI as predictor
regr1 <- lm(glucose_fasting ~ bmi, data=glucose, na.action=na.exclude)
summary(regr1)

# Standardized beta coefficient 
lm.beta(regr1)

# Confidence intervals
confint(regr1)

# Assumptions of regression model 1 already checked (WEEK 2)

# Evaluate correlations between the three variables of interest - fasting glucose, BMI, kcal
cor.test(glucose$glucose_fasting,glucose$bmi, alternative="two.sided", method="spearman", conf.level=.95)
cor.test(glucose$glucose_fasting,glucose$kcal, alternative="two.sided", method="spearman", conf.level=.95)
cor.test(glucose$bmi,glucose$kcal, alternative="two.sided", method="spearman", conf.level=.95)

# Regression model 2 with both bmi and kcal as predictors
regr2 <- lm(glucose_fasting ~ bmi + kcal, data=glucose, na.action=na.exclude)
summary(regr2)

# Compare fit of model
anova(regr1, regr2)

vif(regr2)
mean(vif(regr2))

################################################################################

# Check assumptions model 2

# Check linearity of kcal-fasting glucose association 
scat.linear <- ggplot(glucose,aes(kcal,glucose_fasting)) +
  ggtitle("Linearity of association between fasting glucose levels and average daily caloric intake")+
  geom_point() + labs(x="Average daily caloric intake (kcal)", y="Fasting glucose levels (mg/dL)") +
  geom_smooth(method="lm", colour="purple")
scat.linear 
ggsave("scat_linearity.pdf", scat.linear)

# Residual plots - (check distribution of residuals)

# Save residuals and predicted values
glucose$fitted <- regr2$fitted.values     	# predicted values
glucose$resid <- regr2$residuals           	# raw residuals 
glucose$stand.resid <- rstandard(regr2)   	# standardized residuals
head(glucose)

# Standardized residuals versus observed values bmi (heteroscedasticity)
scat.obs.resid <- ggplot(glucose, aes(bmi, stand.resid)) +
  geom_point() + labs(x="Observed BMI values", y="Standardized residuals") +
  geom_smooth(method="lm", colour="115")

# Standardized residuals versus ID (independence)
scat.resid.ID <- ggplot(glucose, aes(ID, stand.resid)) +
  geom_point() + labs(x="ID", y="Standardized residuals") +
  geom_smooth(method="lm", colour="115")

# Distribution of standardized residuals (normality)
hist.resid <- ggplot(glucose, aes(stand.resid)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=7) + 
  labs(x = "Residuals", y = "Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(glucose$stand.resid, na.rm = TRUE), sd = sd(glucose$stand.resid, na.rm = TRUE)), colour = "115", size = 1) + theme_bw() + labs(title="Histogram showing distribution of standardised residuals")

# Put all the assumption plots together in one window
scat.all <- arrangeGrob(scat.obs.resid, scat.resid.ID, hist.resid, ncol=3, nrow=1)
grid.draw(scat.all) # interactive device
ggsave("scat_residuals.pdf", scat.all) # save image file

# Breusch-Pagan test for homoscedasticity
bptest(regr2)

# Durbin-Watson test for autocorrelation
dwtest(regr2,alternative ="two.sided")

# SW test for normality of residuals
stat.desc(glucose$stand.resid,basic=F,norm=T)

# Check distribution of standardized residuals against normal distribution
aux3 <-as.vector(glucose$stand.resid)
aux3
y1 <- c(aux3[aux3< -3.3], aux3[aux3>3.3])	  # > |3.3|
y2 <- c(aux3[aux3< -2.58], aux3[aux3>2.58])	# > |2.58|
y3 <- c(aux3[aux3< -1.96], aux3[aux3>1.96])	# > |1.96|
y1
y2
y3

################################################################################

# MULTIPLE WITH INTERACTION

# Clear existing workspace objects
rm(list = ls())

# Read in CSV-format file
glucose <- read.csv("glucose (1).csv", header = TRUE)

# Define variables that are factors
glucose$sex <- factor(glucose$sex,levels=c(0,1), labels = c("female","male"))
glucose$exercise <- factor(glucose$exercise,levels=c(0,1,2), labels = c("none","weekly","daily"))

int <- lm(glucose_fasting ~ bmi + exercise + bmi*exercise, data=glucose)
summary(int)

# Check which exercise group is used as reference
by(glucose$glucose_fasting, glucose$exercise,describe)

int0 <- lm(glucose_fasting ~ exercise , data=glucose)
summary(int0)

# Scatterplot
gluc.bmi.exercise <- ggplot(glucose, aes(bmi, glucose_fasting)) + geom_point(aes(shape=exercise)) + geom_smooth(method = "lm", aes(fill = exercise),  size=.5) + scale_shape_manual(values=c(5,7,9)) + labs(x="BMI (kg/m2)", y="Fasting glucose levels (mg/dL)", title="Scatterplot showing the change in relationship between fasting glucose levels and BMI as a function of exercise")

gluc.bmi.exercise

####################################################################################################################################
####################################################################################################################################