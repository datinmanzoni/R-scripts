# correlation between MADRs and Severity 
# read in file
library(readr)
Covariates_OCD <- read_csv("Covariates_OCD.csv")
View(Covariates_OCD)

# correlation between madrs and ROIs
cor.test(Covariates_OCD$MADRS, Covariates_OCD$Sev, alternative="two.sided", method="pearson", conf.level=.95)

# scatterplot 
scat.madrs.sev <- ggplot(Covariates_OCD, aes(MADRS, Sev)) +
  geom_point() + theme_classic() + labs(x="MADRS score", y="YBOCS score") +
  geom_smooth(method="lm", colour="darkorange1") 

scat.madrs.sev
grid.draw(scat.madrs.sev) 
ggsave("scat_madrs.sev_covariates_OCD.pdf", scat.madrs.sev) 
