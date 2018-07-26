## ATUS Prep and visualization (from IPUMS)

#load in packages
library(haven)
library(dplyr)
library(ggplot2)

#use SPSS for value labels
atus.spss <- read_spss("data/atus_00009.sav")
atus <- as_factor(atus.spss)

#remove unwanted variables
atus <- as.data.frame(select(atus, -PERNUM, -LINENO, -WT06))

#make year a factor
atus$YEAR <- as.factor(atus$YEAR)

#change all missing variables to NA 
nrow(filter(atus, UHRSWORKT==9999))
head(filter(atus, is.na(UHRSWORKT)))

atus[atus==9999] <- NA
atus[atus==9995] <- NA
atus[atus==99999.99] <- NA

#create a variable for categorical age
atus$AGE_CAT <- ifelse(atus$AGE < 20, "Teens", 
                ifelse(atus$AGE >= 20 & atus$AGE < 30, "20s",
                ifelse(atus$AGE >= 30 & atus$AGE < 40, "30s",
                ifelse(atus$AGE >= 40 & atus$AGE < 50, "40s",
                ifelse(atus$AGE >= 50 & atus$AGE < 60, "50s",
                ifelse(atus$AGE >= 60 & atus$AGE < 70, "60s",
                ifelse(atus$AGE >= 70 & atus$AGE < 80, "70s",
                ifelse(atus$AGE >= 80, "80+", NA))))))))

#correct factor level ordering so teen comes first
atus$AGE_CAT <- factor(atus$AGE_CAT, levels=c("Teens", "20s", "30s", "40s", "50s", "60s", "70s", "80+"))

#look at summary to check for any more incorrectly coded missing values
summary(atus)

## Descriptive Statitics ################

#number of cases
nrow(atus)

#Mean age 
atus %>%
  summarize(mean_age = mean(AGE), sd_age = sd(AGE), min_age = min(AGE), max_age = max(AGE))


# Counts of age category by YEAR
table(atus$AGE_CAT, atus$YEAR)

#Mean age by year
atus %>%
  group_by(YEAR) %>% 
  summarize(mean_age = mean(AGE), sd_age = sd(AGE), count = n())

# Age by region
table(atus$AGE_CAT, atus$REGION)

#summary of volunteering and socialization by region
atus %>%
  group_by(REGION) %>% 
  summarize(mean_volunteer = mean(ACT_VOL, na.rm=T), mean_socialize = mean(ACT_SOCIAL, na.rm=T))

## Plots ############

#Time spent volunteering by age and region
ggplot(atus, aes(x=AGE_CAT, y=ACT_VOL)) + geom_bar(stat="summary", fun.y = "mean", aes(fill=REGION), position="dodge") + theme_bw()

#Time spent socializing by age and region
ggplot(atus, aes(x=AGE_CAT, y=ACT_SOCIAL)) + geom_bar(stat="summary", fun.y = "mean", aes(fill=REGION), position="dodge") + theme_bw()

#Look at relationship between work and Volunteering for each age group
ggplot(atus, aes(x=ACT_WORK, y=ACT_VOL)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~AGE_CAT) + theme_bw()

#Look at relationship between work and socializing for each age group
ggplot(atus, aes(x=ACT_WORK, y=ACT_SOCIAL)) + geom_point() + geom_smooth(method="lm") + facet_wrap(~AGE_CAT) + theme_bw()

# Correlations ####### 

atus %>% 
  group_by(AGE_CAT, REGION) %>% 
  summarize(cWorkVol = cor(ACT_WORK, ACT_VOL), cWorkSocial = cor(ACT_WORK, ACT_SOCIAL), cSocialVol = cor(ACT_SOCIAL, ACT_VOL)) 
  
