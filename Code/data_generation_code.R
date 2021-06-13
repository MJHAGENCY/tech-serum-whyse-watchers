library(dplyr)

setwd('/Users/kelsey.huntzberry/OneDrive - Personal Use/Microsoft_Hackathon')
health_met <- read.csv('county_health_metrics.csv',stringsAsFactors=FALSE)
census <- read.csv('US_census_data.csv',stringsAsFactors=FALSE)
rur_urban <- read.csv('rural_urban_region.csv',stringsAsFactors=FALSE)
med_exp <- read.csv('medicaid_expansion.csv',stringsAsFactors=FALSE)
uv_index <- read.csv('UV_Index_County.csv',stringsAsFactors=FALSE)

cancer_yn <- as.data.frame(sample(c(0,1), size=60000,prob=c(0.92,0.08),replace=TRUE),stringsAsFactors=FALSE)
colnames(cancer_yn) <- 'cancer_actual'

cancer_yes_temp <- cancer_yn %>% filter(cancer_actual == 1)
cancer_no_temp <- cancer_yn %>% filter(cancer_actual == 0)

yes_rows = nrow(cancer_yes_temp)
no_rows = nrow(cancer_no_temp)

pred_yes <- sample(c(0,1), size=yes_rows, prob=c(0.13,0.87),replace=TRUE)
pred_no <- sample(c(0,1), size=no_rows, prob=c(0.86,0.14),replace=TRUE)

cancer_yes_temp$pred_cancer = pred_yes
cancer_no_temp$pred_cancer = pred_no

cancer_yes_noise <- cancer_yes_temp[1:500,]
cancer_no_noise <- cancer_no_temp[1:6000,]
cancer_yes <- cancer_yes_temp[500:yes_rows,]
cancer_no <- cancer_no_temp[6000:no_rows,]

cancer_noise <- bind_rows(cancer_yes_noise, cancer_no_noise)

yes_rows = nrow(cancer_yes)
no_rows = nrow(cancer_no)
noise_rows = nrow(cancer_noise)

gender_yes <- sample(c('female','male'), size=yes_rows, prob=c(0.45,0.55),replace=TRUE)
gender_no <- sample(c('female','male'), size=no_rows, prob=c(0.49,0.52),replace=TRUE)
gender_noise <- sample(c('female','male'), size=noise_rows, prob=c(0.55,0.45),replace=TRUE)

cancer_yes$gender <- gender_yes
cancer_no$gender <- gender_no
cancer_noise$gender <- gender_noise

set.seed(55)
cancer_yes$age <- round(rnorm(n=yes_rows, mean=50, sd=10))
summary(cancer_yes$age)

set.seed(384)
cancer_no$age <- round(rnorm(n=no_rows, mean=45, sd=8))
summary(cancer_no$age)

set.seed(194)
cancer_noise$age <- round(rnorm(n=noise_rows, mean=50, sd=11))
summary(cancer_noise$age)

med_hh <- median(census$med_hh_income)
med_hh

high_zips <- census %>% filter(med_hh_income > med_hh)
low_zips <- census %>% filter(med_hh_income <= med_hh)

high_fips <- high_zips$FIPS
low_fips <- low_zips$FIPS
all_fips <- census$FIPS

cancer_no$FIPS <- sample(high_fips, size=no_rows,replace=TRUE)
cancer_yes$FIPS <- sample(low_fips, size=yes_rows,replace=TRUE)
cancer_noise$FIPS <- sample(all_fips, size=noise_rows,replace=TRUE)

races <- c('White','Black','Asian','Other')
ethnicities <- c('Hispanic', 'Not Hispanic')

race_yes <- sample(races, size=yes_rows, prob=c(0.85,0.03,0.07,0.05),replace=TRUE)
ethn_yes <- sample(ethnicities, size=yes_rows, prob=c(0.15,0.85),replace=TRUE)

race_no <- sample(races, size=no_rows, prob=c(0.77,0.10,0.09,0.04),replace=TRUE)
ethn_no <- sample(ethnicities, size=no_rows, prob=c(0.2,0.8),replace=TRUE)

race_noise <- sample(races, size=noise_rows, prob=c(0.76,0.13,0.08,0.03),replace=TRUE)
ethn_noise <- sample(ethnicities, size=noise_rows, prob=c(0.22,0.78),replace=TRUE)

cancer_yes$race <- race_yes
cancer_yes$ethnicity <- ethn_yes

cancer_no$race <- race_no
cancer_no$ethnicity <- ethn_no

cancer_noise$race <- race_noise
cancer_noise$ethnicity <- ethn_noise

exposure_values <- c('Regular Sun Exposure','Occasional Sun Exposure') 

exposure_yes <- sample(exposure_values, size=yes_rows, prob=c(0.8,0.2),replace=TRUE)
exposure_no <- sample(exposure_values, size=no_rows, prob=c(0.5,0.5),replace=TRUE)
exposure_noise <- sample(exposure_values, size=noise_rows, prob=c(0.6,0.4),replace=TRUE)

cancer_yes$sun_exposure <- exposure_yes
cancer_no$sun_exposure <- exposure_no
cancer_noise$sun_exposure <- exposure_noise

sunburn_values <- c('Less than 5 Sunburns','5 or More Sunburns') 

sunburn_yes <- sample(sunburn_values, size=yes_rows, prob=c(0.73,0.17),replace=TRUE)
sunburn_no <- sample(sunburn_values, size=no_rows, prob=c(0.53,0.47),replace=TRUE)
sunburn_noise <- sample(sunburn_values, size=noise_rows, prob=c(0.45,0.55),replace=TRUE)

cancer_yes$sunburn_5OrMore <- sunburn_yes
cancer_no$sunburn_5OrMore <- sunburn_no
cancer_noise$sunburn_5OrMore <- sunburn_noise

fam_hist_values <- c('Has Family History','Does Not Have Family History') 

famhist_yes <- sample(fam_hist_values, size=yes_rows, prob=c(0.3,0.7),replace=TRUE)
famhist_no <- sample(fam_hist_values, size=no_rows, prob=c(0.10,0.90),replace=TRUE)
famhist_noise <- sample(fam_hist_values, size=noise_rows, prob=c(0.2,0.8),replace=TRUE)

cancer_yes$family_history <- famhist_yes
cancer_no$family_history <- famhist_no
cancer_noise$family_history <- famhist_noise

pers_hist_values <- c('Has Personal History','Does Not Have Personal History') 

personal_yes <- sample(pers_hist_values, size=yes_rows, prob=c(0.10,0.9),replace=TRUE)
personal_no <- sample(pers_hist_values, size=no_rows, prob=c(0.03,0.97),replace=TRUE)
personal_noise <- sample(pers_hist_values, size=noise_rows, prob=c(0.04,0.96),replace=TRUE)

cancer_yes$personal_history <- personal_yes
cancer_no$personal_history <- personal_no
cancer_noise$personal_history <- personal_noise

hair_color_values <- c('Brown','Blonde','Red','Black') 

hair_color_yes <- sample(hair_color_values, size=yes_rows, prob=c(0.6,0.25,0.05,0.10),replace=TRUE)
hair_color_no <- sample(hair_color_values, size=no_rows, prob=c(0.7,0.17,0.03,0.10),replace=TRUE)
hair_color_noise <- sample(hair_color_values, size=noise_rows, prob=c(0.65,0.22,0.04,0.10),replace=TRUE)

cancer_yes$hair_color <- hair_color_yes
cancer_no$hair_color <- hair_color_no
cancer_noise$hair_color <- hair_color_noise

eye_color_values <- c('Brown','Blue','Green') 

eye_color_yes <- sample(eye_color_values, size=yes_rows, prob=c(0.55,0.3,0.15),replace=TRUE)
eye_color_no <- sample(eye_color_values, size=no_rows, prob=c(0.75,0.15,0.10),replace=TRUE)
eye_color_noise <- sample(eye_color_values, size=noise_rows, prob=c(0.65,0.22,0.13),replace=TRUE)

cancer_yes$eye_color <- eye_color_yes
cancer_no$eye_color <- eye_color_no
cancer_noise$eye_color <- eye_color_noise

cancer_all_temp <- bind_rows(cancer_yes, cancer_no, cancer_noise)

cancer_all_yes <- cancer_all_temp %>% filter(pred_cancer == 1)
cancer_all_no <- cancer_all_temp %>% filter(pred_cancer == 0)
rows_all_yes = nrow(cancer_all_yes)
rows_all_no = nrow(cancer_all_no)

set.seed(48)
prob_yes <- rnorm(n=rows_all_yes, mean=0.90, sd=0.04)
summary(prob_yes)

set.seed(48)
prob_no <-rnorm(n=rows_all_no, mean=0.20, sd=0.04)
summary(prob_no)

cancer_all_yes$prob_cancer <- prob_yes
cancer_all_no$prob_cancer <- prob_no

cancer_all <- bind_rows(cancer_all_yes, cancer_all_no)

cancer_all$prob_cancer[cancer_all$prob_cancer < 0] <- 0
cancer_all$prob_cancer[cancer_all$prob_cancer > 1] <- 1

health_met_sm <- health_met %>% select(-state,-county)

cancer_whlth_met <- left_join(cancer_all, health_met_sm) %>%
                        left_join(., census) %>%
                        left_join(., rur_urban) %>%
                        left_join(., med_exp) %>%
                        left_join(., uv_index)

write.csv(cancer_whlth_met, '/Users/kelsey.huntzberry/OneDrive - Personal Use/Microsoft_Hackathon/tech_serum_hackathon_data.csv', row.names=FALSE)

