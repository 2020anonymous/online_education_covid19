

# This script includes the code to fit the models in paper "Internet search data shows increasing interest in online education during the COVID-19 pandemic, with females showing greater increases". The script runs in R (version 3.4.1).

# read the synthetic dataset
sample_data<-read.table(paste(data_directory, "synthetic_sample_data", ".txt", sep=""), header=TRUE, stringsAsFactors=FALSE, as.is=TRUE)
## in the above line, please replace data_directory with the directory where you store the synthetic sample data.

## the synthetic dataset sample_data has 9 columns: 
# week_index (which range from 1 to 40 to indicate the 40 weeks' serach data), age_group_index (which takes value 0~4, representing 24 yr or younger, 25-34 yr, 35-49 yr, 50-64 yr and 4 represents 65 yr or older respectively), 
# gender_index (which takes value 0 and 1, representing males and females respectively), 
# phase_indicator (which takes value 0, 1 and 2, representing pre-pandemic phase, lockdown phase, and 'new normal' phase respectively), 
# search_intensity_onlineEdu and search_weight_onlineEdu (calculated as the inverse of the SE of the corresponding search_intensity_onlineEdu),
# search_intensity_unemployment,
# covid_case and covid_death (the number of covid cases and deaths per 100,000 people in the US).


# set the baseline
sample_data[,'week_index']<-as.factor(sample_data[,'week_index'])
sample_data[,'week_index']<-relevel(sample_data[,'week_index'], ref = '21')  ## use the 21st week (week of March 2-8) as baseline

sample_data[,'gender_index']<-as.factor(sample_data[,'gender_index'])
sample_data[,'gender_index']<-relevel(sample_data[,'gender_index'], ref = '0')  ## use gender 0 (males) as baseline

sample_data[,'age_group_index']<-as.factor(sample_data[,'age_group_index'])
sample_data[,'age_group_index']<-relevel(sample_data[,'age_group_index'], ref = '0')   ## use age group 0 (24 years old or younger) as baseline

sample_data[,'phase_indicator']<-as.factor(sample_data[,'phase_indicator'])
sample_data[,'phase_indicator']<-relevel(sample_data[,'phase_indicator'], ref = '0')   ## use phase 0 (pre-pandemic period) as baseline


# model 1: overall changes of search intensity during the progress of the pandemic
model_fit<-lm(as.formula('search_intensity_onlineEdu ~  as.factor(age_group_index) +  as.factor(gender_index) +  as.factor(phase_indicator)   '), data=data.frame(sample_data), weight = search_weight_onlineEdu)
# obtain coefficients of phase_indicator for lockdown phase and 'new normal' phase
summary(model_fit)$coefficients[c('as.factor(phase_indicator)1', 'as.factor(phase_indicator)2') ,]


# model 2: differential effect among females and males
model_fit<-lm(as.formula('search_intensity_onlineEdu ~  as.factor(age_group_index) +  as.factor(gender_index) +  as.factor(phase_indicator)  + as.factor(phase_indicator):as.factor(gender_index)  '), data=data.frame(sample_data), weight = search_weight_onlineEdu)
# obtain differential effect among females and males, during lockdown phase and 'new normal' phase respectively
summary(model_fit)$coefficients[c('as.factor(gender_index)1:as.factor(phase_indicator)1', 'as.factor(gender_index)1:as.factor(phase_indicator)2') ,]



# model 3: overall changes of search intensity during the progress of the pandemic, with the number of covid-19 cases and deaths
model_fit<-lm(as.formula('search_intensity_onlineEdu ~  as.factor(age_group_index) +  as.factor(gender_index) +  covid_case  +  covid_death  '), data=data.frame(sample_data), weight = search_weight_onlineEdu)
# obtain coefficients of covid_case and covid_death
summary(model_fit)$coefficients[c('covid_case', 'covid_death') ,]


# model 4: differential effect among females and males, with the number of covid-19 cases and deaths
model_fit<-lm(as.formula('search_intensity_onlineEdu ~  as.factor(age_group_index) +  as.factor(gender_index) +  covid_case  +  covid_death +  covid_case:as.factor(gender_index)  +  covid_death:as.factor(gender_index) '), data=data.frame(sample_data), weight = search_weight_onlineEdu)
# obtain differential effect among females and males, with respect to covid cases and deaths
summary(model_fit)$coefficients[c('as.factor(gender_index)1:covid_case', 'as.factor(gender_index)1:covid_death') ,]

# model 5: differential effect among females and males, within each age group
model_fit<-lm(as.formula('search_intensity_onlineEdu ~  as.factor(age_group_index) +  as.factor(gender_index) +  as.factor(phase_indicator)  + as.factor(phase_indicator):as.factor(age_group_index)  +as.factor(gender_index):as.factor(age_group_index) + as.factor(phase_indicator):as.factor(gender_index):as.factor(age_group_index)  '), data=data.frame(sample_data), weight = search_weight_onlineEdu)

# obtain differential effect among females and males, during lockdown phase and 'new normal' phase respectively, within each age group
# age group 0 (24 yr or younger)
summary(model_fit)$coefficients[c('as.factor(age_group_index)0:as.factor(gender_index)1:as.factor(phase_indicator)1', 'as.factor(age_group_index)0:as.factor(gender_index)1:as.factor(phase_indicator)2') ,]

# age group 1 (25-34 yr)
summary(model_fit)$coefficients[c('as.factor(age_group_index)1:as.factor(gender_index)1:as.factor(phase_indicator)1', 'as.factor(age_group_index)1:as.factor(gender_index)1:as.factor(phase_indicator)2') ,]

# age group 2 (35-49 yr)
summary(model_fit)$coefficients[c('as.factor(age_group_index)2:as.factor(gender_index)1:as.factor(phase_indicator)1', 'as.factor(age_group_index)2:as.factor(gender_index)1:as.factor(phase_indicator)2') ,]

# age group 3 (50-64 yr)
summary(model_fit)$coefficients[c('as.factor(age_group_index)3:as.factor(gender_index)1:as.factor(phase_indicator)1', 'as.factor(age_group_index)3:as.factor(gender_index)1:as.factor(phase_indicator)2') ,]

# age group 4 (65 yr or above)
summary(model_fit)$coefficients[c('as.factor(age_group_index)4:as.factor(gender_index)1:as.factor(phase_indicator)1', 'as.factor(age_group_index)4:as.factor(gender_index)1:as.factor(phase_indicator)2') ,]



# model 6: relationship between concern over unemployment and searches of online education
model_fit<-lm(as.formula('search_intensity_onlineEdu ~ search_intensity_unemployment +  as.factor(age_group_index) +  as.factor(gender_index) +  covid_case  +  covid_death  '), data=data.frame(sample_data), weight = search_weight_onlineEdu)
# obtain coefficients of search_intensity_unemployment, which captures the relationship between concern over unemployment and searches of online education
summary(model_fit)$coefficients[c('search_intensity_unemployment') ,]


# model 7: DID model assumption check: replace the interaction of phrase indicators and female indicator in model (2) with the interaction of week indicators and female indicator
model_fit<-lm(as.formula('search_intensity_onlineEdu ~  as.factor(age_group_index) +  as.factor(gender_index) +  as.factor(week_index)  + as.factor(week_index):as.factor(gender_index)  '), data=data.frame(sample_data), weight = search_weight_onlineEdu)
# obtain the coefficients of the interaction of week indicators and female indicator
summary(model_fit)$coefficients[paste('as.factor(gender_index)1:as.factor(week_index)', c(1:20, 22:40), sep=""),]


