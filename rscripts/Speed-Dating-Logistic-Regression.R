# Matt Bartholomew and Sven Chilton
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

library('dplyr')
library("readr")
library('glmnet')
library('pROC')

# Sigmoid function
sigmoid = function(x) 1./(1.+exp(-x))

# Read in the data frame
df = read.csv('../data/speeddating-aggregated.csv')

# Check for NAs
complete.cases(df)

# Filter them out with na.omit
df = na.omit(df)
dim(df)

View(df)

# Filter the columns by type
activities = dplyr::select(df, sports:yoga)
View(activities)

# Run 10-fold cross-validation with glmnet and L2 normalization
gender_predictor = cv.glmnet(scale(activities), df[['gender']], family='binomial')
predicted_gender_log_odds = predict(gender_predictor, scale(activities))
predicted_gender = as.numeric(predicted_gender_log_odds >= 0)
predicted_gender_sigmoid = sigmoid(predicted_gender_log_odds)
roc(df[['gender']], predicted_gender_sigmoid, plot=TRUE)

####################################

# Now let's filter out by career
df_careers = dplyr::filter(df, career_c == 2 | career_c == 7)
head(df_careers)

# Let's convert the career_c column to a binary, with 0 ==> academia,  
# and 1 ==> business/finance
df_careers[['career_c']] = (df_careers[['career_c']] - 2)/5
View(df_careers)

# Slice activities
activities_sliced = dplyr::select(df_careers, sports:yoga)

# Run 10-fold cross-validation with glmnet and L2 normalization
career_predictor = cv.glmnet(scale(activities_sliced), df_careers[['career_c']], family='binomial', alpha=0)
predicted_career_log_odds = predict(career_predictor, scale(activities_sliced))
predicted_career = as.numeric(predicted_career_log_odds >= 0)
predicted_career_sigmoid = sigmoid(predicted_career_log_odds)
roc(df_careers[['career_c']], predicted_career_sigmoid, plot=TRUE)

dim(df_careers)
dim(activities_sliced)

is.matrix(df_careers[['career_c']])

####################################

# Now let's filter out by race
# race = 2 ==> white, race = 4==> asian
df_race = dplyr::filter(df, race == 2 | race == 4)
head(df_race)

# Let's convert the race column to a binary
# race = 2 ==> white, race = 4==> asian
df_race[['race']] = (df_race[['race']] - 2)/2
View(df_race)

# Slice activities (by race)
activities_sliced = dplyr::select(df_race, sports:yoga)

# Run 10-fold cross-validation with glmnet and L2 normalization
race_predictor = cv.glmnet(scale(activities_sliced), df_race[['race']], family='binomial', alpha=0)
predicted_race_log_odds = predict(race_predictor, scale(activities_sliced))
predicted_race = as.numeric(predicted_race_log_odds >= 0)
predicted_race_sigmoid = sigmoid(predicted_race_log_odds)
roc(df_race[['race']], predicted_race_sigmoid, plot=TRUE)

####################################














