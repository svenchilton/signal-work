# Sven Chilton and Nathan Helm-Burger
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

# African Soil Property Prediction
# Data from Kaggle Challenge
library("caret")
library("dplyr")
library("readr")
library('glmnet')

alphas = c(1, 0.1, 0.05, 0.01, 0.001, 0)
Ca_alphas = c(0.12, 0.11, 0.1, 0.09, .08)
Other_alphas = c(1, 0.9, 0.8, 0.7, 0.6)
df = data.frame(read_csv('../data/african_soil_training.csv'))

# Select bands, all columns that start with "m"
# Make sure to scale them first
bands = scale(dplyr::select(df, starts_with("m")))

# First, target Calcium from potential targets (Ca:Sand)
# and convert from list to vector
targets = dplyr::select(df, Ca:Sand)

# Initialize a list whose names match those of the columns in targets
soil_params_list = vector('list',length(colnames(targets)))
names(soil_params_list) = colnames(targets)

# Set grid of parameter values to search over
param_grid = expand.grid(alpha = alphas, lambda = 10^seq(-4, 0, length.out=10))

# Tell the Caret trainer to use 10-fold cross-validation, repeated 3 times, as a control 
# for every (alpha, lambda) pair
control = trainControl(method="repeatedcv", number=10,repeats=3, verboseIter=TRUE)

# Loop over the target columns
# WARNING: THIS TAKES FOR-EEEEEEEEEEEEEEEEEE-VER
for (col in colnames(targets)) {
  # Search over the grid
  soil_params_list[[col]] = 
    list(train(x=bands, y=targets[[col]], method="glmnet", tuneGrid=param_grid, trControl=control))
}


# Make a data frame showing the optimum (alpha, lambda) and RMSE for each target
# Initialize and populate the columns
alpha_vec  = rep(NA, ncol(targets))
lambda_vec = rep(NA, ncol(targets))
rmse_vec   = rep(NA, ncol(targets))
for (i in 1:ncol(targets)) {
  aldf = soil_params_list[[i]][[1]]$bestTune
  alpha_vec[i]  = aldf[1,'alpha']
  lambda_vec[i] = aldf[1,'lambda']
  #rmse_vec[i]   = soil_params_list[[i]][[1]]$results$RMSE
}

params_df = data.frame(target=colnames(targets), alpha=alpha_vec, lambda=lambda_vec, rmse=rmse_vec)

str(soil_params_list[[2]][[1]]$bestTune)

# Make predictions for each target


# Make a dataframe with a column for each target with predictions for each

# View the optimal values of alpha and lambda
str(caret_fit$bestTune)

# View the cross-validated RMSE estimates
str(caret_fit$results$RMSE)


# To graph the bands, we need to extract their numbers from their names and treat their numbers as type numeric
bandNames = names(bands)
waveNums = as.numeric(sapply(bandNames, function (x) {as.numeric(substr(x, 2, 10))}))


target = colnames(targets)[1]
target

is.vector(targets[[target]])



length(soil_params_list[[1]][[1]]$results$RMSE)
min(soil_params_list[[1]][[1]]$results$RMSE)


View(params_df)
View(param_grid)

soil_params_list[[1]][[1]]$results$RMSE[38]

# Load in the test data frame
test = data.frame(read_csv('../data/sorted_test.csv'))

# Select bands from the test data, all columns that start with "m"
# Make sure to scale them first
test_bands = scale(dplyr::select(test, starts_with("m")))


# Make predicitons based on the training results computed above
soil_predictions_list = vector('list',length(colnames(targets)))
names(soil_predictions_list) = colnames(targets)

for(col in colnames(targets)) {
  soil_predictions_list[[col]] = predict(soil_params_list[[col]][[1]],test_bands)
}


# Make a data frame from the predictions generated above
predictions = data.frame(soil_predictions_list)

# Make sure to include the area names (PIDN) column from the test data set
predictions = cbind(predictions, PIDN = test[['PIDN']])
head(predictions)
colnames(predictions)[6] = 'PIDN'

# Reorder the columns of the predictions data frame
col_nums = 1:length(colnames(predictions))
col_nums = c(tail(col_nums,1), head(col_nums,-1))
predictions = predictions[,col_nums]


write_csv(predictions, "Predictions.csv", col_names = TRUE)
examine_test = data.frame(read_csv("../data/African-Soil-Predictions.csv"))
head(examine_test)
dim(predictions)
dim(examine_test)


# OK, that would have put us in 900th place.  Let's see if we can do better.
# The optimum (alpha, lambda) pairs which the caret package gave us were 
# merely the best of the ones we tested.  We can start by searching different 
# ranges of alpha and lambda values closer to the optimum ones

# Set grid of parameter values to search over
lambda_mult = seq(0.2, 1.8, 0.2)

# Make a list of param grids, one for each prediction variable
grid_list = vector('list', ncol(targets))
names(grid_list) = colnames(targets)

for (i in 1:length(grid_list)) {
  target_name = names(grid_list)[i]
  if (target_name == 'Ca') alpha_vals = Ca_alphas
  else alpha_vals = Other_alphas
  print(paste0('alpha_vals = ', alpha_vals))
  lambda_vals = lambda_mult*params_df[i,'lambda']
  grid_list[[target_name]] = expand.grid(alpha = alpha_vals, lambda = lambda_vals)
}

# Now that we have our parameter grids, let's re-run glmnet
# Initialize a list whose names match those of the columns in targets
soil_fit_list_2 = vector('list', ncol(targets))
names(soil_fit_list_2) = colnames(targets)

# Loop over the target columns
# WARNING: THIS TAKES FOR-EEEEEEEEEEEEEEEEEE-VER
for (col in colnames(targets)) {
  print(c('Column in progress: ', col))
  # Search over the grid
  soil_fit_list_2[[col]] = 
    train(x=bands, y=targets[[col]], method="glmnet", tuneGrid=grid_list[[col]], trControl=control)
}

# Make a data frame showing the optimum (alpha, lambda) and RMSE for each target
# Initialize and populate the columns
alpha_vec2  = rep(NA, ncol(targets))
lambda_vec2 = rep(NA, ncol(targets))
for (i in 1:ncol(targets)) {
  aldf = soil_fit_list_2[[i]]$bestTune
  alpha_vec2[i]  = aldf[1,'alpha']
  lambda_vec2[i] = aldf[1,'lambda']
}
params_df_2 = data.frame(target=colnames(targets), alpha=alpha_vec2, lambda=lambda_vec2)
View(params_df_2)

# Make predicitons based on the training results computed above
soil_predictions_list_2 = vector('list',ncol(targets))
names(soil_predictions_list_2) = colnames(targets)
for(col in colnames(targets)) {
  soil_predictions_list_2[[col]] = predict(soil_fit_list_2[[col]],test_bands)
}

head(soil_fit_list_2)

# Make a data frame from the predictions generated above
predictions2 = data.frame(soil_predictions_list_2)

# Make sure to include the area names (PIDN) column from the test data set
predictions2 = cbind(predictions2, PIDN = test[['PIDN']])
head(predictions)

## Reorder the columns of the predictions data frame
#col_nums = 1:length(colnames(predictions2))
#col_nums = c(tail(col_nums,1), head(col_nums,-1))
#predictions2 = predictions2[,col_nums]


write_csv(predictions2, 
          "../data/African-Soil-Predictions2.csv", 
          col_names = TRUE)

# Oh, shoot, we forgot to do this earlier...
P.df = cbind(targets['P'], logP = log(targets['P']+1))
colnames(P.df) = c('P', 'logP')
ggplot(P.df) + 
  geom_histogram(aes(x=P), fill='blue', alpha=0.5, bins=100) + 
  geom_histogram(aes(x=logP), fill='red', alpha=0.5, bins=100)
View(P.df)


logP.train = train(x=bands, 
                   y=P.df[['logP']], 
                   method="glmnet", 
                   tuneGrid=grid_list[['P']], 
                   trControl=control)
logP.predict = predict(logP.train, test_bands)
newP.predict = exp(logP.predict) - 1

predictions3 = predictions2
predictions3[['P']] = newP.predict

write_csv(predictions3, 
          "../data/African-Soil-Predictions3.csv", 
          col_names = TRUE)


# For our third attempt, we can take the results from our 
# second attempt, and average them with a different method
# Trying Naive Bayes from package 'e1071'
install.packages('e1071')
library('e1071')

# Define Naive Bayes model



colnames(targets)
col = 'Ca'
subset_df = cbind(bands, target = targets[[col]])
head(subset_df[,1:6])




