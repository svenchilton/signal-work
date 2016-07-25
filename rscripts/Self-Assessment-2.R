setwd('/Users/svenchilton/GitHub/signal-data-science/R-curriculum/assignments/self-assessments/self-assessment-2')

library('psych')
library('dplyr')
library('ggplot2')
library('glmnet')
library('corrplot')

# Start time: 10:01 am
# End time: 3:04 pm

# Set the seed to 1 for reproducibility
set.seed(1)

# Create the alpha-lambda search grid
alphas  = (0:10)/10
lambdas = 10^seq(1, -3, length.out=50)
param_grid = expand.grid(alpha = alphas, lambda = lambdas)
View(param_grid)

# Load the msq data frame and fill NA values with column means
df = msq
View(df)

for(i in 1:ncol(df)) {
  if (length(df[is.na(df[,i]), i]) > 0) {
    df[is.na(df[,i]), i] = mean(df[,i], na.rm=TRUE)
  }
}

# Extract the features
features = dplyr::select(df, active:scornful)

# Make separate variables for the extraversion and 
# neuroticism columns
extraversion = df[['Extraversion']]
neuroticism  = df[['Neuroticism']]

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}

# n-fold cross-validation
nfold_cv = function(features, 
                    target1 = extraversion, 
                    target2 = neuroticism, 
                    n_folds = 10,
                    alpha,
                    lambda) {
  # Total number of rows in the features df
  n_rows = nrow(features)
  # Make sure that the features df and the target vectors 
  # have the same length
  if (length(target1) != n_rows) stop('features data frame and target1 vector must have same length')
  if (length(target2) != n_rows) stop('features data frame and target2 vector must have same length')
  # Find the number of rows in each bin
  # Upper bound
  bin_rows_ub = ceiling(n_rows/n_folds)
  # Cycle through the number of folds/bins until we have the 
  # total number of rows of the df, then sort the result
  bin_vec = sort(head(rep(1:n_folds, bin_rows_ub), n_rows))
  # Shuffle the rows of the df
  shuff_rows = sample(1:n_rows, n_rows, replace = FALSE)
  # Initialize lists with n_folds elements, to be populated with: 
  # training features
  # test features
  # target1 training values 
  # target2 training values
  train_features = vector('list',n_folds)
  test_features  = vector('list',n_folds)
  train_target1  = vector('list',n_folds)
  train_target2  = vector('list',n_folds)
  # Iterate through each fold/bin and populate the bins
  for (i in 1:n_folds) {
    train_features[[i]] = features[shuff_rows[bin_vec != i],]
    test_features[[i]]  = features[shuff_rows[bin_vec == i],]
    train_target1[[i]]  = target1[shuff_rows[bin_vec != i]]
    train_target2[[i]]  = target2[shuff_rows[bin_vec != i]]
  }
  # Initialize vector which will contain predicted values 
  # of the targets
  predictions1 = rep(NA, n_rows)
  predictions2 = rep(NA, n_rows)
  # Initialize a counter for populating the predictions vectors
  j = 0
  # Iterate through each bin again
  for (i in 1:n_folds) {
    print(paste0('alpha = ',alpha,', lambda = ',lambda,', fold = ',i))
    # Scale the training and test features from the current bin
    # Make sure to scale the test features with the same parameters 
    # as the training features
    scaled_train_features = scale(train_features[[i]])
    center    = attributes(scaled_train_features)[['scaled:center']]
    scale_fac = attributes(scaled_train_features)[['scaled:scale']]
    scaled_test_features = 
      scale(test_features[[i]], center=center, scale=scale_fac)
    # Run the glmnet model on the training set
    fit1 = glmnet(scaled_train_features, train_target1[[i]], 
                  alpha=alpha, lambda=lambda)
    fit2 = glmnet(scaled_train_features, train_target2[[i]], 
                  alpha=alpha, lambda=lambda)
    # Populate the predictions vectors with predictions 
    # based on the test set
    num_test_rows = nrow(scaled_test_features)
    predictions1[(j+1):(j+num_test_rows)] = 
      predict(fit1, scaled_test_features, s=lambda)
    predictions2[(j+1):(j+num_test_rows)] = 
      predict(fit2, scaled_test_features, s=lambda)
    # Update the counter
    j = j+num_test_rows
  }
  # Return the RMSEs between predictions and the actual target values
  rmse1 = rmse(predictions1, target1[shuff_rows])
  rmse2 = rmse(predictions2, target2[shuff_rows])
  return(c(rmse1=rmse1, rmse2=rmse2))
}

# Function for computing the n-fold cross-validated RMSEs of two targets
# given a range of alpha and lambda values
# Returns a data frame with 4 columns: 
# alpha, lambda, rmse_target1, rmse_target2
grid_search = function(features, 
                       target1 = extraversion, 
                       target2 = neuroticism, 
                       n_folds = 10,
                       alphas  = alphas,
                       lambdas = lambdas) {
  # Total number of rows in the features df
  n_rows = nrow(features)
  # Make sure that the features df and the target vectors 
  # have the same length
  if (length(target1) != n_rows) stop('features data frame and target1 vector must have same length')
  if (length(target2) != n_rows) stop('features data frame and target2 vector must have same length')
  # Initialize the df to be returned
  n_output_rows = length(alphas)*length(lambdas)
  grid_df = expand.grid(lambda = lambdas, alpha = alphas)
  grid_df = grid_df[,c(2,1)]
  colnames(grid_df) = c('alpha', 'lambda')
  rmse1col = rep(NA, n_output_rows)
  rmse2col = rep(NA, n_output_rows)
  for (i in 1:length(alphas)) {
    alpha = alphas[[i]]
    for (j in 1:length(lambdas)) {
      rownum = (i-1)*length(lambdas) + j
      lambda = lambdas[[j]]
      rmses = nfold_cv(features, target1, target2, n_folds, alpha, lambda)
      rmse1col[rownum] = rmses[1]
      rmse2col[rownum] = rmses[2]
    }
  }
  # It appears to be faster to grow a df by two columns than to set the 
  # elements in those columns of a pre-initialized df row by row
  grid_df[,'rmse_target1'] = rmse1col
  grid_df[,'rmse_target2'] = rmse2col
  return(grid_df)
}

# Run the overarching function to generate a df containing the target RMSE 
# values for every given alpha and lambda
grid_search_df = grid_search(features,
                             target1 = extraversion, 
                             target2 = neuroticism, 
                             n_folds = 10,
                             alphas  = alphas,
                             lambdas = lambdas)
View(grid_search_df)

# Rename the RMSE columns
colnames(grid_search_df)[c(3,4)] = c('rmse_extraversion', 'rmse_neuroticism')

# Function for returning the index of the vector v corresponding to the 
# first appearance of its minimal value
arg_min = function(v) which.min(v)

# Find the rows of grid_search_df corresponding to the minimal RMSE 
# values for predicting both extraversion and neuroticism
extraversion_min_row = arg_min(grid_search_df[['rmse_extraversion']])
neuroticism_min_row  = arg_min(grid_search_df[['rmse_neuroticism']])

# Find the corresponding alpha and lambda values
extraversion_min_params = grid_search_df[extraversion_min_row, c(1,2)]
neuroticism_min_params  = grid_search_df[neuroticism_min_row,  c(1,2)]

# Train regularized linear models for extraversion and neuroticism 
# using the optimal values of alpha and lambda determined above 
# by employing glmnet() ON THE ENTIRE DATA SET
extraversion_alpha  = extraversion_min_params[1]
extraversion_lambda = extraversion_min_params[2]
extraversion_fit = glmnet(scale(features), 
                          extraversion, 
                          alpha=extraversion_alpha, 
                          lambda=extraversion_lambda)
neuroticism_alpha  = neuroticism_min_params[1]
neuroticism_lambda = neuroticism_min_params[2]
neuroticism_fit = glmnet(scale(features), 
                          neuroticism, 
                          alpha=neuroticism_alpha, 
                          lambda=neuroticism_lambda)

# Extract the coefficients associated with these regularized linear 
# models for extraversion and neuroticism
extraversion_coeffs = 
  as.matrix(coef(extraversion_fit, s=extraversion_lambda))
neuroticism_coeffs  = 
  as.matrix(coef(neuroticism_fit,  s=neuroticism_lambda))

# Combine the 1-column coefficient matrices into a single matrix
coeffs = cbind(extraversion = extraversion_coeffs, 
               neuroticism  = neuroticism_coeffs)

# Then clean it up by: 
# Changing the column names
colnames(coeffs) = c('Extraversion','Neuroticism')

# Removing the top row, corresponding to the intercept
coeffs = tail(coeffs, -1)

# Removing rows where both coefficients are 0
extraversion_zeros = coeffs[,1] == 0.0
neuroticism_zeros  = coeffs[,2] == 0.0
coeffs = coeffs[!(extraversion_zeros & neuroticism_zeros),]

# Calling quantile on the absolute value of each column
abs_extr_quant = quantile(abs(coeffs[,1]))
abs_neur_quant = quantile(abs(coeffs[,2]))

# Removing every row from the matrix where both of the 
# magnitudes of the coefficients fall under the 75th pecentile 
# for their respective columns.
abs_extr_below_4th_quant = abs(coeffs[,1]) < abs_extr_quant[4]
abs_neur_below_4th_quant = abs(coeffs[,2]) < abs_neur_quant[4]
coeffs = 
  coeffs[!(abs_extr_below_4th_quant & abs_neur_below_4th_quant),]

# Plot the matrix of coefficients using corrplot() 
# with is.corr=FALSE 
corrplot(coeffs, is.corr=FALSE)































