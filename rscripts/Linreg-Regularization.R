# Sven Chilton and Nathan Helm-Burger
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')
library('ggplot2')
library('dplyr')
library('Rmisc')
library('glmnet')
library('caret')

# Exploring regularization with simulated data
set.seed(1) 
j = 50
a = 0.25
x = rnorm(j)
error = sqrt(1 - a^2)*rnorm(j)
y = a*x + error
x
y

# Let's examine the fit between x and y
summary(lm(y ~ x - 1))

# Sure enough, the estimated slope is 0.2231

summary(lm(y ~ x - 1))[['coefficients']][1]

# Our first attempt at a cost function
# We are assuming that y ~= aEst*x
cost = function(x, y, aEst, lambda, p) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  # Make sure that p is 1 or 2
  if (p != 1 & p != 2) stop ('p must equal 1 or 2')
  return(sum((y - aEst*x)^2) + lambda*abs(aEst)^p)
}

# Ensure that cost() is working properly with this test: 
cost(1, 2, 3, 4, 2)

# This does, indeed, return 37, as expected

# Create a vector of lambda values
lambda_vec = 2^(-2:7)

# Create a vector of aEst values
aEst_vec = seq(-0.1, 0.3, 0.001)

# Create a data frame where each row represents a unique 
# combination of lambda and alpha
grid = expand.grid(lambda=lambda_vec, alpha=aEst_vec)
View(grid)


# Add costL1 and costL2 columns to our grid data frame
# First, initialize column vectors
costL1_vec = rep(NA, nrow(grid))
costL2_vec = rep(NA, nrow(grid))
# Loop through the rows and use them to populate the 
# cost vectors
for (i in 1:nrow(grid)) {
  alpha  = grid[i,'alpha']
  lambda = grid[i,'lambda']
  costL1_vec[i] = cost(x, y, alpha, lambda, 1)
  costL2_vec[i] = cost(x, y, alpha, lambda, 2)
}
# Add the costL1 and costL2 vectors to the grid data frame 
# as columns
grid[,'costL1'] = costL1_vec
grid[,'costL2'] = costL2_vec


# Function for extracting a plot object showing the regularized 
# cost function
get_plot = function(lambda, p, df=grid) {
  # Make sure that p is 1 or 2
  if (p != 1 & p != 2) stop ('p must equal 1 or 2')
  # Extract the columns of our df for which the lambda column has the 
  # same value as the lambda argument
  df = df[df[,"lambda"] == lambda,]
  # Create the plot object
  if (p == 1) {
    plot_object = ggplot(df) + 
      geom_point( aes(alpha, costL1), color='red') +
      geom_smooth(aes(alpha, costL1), color='blue')
  } else if (p == 2) {
    plot_object = ggplot(df) + 
      geom_point( aes(alpha, costL2), color='red') +
      geom_smooth(aes(alpha, costL2), color='blue')
  }
  return(plot_object)
}

# Create lists of plot objects for p = 1 and p = 2
plotsL1 = lapply(as.list(lambda_vec), get_plot, p=1)
plotsL2 = lapply(as.list(lambda_vec), get_plot, p=2)

# Call Rmisc::multiplot() on the plot lists generated above
multiplot(plotlist = plotsL1, cols = 2)
multiplot(plotlist = plotsL2, cols = 2)

# Getting started with glmnet()

# First, let's load in the speed dating data frame
df = read.csv('../data/speed-dating-simple.csv')
View(df)

# Filter out the gender column and all of the trait columns except attr_o
activities_scaled = scale(dplyr::select(df, sports:yoga))
View(activities_scaled)
attr_o = df[,"attr_o"]

# Run glmnet() with activities_scaled as x and attr_o as y
# NOTE: The alpha parameter in glmnet() is NOT the power p!
# Rather, it is a mixing parameter.  
# alpha = 1 ==> L1 (lasso) regularization
# alpha = 0 ==> L2 (ridge) regularization
glmfitL1 = glmnet(activities_scaled, attr_o, alpha=1)
glmfitL1[['lambda']]
glmfitL2 = glmnet(activities_scaled, attr_o, alpha=0)
glmfitL2[['lambda']]

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}


get_rmses = function(fit, features, target) {
  if (length(target) != nrow(features)) stop('Arguments "features" and "target" must be of equal length')
  lambdas = fit[['lambda']]
  # Initialize a vector which we'll populate with the RMSE between the 
  # target and predicted data for each lambda in lambdas
  rmses = rep(NA, length(lambdas))
  # Iterate through the lambda values obtained from fit
  for (i in 1:length(lambdas)) {
    predictions = predict(fit, features, s=lambdas[i])
    rmses[i] = rmse(target, predictions)
  }
  return(rmses)
}

rmsesL1 = get_rmses(glmfitL1, activities_scaled, attr_o)
rmsesL2 = get_rmses(glmfitL2, activities_scaled, attr_o)

# Plot the RMSEs calculated above
dfL1 = data.frame(lambda = glmfitL1[['lambda']], rmse = rmsesL1)
dfL2 = data.frame(lambda = glmfitL2[['lambda']], rmse = rmsesL2)
ggplot() + 
  geom_smooth(data = dfL1, aes(x = lambda, y = rmse), color="red") +
  geom_point(data = dfL1, aes(x = lambda, y = rmse), color="blue")  
ggplot() + 
  geom_smooth(data = dfL2, aes(x = lambda, y = rmse), color="red") +
  geom_point(data = dfL2, aes(x = lambda, y = rmse), color="blue")  



# Generate cross-validated glmnet() fits with cv.glmnet()
cv_glmfitL1 = cv.glmnet(activities_scaled, attr_o, alpha=1)
cv_glmfitL2 = cv.glmnet(activities_scaled, attr_o, alpha=0)

# Extract the lambdas from the fits generated above which minimize 
# (w.r.t. the lambas tested) the RMSE estimates
cv_glmfitL1[['lambda.min']]
cv_glmfitL2[['lambda.min']]

# Plot the RMSE estimates against the lambdas tested
cv_glmfitL1_df = data.frame(lambda = cv_glmfitL1[['lambda']], 
                            rmse   = sqrt(cv_glmfitL1[['cvm']]))
cv_glmfitL2_df = data.frame(lambda = cv_glmfitL2[['lambda']], 
                            rmse   = sqrt(cv_glmfitL2[['cvm']]))

ggplot(cv_glmfitL1_df, aes(lambda, rmse)) + 
  geom_point(color="blue")  +
  geom_smooth(color="red", size=0.5)

ggplot(cv_glmfitL2_df, aes(lambda, rmse)) + 
  geom_point(color="blue") +
  geom_smooth(color="red", size=0.5) +
  scale_x_continuous(limits = c(0, 5)) + 
  scale_y_continuous(limits = c(1.11, 1.16))



# n-fold cross-validation
nfold_cv = function(df, n_folds, to_predict='attr_o') {
  # Total number of rows in the df
  n_rows = nrow(df)
  # Find the number of rows in each bin
  # Lower bound
  bin_rows_lb = floor(n_rows/n_folds)
  # Upper bound
  bin_rows_ub = ceiling(n_rows/n_folds)
  # Cycle through the number of folds/bins until we have the 
  # total number of rows of the df, then sort the result
  bin_vec = sort(head(rep(1:n_folds, bin_rows_ub), n_rows))
  # Shuffle the rows of the df
  shuff_rows = sample(1:n_rows, n_rows, replace = FALSE)
  # Initialize a list with n_folds elements, each of which will 
  # then be populated with indices extraced from shuff_rows 
  bins = vector('list',n_folds)
  # Iterate through each fold/bin and populate said bin
  for (i in 1:n_folds) bins[[i]] = df[shuff_rows[bin_vec == i],]
  # Initialize a vector which will contain predicted values 
  # of the to_predict feature
  predictions = rep(NA, n_rows)
  # Initialize a counter for populating the predictions vector
  j = 0
  # Iterate through each bin again
  # This time, extract one bin to use as a test set, and combine 
  # the rest to use as a cross-validation set
  for (i in 1:n_folds) {
    # Extract the current bin
    test = bins[[i]]
    # Combine the other bins into a cross-validation set
    cv = do.call(rbind, bins[-i])
    # Run the linear model on the cv set
    arg1 = paste(to_predict,"~ .") # Default separation (' ')
    linear_fit = lm(arg1, cv)
    # Populate the predictions vector with predictions 
    # based on the test set
    predictions[(j+1):(j+nrow(test))] = predict(linear_fit, test)
    # Update the counter
    j = j+nrow(test)
  }
  # Return the RMSE between predictions and the actual (shuffled) 
  # to_predict column of df
  return(rmse(predictions, df[shuff_rows, to_predict]))
}



# Backward stepwise linear regression
# Again, remove any features you won't be using BEFORE calling 
# this function
backward_step = function(df, n_folds=10, to_predict='attr_o') {
  # Find the number of feature columns used to predict the 
  # to_predict feature
  n_features = ncol(df) - 1
  # Create a vector for number of features removed
  removed_count = (1:n_features) - 1
  # Initialize a vector to be populated by the n-fold 
  # cross-validated RMSE value for each feature removed
  rmse_vec = rep(NA, n_features)
  # Initialize a vector which will track which feature is 
  # removed before running cross-validation
  removed_features = rep(NA, n_features)
  # Iterate through the features
  # At each iteration, remove the feature with the highest 
  # p-value, i.e. the lowest correlation to the to_predict 
  # feature
  for (i in 1:n_features) {
    # Run n-fold cross-validation ONCE 
    rmse_vec[i] = nfold_cv(df, n_folds, to_predict='attr_o')
    # Run a linear fit on the ENTIRE df
    arg1 = paste(to_predict,"~ .") # Default separation (' ')
    linear_fit = lm(arg1, df)
    # Extract the p-values, find the feature with the greatest 
    # p-value, and remove it from the df
    lf_coef_df = data.frame(summary(linear_fit)['coefficients'])
    pvals = lf_coef_df[,4]
    features = rownames(lf_coef_df)
    bad_feature = features[pvals == max(pvals)]
    removed_features[i] = bad_feature
    df = df[,(colnames(df) != bad_feature)]
  }
  # Remove the last element of the removed_features vector, 
  # then append an NA to the beginning of it
  removed_features = c(NA, head(removed_features, -1))
  # Make and return a data frame with all three of the 
  # vectors we created as columns
  return(data.frame(list(num_features_removed=removed_count, 
                         last_feature_removed=removed_features, 
                         rmse=rmse_vec)))
}

View(dfx)
pred_cols
# FOR REFERENCE
# Loop over each column which we're predicting
for (col in pred_cols) {
  dfx = dplyr::select(df, one_of(col), sports:yoga)
  arg1 = paste(col,"~ .") # Default separation (' ')
  model_init = lm(arg1, dfx)
  model = formula(model_init)
  step_reg = step(model_init, model, direction="backward")
  back_coeffs_list[[col]] = data.frame(step_reg['coefficients'])
}

col = 'attr_o'
str(step_reg)
res = step_reg[['residuals']]
res
nrow(df)
# Predict extracts residuals automagically from the model (step_reg)
step_rmse = sqrt(mean(res^2))

all( predict(step_reg, dfx) == step_reg[['fitted.values']] )



# Call rmse on the results of predict, compared to the original column (attr_o)
rmse(predict(step_reg, df), df[['attr_o']])

# Run glmnet twice, once for L1, once for L2

three_way_n_fold = function(df, n_folds=10, to_predict='attr_o') {
  # Filter out the columns we won't be using
  df = dplyr::select(df, one_of(to_predict), sports:yoga)
  # Total number of rows in the df
  n_rows = nrow(df)
  # Find the number of rows in each bin
  # Upper bound
  bin_rows_ub = ceiling(n_rows/n_folds)
  # Cycle through the number of folds/bins until we have the 
  # total number of rows of the df, then sort the result
  bin_vec = sort(head(rep(1:n_folds, bin_rows_ub), n_rows))
  # Shuffle the rows of the df
  shuff_rows = sample(1:n_rows, n_rows, replace = FALSE)
  # Initialize lists with n_folds elements, each of which will 
  # then be populated with indices extraced from shuff_rows 
  bins = vector('list',n_folds)
  # Iterate through each fold/bin and populate said bin
  for (i in 1:n_folds) bins[[i]] = df[shuff_rows[bin_vec == i],]
  # Initialize vectors which will contain values of the to_predict 
  # feature predicted by backward stepwise linear regression
  pred_back = rep(NA, n_rows)
  pred_glm1 = rep(NA, n_rows)
  pred_glm2 = rep(NA, n_rows)
  # Initialize a counter for populating the predictions vectors
  j = 0
  # Iterate through each bin again
  # This time, extract one bin to use as a test set, and combine 
  # the rest to use as a cross-validation set
  for (i in 1:n_folds) {
    # Extract the current bin
    test = bins[[i]]
    # Combine the other bins into a cross-validation set
    cv = do.call(rbind, bins[-i])
    # Scale the features columns of the cross-validation set
    cv_actvts_scaled = scale(dplyr::select(cv, sports:yoga))
    # Extract the scaling parameters from above, so we can 
    # apply the same scaling to the test set
    center    = attributes(cv_actvts_scaled)[['scaled:center']]
    scale_fac = attributes(cv_actvts_scaled)[['scaled:scale']]
    # Scale the test set using the parameters from above
    test_actvts_scaled = 
      scale(dplyr::select(test, sports:yoga), center=center, scale=scale_fac)
    # Three calls for this fold, one for each model type
    # Train a linear model on the cross-validation set, then 
    # test it on the test set with backward stepwise regression
    arg1 = paste(to_predict,"~ .") # Default separation (' ')
    model_init = lm(arg1, cv)
    model = formula(model_init)
    step_reg = step(model_init, model, direction="backward")
    pred_back[(j+1):(j+nrow(test))] = predict(step_reg, test)
    # Train regularized models on the scaled cross-validation set, 
    # with both L1 and L2 regularization, then populate the appropiate 
    # prediction vectors
    cv_glmfitL1 = cv.glmnet(cv_actvts_scaled, cv[[to_predict]], alpha=1)
    cv_glmfitL2 = cv.glmnet(cv_actvts_scaled, cv[[to_predict]], alpha=0)
    pred_glm1[(j+1):(j+nrow(test))] = 
      predict(cv_glmfitL1, test_actvts_scaled, s=cv_glmfitL1[['lambda.min']])
    pred_glm2[(j+1):(j+nrow(test))] = 
      predict(cv_glmfitL2, test_actvts_scaled, s=cv_glmfitL2[['lambda.min']])
    # Update the counter
    j = j+nrow(test)
  }
  # Return the RMSE between predictions and the actual (shuffled) 
  # to_predict column of df for backward stepwise linear regression 
  # and glmnet regression with both L1 and L2 regularization
  rmseBS = rmse(pred_back, df[shuff_rows, to_predict])
  rmseL1 = rmse(pred_glm1, df[shuff_rows, to_predict])
  rmseL2 = rmse(pred_glm2, df[shuff_rows, to_predict])
  return(c(rmseBS=rmseBS, rmseL1=rmseL1, rmseL2=rmseL2))
}



View(df)
# Run the three-way n-fold function above with default parameters
rmses = three_way_n_fold(df, n_fold=10)


# Now let's use the Caret package to find the optimum coefficient 
# and regularization parameter.  Then let's see how the resulting 
# RMSE compares to the values obtained above.

# Set grid of parameter values to search over
param_grid = expand.grid(alpha = 1:10 * 0.1, 
                         lambda = 10^seq(-4, 0, length.out=10))
# Set 10-fold cross validation repeated 3x
control = trainControl(method="repeatedcv", number=10, repeats=3, verboseIter=TRUE)
# Search over the grid
caret_fit = train(x=scale(dplyr::select(df, sports:yoga)), 
                  y=df[['attr_o']], 
                  method="glmnet", 
                  tuneGrid=param_grid, 
                  trControl=control)
# View the optimal values of alpha and lambda
caret_fit$bestTune
# View the cross-validated RMSE estimates
caret_fit$results$RMSE







