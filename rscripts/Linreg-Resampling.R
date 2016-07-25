# Sven Chilton Jimin (Jimmy) Yu
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')
library(dplyr)
library(ggplot2)

# Read in the speed dating dataset
df = read.csv('../data/speed-dating-simple.csv')
df
View(df)

info = read.delim('../data/speed-dating-info.txt')
info
View(info)

# Extract only the males from df
df = df[df[['gender']] == 1,]
View(df)




linear_fit = lm(attr_o ~ .-gender-attr_o-sinc_o-intel_o-fun_o-amb_o, df)
linear_fit = lm(attr_o ~ ., select(df, attr_o, sports:yoga))
summary(linear_fit)

# Function for splitting the data frame df into a training set and a test set
split_data = function(df, train_frac=0.5) {
  train_size = floor(train_frac*nrow(df))
  shuff_rows = sample(1:nrow(df), nrow(df), replace = FALSE)
  train_ind  = head(shuff_rows, train_size)
  test_ind   = tail(shuff_rows, -train_size) 
  # Return a named list with the training set as the first element
  # and the test set as the second
  l = vector("list", 2)
  names(l) = c('train','test')
  l[['train']] = df[train_ind,]
  l[['test']]  = df[test_ind,]
  return(l)
}

test_df
testl = split_data(test_df, train_frac=0.3)
testl$train
testl$test

# Function which runs a linear model on a training set 
# and then uses that fit on the test set to predict a 
# feature (in this case attractiveness?)
# Make sure that you've filtered out the features which 
# you're not using in the fit (either as a predictor or 
# predicted variable) before calling this
split_predict = function(df, training_frac=0.5, to_predict='attr_o') {
  # First split the data into training and test sets
  l = split_data(df, train_frac=0.5)
  train = l[['train']]
  test  = l[['test']]
  # Run the linear model on the training set
  arg1 = paste(to_predict,"~ .") # Default separation (' ')
  linear_fit = lm(arg1, train)
  # Return a named list with the predictions acting on the 
  # training set as the first element and the predictions 
  # acting on the test set as the second
  lp = vector("list", 2)
  names(lp) = c('train','test')
  lp[['train']] = predict(linear_fit, train)
  lp[['test']]  = predict(linear_fit, test)
  return(lp)
}

View(df)
filtered_df = select(df, attr_o, sports:yoga)
View(filtered_df)
lp = split_predict(filtered_df, training_frac=0.5, to_predict='attr_o')
lp
names(lp[['train']])
lp[['test']]
filtered_df[names(lp[['train']]),'attr_o']


# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}

# Take in a(n appropriately filtered) df, (split it into training and test 
# sets, train a linear fit on the training set and use it to predict a 
# given feature in both the training and test sets, compute the RMSE 
# between the true and predicted values of the given feature for the 
# training and test sets) n times, and output the RMSEs for each iteration 
# for the training and test data
run_predict = function(df, training_frac=0.5, to_predict='attr_o', n=100) {
  # Initialize vectors which will later contain the training and test 
  # RMSEs for each iteration of the for loop
  train_rmse = rep(NA, n)
  test_rmse  = rep(NA, n)
  for (i in 1:n) {
    # Split and predict the data
    lp = split_predict(df, training_frac=0.5, to_predict='attr_o')
    train_predict = lp[['train']]
    test_predict  = lp[['test']]
    # Extract the rows from the to_predict column of the input df which 
    # correspond to the training and test sets (in the proper order)
    train_actual = df[names(train_predict),to_predict]
    test_actual  = df[names(test_predict), to_predict]
    # Compute the train and test RMSEs and populate the appropriate 
    # vectors 
    train_rmse[i] = rmse(train_actual, train_predict)
    test_rmse[i]  = rmse(test_actual,  test_predict)
  }
  # Make a data frame out of the RMSE vectors and return it
  rmse_df = data.frame(list(train_rmse=train_rmse, test_rmse=test_rmse))
  return(rmse_df)
}

# Hell with it.  Let's remove the columns of df which we're not using.  
# Then, let's calculate the training and test RMSEs for 100 runs
df = select(df, attr_o, sports:yoga)
View(df)
rmse_df = run_predict(df, training_frac=0.5, to_predict='attr_o', n=100)
View(rmse_df)

# Plot the distributions of the RMSEs calculated above
ggplot() + 
  geom_density(aes(rmse_df['train_rmse']), fill='red',  alpha=0.5) +
  geom_density(aes(rmse_df['test_rmse']),  fill='blue', alpha=0.5)

# Calculate the mean and standard error (standard deviation divided by 
# root n, where n is the number of samples) of both the training and  
# test RMSEs
train_rmse_mean = mean(rmse_df[['train_rmse']])
test_rmse_mean  = mean(rmse_df[['test_rmse']])
train_rmse_serr = sd(rmse_df[['train_rmse']]) / sqrt(nrow(rmse_df))
test_rmse_serr  = sd(rmse_df[['test_rmse']])  / sqrt(nrow(rmse_df))

# As expected, the test set has a somewhat greater mean and standard 
# error in its RMSE values than the training set does.


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

View(df)
n_folds = 10
dummy_rmse = nfold_cv(df, n_folds, to_predict='attr_o')


# Function for running n-fold cross-validation k times
run_nfold_cv = function(df, n_folds, to_predict='attr_o', k=100) {
  # Initialize a vector of RMSEs
  rmse_vec = rep(NA, k)
  # Run n-fold cross-validation k times to populate the 
  # RMSE vector
  for (i in 1:k) rmse_vec[i] = nfold_cv(df, n_folds, to_predict)
  return(rmse_vec)
}

rmse_cv_n2_k100  = run_nfold_cv(df, n_folds=2,  to_predict='attr_o', k=100)
rmse_cv_n10_k100 = run_nfold_cv(df, n_folds=10, to_predict='attr_o', k=100)

# Compute the means and standard errors of the two rmse vectors above
mean_n2_k100  = mean(rmse_cv_n2_k100)
mean_n10_k100 = mean(rmse_cv_n10_k100)
serr_n2_k100  = sd(rmse_cv_n2_k100)  / sqrt(length(rmse_cv_n2_k100))
serr_n10_k100 = sd(rmse_cv_n10_k100) / sqrt(length(rmse_cv_n10_k100))
  
# According to this, 10-fold cross-validation on our data set yields 
# roughly the same mean as 2-fold, but has a significantly better 
# standard error


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

View(df)

back_rmse_df = backward_step(df, n_folds=10, to_predict='attr_o')
View(back_rmse_df)

# Plot of RMSE vs. num_features_removed as calculated above
ggplot(back_rmse_df, aes(num_features_removed, rmse)) + 
  geom_point(color='red') +
  geom_smooth(color='blue')

# It appears that removing 7 or 8 features from our male-filtered 
# speed-dating data frame results in the lowest error, as computed 
# through (in this case) 10-fold cross-validation.  However, the 
# in this case, the differences in RMSE are slight.


# All right, now we can use R's built-in step() function

# Reload the original df
df = read.csv('../data/speed-dating-simple.csv')

# Filter out the females
df = df[df[['gender']] == 1,]

# Columns which we're predicting
# attr_o:amb_o, 2:6, attr_o, sinc_o, intel_o, fun_o, amb_o
pred_cols = c('attr_o', 'sinc_o', 'intel_o', 'fun_o', 'amb_o')

# Initialize a list with each element corresponding to a 
# column which we're predicting
# We will populate each list with a coefficient data frame 
# from the last model in the step function
back_coeffs_list = vector('list',length(pred_cols))
names(back_coeffs_list) = pred_cols

# Loop over each column which we're predicting
for (col in pred_cols) {
  dfx = select(df, one_of(col), sports:yoga)
  arg1 = paste(col,"~ .") # Default separation (' ')
  model_init = lm(arg1, dfx)
  model = formula(model_init)
  step_reg = step(model_init, model, direction="backward")
  back_coeffs_list[[col]] = data.frame(step_reg['coefficients'])
}

back_coeffs_list[['attr_o']]
data.frame(step_reg['coefficients'])

# Bootstrapping

# Intentionally bad bootstrapping function
# Even though this is intentionally bad, make sure to filter out the columns 
# we won't be using before calling this function
bootstrap_bad = function(df, approach, to_predict='attr_o', num_samples=100) {
  # Make sure the approach argument is set properly
  if ((approach != 1) & (approach != 2)) stop('Argument "approach" must be either 1 or 2')
  # Initialize a vector which will contain the RMSE value of each sample
  rmse_vec = rep(NA, num_samples)
  # Carry out the bootstrapping
  for (i in 1:num_samples) {
    # Collect a random sample of the rows WITH REPLACEMENT
    # It will thus be possible to obtain the same row more than once 
    # in the sample
    boot_rows = sample(1:nrow(df), nrow(df), replace = TRUE)
    boot_df   = df[boot_rows,]
    # Approach 1: use boot_df as the training set and df as the test set
    # Approach 2: use df as the training set and boot_df as the test set
    if (approach == 1) {
      # Run a linear fit on the ENTIRE bootstrapped df
      arg1 = paste(to_predict,"~ .") # Default separation (' ')
      linear_fit = lm(arg1, boot_df)
      # Predict the feature using the ENTIRE original (filtered) df
      predictions = predict(linear_fit, df)
      # Take the RMSE between the predicted feature values and the 
      # original feature values and use it the populate rmse_vec
      rmse_vec[i] = rmse(predictions, df[,to_predict])
    } else if (approach == 2) {
      # Run a linear fit on the ENTIRE original (filtered) df
      arg1 = paste(to_predict,"~ .") # Default separation (' ')
      linear_fit = lm(arg1, df)
      # Predict the feature using the ENTIRE bootstrapped df
      predictions = predict(linear_fit, boot_df)
      # Take the RMSE between the predicted feature values and the 
      # original feature values and use it the populate rmse_vec
      rmse_vec[i] = rmse(predictions, boot_df[,to_predict])
    }
  }
  return(mean(rmse_vec))
}

View(df)
# Filter the df: remove the gender column and all the trait columns 
# except attractiveness (attr_o)
# Hmm, somehow I appear to have introduced a namespace conflict
# Fortunately, making it explicit that I meant to use select() 
# from the dplyr package resolved it
df = dplyr::select(df, attr_o, sports:yoga)

# Run the bad bootstrap function on the filtered df
# Take the default num_samples = 100
mean_rmse_1 = bootstrap_bad(df, approach=1, to_predict='attr_o', num_samples=100)
mean_rmse_2 = bootstrap_bad(df, approach=2, to_predict='attr_o', num_samples=100)


# Backward stepwise linear regression with bad bootstrapping
# Again, remove any features you won't be using BEFORE calling 
# this function
backward_step_2 = function(df, to_predict='attr_o', n_folds=10, num_samples=100) {
  # Find the number of feature columns used to predict the 
  # to_predict feature
  n_features = ncol(df) - 1
  # Create a vector for number of features removed
  removed_count = (1:n_features) - 1
  # Initialize vectors to be populated by the RMSE values 
  # for each feature removed via cross-validation, a linear 
  # fit of the entire df, and (bad) bootstrapping via the 
  # two approaches
  cv_rmse_vec = rep(NA, n_features)
  lf_rmse_vec = rep(NA, n_features)
  b1_rmse_vec = rep(NA, n_features)
  b2_rmse_vec = rep(NA, n_features)
  # Initialize a vector which will track which feature is 
  # removed before running cross-validation
  removed_features = rep(NA, n_features)
  # Iterate through the features
  # At each iteration, remove the feature with the highest 
  # p-value, i.e. the lowest correlation to the to_predict 
  # feature
  for (i in 1:n_features) {
    # Run n-fold cross-validation ONCE 
    cv_rmse_vec[i] = nfold_cv(df, n_folds, to_predict='attr_o')
    # Run a linear fit on the ENTIRE df
    arg1 = paste(to_predict,"~ .") # Default separation (' ')
    linear_fit = lm(arg1, df)
    # Predict the to_predict feature using the linear fit
    predictions = predict(linear_fit, df)
    lf_rmse_vec[i] = rmse(predictions, df[,to_predict])
    # Run the bad bootstrap method with both approaches
    b1_rmse_vec[i] = bootstrap_bad(df, approach=1, to_predict=to_predict, 
                                   num_samples=num_samples)
    b2_rmse_vec[i] = bootstrap_bad(df, approach=2, to_predict=to_predict, 
                                   num_samples=num_samples)
    # Returning to the linear fit, 
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
                         cv_rmse=cv_rmse_vec,
                         lf_rmse=lf_rmse_vec,
                         b1_rmse=b1_rmse_vec,
                         b2_rmse=b2_rmse_vec)))
}

# Call the backward step function from above (using bad bootstrapping )
bs2_df = backward_step_2(df, to_predict='attr_o', n_folds=10, num_samples=100)
View(bs2_df)

# Let's plot the RMSEs calculated above vs. features removed
ggplot() + 
  geom_point( data=bs2_df, aes(num_features_removed, cv_rmse), color='red') +
  geom_smooth(data=bs2_df, aes(num_features_removed, cv_rmse), color='red') +
  geom_point( data=bs2_df, aes(num_features_removed, lf_rmse), color='blue') +
  geom_smooth(data=bs2_df, aes(num_features_removed, lf_rmse), color='blue') +
  geom_point( data=bs2_df, aes(num_features_removed, b1_rmse), color='green') +
  geom_smooth(data=bs2_df, aes(num_features_removed, b1_rmse), color='green') +
  geom_point( data=bs2_df, aes(num_features_removed, b2_rmse), color='purple') +
  geom_smooth(data=bs2_df, aes(num_features_removed, b2_rmse), color='purple')
  

# Now let's write a good bootstrapping function which modifies Approach 1 
# of the bad bootstrapping function in that it only makes predictions on 
# rows which were not part of the randomly sampled set
bootstrap_good = function(df, to_predict='attr_o', num_samples=100) {
  # Initialize a vector which will contain the RMSE value of each sample
  rmse_vec = rep(NA, num_samples)
  # Carry out the bootstrapping
  for (i in 1:num_samples) {
    # Collect a random sample of the rows WITH REPLACEMENT
    # It will thus be possible to obtain the same row more than once 
    # in the sample
    boot_rows = sample(1:nrow(df), nrow(df), replace = TRUE)
    boot_df   = df[boot_rows,]
    # Approach 1: use boot_df as the training set and df as the test set
    # Approach 2: use df as the training set and boot_df as the test set
    # Run a linear fit on the ENTIRE bootstrapped df
    arg1 = paste(to_predict,"~ .") # Default separation (' ')
    linear_fit = lm(arg1, boot_df)
    # Predict the feature using the rows of the original (filtered) df 
    # which are not contained in boot_rows
    predict_rows = setdiff(1:nrow(df), boot_rows)
    predictions = predict(linear_fit, df[predict_rows,])
    # Take the RMSE between the predicted feature values and the 
    # original feature values and use it the populate rmse_vec
    rmse_vec[i] = rmse(predictions, df[predict_rows,to_predict])
  }
  return(mean(rmse_vec))
}

# Test the good bootstrap function with the default arguments
bootstrap_good(df)



# Backward stepwise linear regression with good bootstrapping
# Again, remove any features you won't be using BEFORE calling 
# this function
backward_step_3 = function(df, to_predict='attr_o', n_folds=10, num_samples=100) {
  # Find the number of feature columns used to predict the 
  # to_predict feature
  n_features = ncol(df) - 1
  # Create a vector for number of features removed
  removed_count = (1:n_features) - 1
  # Initialize vectors to be populated by the RMSE values 
  # for each feature removed via cross-validation, a linear 
  # fit of the entire df, and (good) bootstrapping 
  cv_rmse_vec = rep(NA, n_features)
  lf_rmse_vec = rep(NA, n_features)
  bs_rmse_vec = rep(NA, n_features)
  # Initialize a vector which will track which feature is 
  # removed before running cross-validation
  removed_features = rep(NA, n_features)
  # Iterate through the features
  # At each iteration, remove the feature with the highest 
  # p-value, i.e. the lowest correlation to the to_predict 
  # feature
  for (i in 1:n_features) {
    # Run n-fold cross-validation ONCE 
    cv_rmse_vec[i] = nfold_cv(df, n_folds, to_predict='attr_o')
    # Run a linear fit on the ENTIRE df
    arg1 = paste(to_predict,"~ .") # Default separation (' ')
    linear_fit = lm(arg1, df)
    # Predict the to_predict feature using the linear fit
    predictions = predict(linear_fit, df)
    lf_rmse_vec[i] = rmse(predictions, df[,to_predict])
    # Run the (good) bootstrap method
    bs_rmse_vec[i] = 
      bootstrap_good(df, to_predict=to_predict, num_samples=num_samples)
    # Returning to the linear fit, 
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
                         cv_rmse=cv_rmse_vec,
                         lf_rmse=lf_rmse_vec,
                         bs_rmse=bs_rmse_vec)))
}

# Call the backward step function from above (using good bootstrapping )
bs3_df = backward_step_3(df, to_predict='attr_o', n_folds=10, num_samples=100)
View(bs3_df)

# Let's plot the RMSEs calculated above vs. features removed
ggplot() + 
  geom_point( data=bs3_df, aes(num_features_removed, cv_rmse), color='red') +
  geom_smooth(data=bs3_df, aes(num_features_removed, cv_rmse), color='red') +
  geom_point( data=bs3_df, aes(num_features_removed, lf_rmse), color='blue') +
  geom_smooth(data=bs3_df, aes(num_features_removed, lf_rmse), color='blue') +
  geom_point( data=bs3_df, aes(num_features_removed, bs_rmse), color='green') +
  geom_smooth(data=bs3_df, aes(num_features_removed, bs_rmse), color='green')


# Variance of parameter estimates
# Imagine financial assets X and Y.  You invest a fraction alpha in asset X 
# and 1-alpha in asset Y.  Assuming X and Y are uncorrelated, tt turns out 
# that the alpha which minimizes the variance of your returns is 
# sigmay^2/(sigmax^2 + sigmay^2), where sigmax is the standard deviation of 
# X and sigmax is the standard deviation of Y

# Function for calculating alpha given two vectors (or lists?) of the 
# same length
calc_alpha = function(x,y) {
  # Make sure x and y are the same length
  if (length(x) != length(y)) stop('x and y must be of equal length')
  return(var(y)/(var(x) + var(y)))
}


# For given standard deviations sdX and sdY and a mean mu, create 
# distributions X and Y with rnorm and num_samples number of samples.  
# Then, iterating through num_boot, we create a bootstrapped sample 
# of indices, filter both X and Y with those indices, and then 
# calculate alpha for X and Y thus filtered.  We populate a vector 
# alphas with each alpha.
gen_alphas = function(sdX, sdY, mu=10, num_samples=100, num_boot=1000) {
  # Generate the X and Y distributions
  X = rnorm(num_samples, mean=mu, sd=sdX)
  Y = rnorm(num_samples, mean=mu, sd=sdY)
  # Initialize and then populate the alphas vector
  alphas = rep(NA, num_boot)
  for (i in 1:num_boot) {
    #boot_rows = sample(1:num_samples, num_samples, replace = TRUE)
    #alphas[i] = calc_alpha(X[boot_rows], Y[boot_rows])
    Xboot = X[sample(1:num_samples, num_samples, replace = TRUE)]
    Yboot = Y[sample(1:num_samples, num_samples, replace = TRUE)]
    alphas[i] = calc_alpha(Xboot, Yboot)
  }
  return(alphas)
}

# Run gen_alphas with (sdX, sdY) = (1,2), (1,3), (1,4)
alphas_12 = gen_alphas(1, 2, mu=10, num_samples=100, num_boot=1000)
alphas_13 = gen_alphas(1, 3, mu=10, num_samples=100, num_boot=1000)
alphas_14 = gen_alphas(1, 4, mu=10, num_samples=100, num_boot=1000)

# Make a df from the alphas vectors
alphas_df = data.frame(list(alphas_12=alphas_12, 
                            alphas_13=alphas_13,
                            alphas_14=alphas_14))

# Plot histograms of the alpha vectors calculated above
ggplot(alphas_df) +
  geom_histogram(aes(alphas_12), fill='red',   alpha='0.5', bins=100) +
  geom_histogram(aes(alphas_13), fill='blue',  alpha='0.5', bins=100) +
  geom_histogram(aes(alphas_14), fill='green', alpha='0.5', bins=100) 
  














