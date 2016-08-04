# Sven Chilton
# Signal Data Science Cohort 3

# Set the working directory
setwd('~/GitHub/signal-work/rscripts/')

# Load the appropriate packages
library('psych')
library('dplyr')
library('softImpute')
library('DAAG')
library('corrplot')
library('pROC')
library('dummies')
library('ggplot2')

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}

# Load all the data
melted = read.csv('../data/melted.csv')
questionKey = read.csv('../data/questionKey.csv')
demographics = read.csv('../data/demographics.csv')

# Convert the user and question ID columns (uid and qid) in each of 
# the three dfs we just loaded into numerics, after eliminating the 
# u and q prefixes
# First eliminate the u's and q's from the uid and qid columns
# This converts them to characters
melted$uid = gsub('u','',melted$uid)
melted$qid = gsub('q','',melted$qid)
questionKey$qid  = gsub('q','',questionKey$qid)
demographics$uid = gsub('u','',demographics$uid)
# Convert the columns back to numerics
melted$uid = as.numeric(melted$uid)
melted$qid = as.numeric(melted$qid)
questionKey$qid  = as.numeric(questionKey$qid)
demographics$uid = as.numeric(demographics$uid)

# Restrict ourselves to male users
# For ease, I'm going to extract only rows where 
# demographics$d_gender == 'Man'
# First remove any rows where demographics$d_gender is NA
# It took me forever to figure out that leaving them in 
# is problematic.
demographics = demographics[!is.na(demographics$d_gender),]
demographics = demographics[demographics$d_gender == 'Man',]
melted = melted[melted$uid %in% demographics$uid,]

# Convert the appropriate columns to factors
# That way, when we convert them back to numerics, they'll 
# be in the right order
melted$uid = as.factor(melted$uid)
melted$qid = as.factor(melted$qid)
questionKey$qid  = as.factor(questionKey$qid)
demographics$uid = as.factor(demographics$uid)
# Convert the columns back to numerics
# The ranges of uid and qid should now be 1 to num(uid) and 
# 1 to num(qid)
melted$uid = as.numeric(melted$uid)
melted$qid = as.numeric(melted$qid)
questionKey$qid  = as.numeric(questionKey$qid)
demographics$uid = as.numeric(demographics$uid)


# Determine how many users are represented
max_uid_m = max(melted$uid)
min_uid_m = min(melted$uid)
max_uid_d = max(demographics$uid)
min_uid_d = min(demographics$uid)
num_uid_m = length(unique(melted$uid))
num_uid_d = length(unique(demographics$uid))
print(paste0('The maximum uid in "melted" is ',max_uid_m))
print(paste0('The minimum uid in "melted" is ',min_uid_m))
print(paste0('The maximum uid in "demographics" is ',max_uid_d))
print(paste0('The minimum uid in "demographics" is ',min_uid_d))
print(paste0(num_uid_m,' users are represented in "melted"'))
print(paste0(num_uid_d,' users are represented in "demographics"'))
print(paste0('all(melted$uid %in% demographics$uid): ',
              all(melted$uid %in% demographics$uid)))
print(paste0('all(demographics$uid %in% melted$uid): ',
              all(demographics$uid %in% melted$uid)))

# Determine how many questions are represented
max_qid_m = max(melted$qid)
min_qid_m = min(melted$qid)
max_qid_q = max(questionKey$qid)
min_qid_q = min(questionKey$qid)
num_qid_m = length(unique(melted$qid))
num_qid_q = length(unique(questionKey$qid))
print(paste0('The maximum qid in "melted" is ',max_qid_m))
print(paste0('The minimum qid in "melted" is ',min_qid_m))
print(paste0('The maximum qid in "questionKey" is ',max_qid_q))
print(paste0('The minimum qid in "questionKey" is ',min_qid_q))
print(paste0(num_qid_m,' questions are represented in "melted"'))
print(paste0(num_qid_q,' questions are represented in "questionKey"'))
print(paste0('all(melted$qid %in% questionKey$qid): ',
             all(melted$qid %in% questionKey$qid)))
print(paste0('all(questionKey$qid %in% melted$qid): ',
             all(questionKey$qid %in% melted$qid)))



# Split the melted data into a training and a test set
# Code reuse FTW!
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

# Perform the split, with 80% of the melted data as training data
# Set the seed on the same line as the split call for reproducibility
set.seed(1); melted_list = split_data(melted, train_frac=0.8)
melted_train = melted_list$train
melted_test  = melted_list$test

# Some of the users and questions may have been eliminated in the 
# training set, so we'll create a fake question which every real 
# user has answered and a fake user who has answered every real 
# question

# Take the overall mean of the value column of the entire melted df
# The fake user will answer every question with this value (or rather,
# would do so if it were possible) and every real user will answer 
# the fake question with this value
q_mean = mean(melted$value)

# Fake user
# Since we did our jobs properly and max_uid_d == max_uid_m, 
# set max_uid = max_uid_d
max_uid  = max_uid_d
fake_uid = max_uid+1
fake_user_df = data.frame(uid = fake_uid,
                          qid = unique(questionKey$qid),
                          value = q_mean)

# Fake question
# Since we did our jobs properly and max_qid_q == max_qid_m, 
# set max_qid = max_qid_q
max_qid  = max_qid_q
fake_qid = max_qid+1
fake_question_df = data.frame(uid = unique(demographics$uid),
                              qid = fake_qid,
                              value = q_mean)

# Perturb the fake values slightly with a normally distributed 
# error term with mean = 0 and sd = 0.01
fake_user_df$value = 
  fake_user_df$value + rnorm(nrow(fake_user_df), mean=0, sd=0.01)
fake_question_df$value = 
  fake_question_df$value + rnorm(nrow(fake_question_df), mean=0, sd=0.01)

# rbind() the fake dfs to the training set
melted_train = rbind(melted_train, fake_user_df, fake_question_df)

# Create a sparse matrix with the uids as rows and the qids as columns, 
# using the Incomplete() function from the softImpute package.
uid_by_qid = Incomplete(melted_train$uid, 
                        melted_train$qid, 
                        melted_train$value)

# Begin collaborative filtering
uid_by_qid = biScale(uid_by_qid, maxit = 25, trace = TRUE)
lam0 = lambda0(uid_by_qid)
lam_vec = exp(seq(log(lam0), log(1), length.out = 20))

# Initialize a data frame results with three columns: lambda, rank, and 
# rmse, where the lambda column is equal to the previously generated 
# sequence of values of lambda to test. Initialize a list fits as well 
# to store the results of alternating least squares for each value of 
# lambda.
num_lambdas = length(lam_vec)
results = data.frame(lambda=lam_vec, 
                     rank=rep(NA, num_lambdas), 
                     rmse=rep(NA, num_lambdas))
fits = vector('list', num_lambdas)

# Populate the results df and fits list
for (i in 1:num_lambdas) {
  print(paste0('i = ',i,' out of ',num_lambdas))
  # Complete our user-by-movie matrix with softImpute()
  # Populate fits with the result
  if (i == 1) {
    fits[[i]] = softImpute(uid_by_qid, rank.max = 30, 
                           lambda = lam_vec[i], maxit = 1000)
  } else {
    fits[[i]] = softImpute(uid_by_qid, rank.max = 30, lambda = lam_vec[i], 
                           maxit = 1000, warm.start = fits[[i-1]])
  }
  # Compute the rank of the diagonal matrix generated by softImpute()
  # and populate the appropriate results element
  dround = round(fits[[i]]$d, digits=4)
  results[i, 'rank'] = sum(dround != 0.0)
  # Call impute() to predict the ratings in the test set
  imp = impute(fits[[i]], melted_test$uid, melted_test$qid)
  # Calculate the RMSE between the predicted and actual ratings
  # in the test set and populate the appopriate results element
  results[i, 'rmse'] = rmse(imp, melted_test$value)
}

# Extract the softImpute() model yielding the lowest RMSE
best_svd = fits[[which.min(results$rmse)]]

# Now that we've gone through the mechanics of imputing a 
# recommender system, let's use the professional-grade 
# tool for the task: 
# Remember, rows for users, columns for questions
complete_values = 
  complete(Incomplete(melted_train$uid, 
                      melted_train$qid),
           best_svd)
complete_values = complete_values[1:max_uid, 1:max_qid]

# Run PCA on the completed matrix
melted_pca = prcomp(complete_values, scale. = TRUE)

# Plot the eigenvalues of the SVD to estimate how many 
# factors are necessary for subsequent factor analysis
ggplot() + 
  geom_point(aes(x=1:50, y=melted_pca$sdev[1:50]), color='blue')


# The eigenvalues have stopped decaying rapidly by the 10th, 
# so let's run factor analysis with 10 factors
melted_fa = fa(scale(complete_values), nfactors=10, rotate='oblimin')


# Write a function for extracting the most important loadings 
# of each factor, i.e. the features with the strongest 
# correlations (whether positive or negative) to a factor
most_important_loadings = function(loadings, 
                                   col,
                                   n=10, 
                                   translate=FALSE,
                                   key=NULL, 
                                   kcol=NULL,
                                   vcol=NULL) {
  if (!is.matrix(loadings) & !is.data.frame(loadings)) {
    stop('Argument "loadings" must be a matrix or data frame')
  }
  if (!translate) {
    return(order(abs(loadings[,col]), decreasing = TRUE)[1:n])
  } else {
    if (!is.matrix(key) & !is.data.frame(key)) {
      stop('Argument "key" must be a matrix or data frame if argument "translate" is TRUE')
    }
    if (!is.numeric(kcol) & !is.character(kcol)) {
      stop('Argument "kcol" must be a numeric or character if argument "translate" is TRUE')
    }
    if (!is.numeric(vcol) & !is.character(vcol)) {
      stop('Argument "vcol" must be a numeric or character if argument "translate" is TRUE')
    }
    return(key[match(order(abs(loadings[,col]), decreasing = TRUE)[1:n], key[[kcol]]), vcol])
  }
}

# Print the most important n loadings for each factor represented 
# by a column in the loadings matrix/df
print_most_important_loadings = function(loadings, 
                                         n=10,
                                         translate=FALSE,
                                         key=NULL, 
                                         kcol=NULL,
                                         vcol=NULL) {
  for (i in 1:ncol(loadings)) {
    print(paste0(n,' most important loadings for factor ',i,':'))
    print(most_important_loadings(loadings, col=i, n, 
                                  translate, key, 
                                  kcol, vcol))
    print('')
  }
}


print_most_important_loadings(melted_fa$loadings, n=10,
                              translate=TRUE, 
                              key=questionKey,
                              kcol='qid',
                              vcol='text')












