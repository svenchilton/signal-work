# Sven Chilton and Ivan Dimitrov
# Signal Data Science Cohort 3

setwd('/Users/svenchilton/GitHub/signal-data-science/R-curriculum/assignments/2-basic-topics/dimensionality-reduction/recommender-systems/')

library('dplyr')
library('softImpute')
library('DAAG')
library('corrplot')
library('pROC')

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}

# Read in the ratings data
ratings = read.csv('~/Documents/ml-1m/ratings.dat', sep = ':', header = FALSE)

# Remove the empty columns
ratings = ratings[,!is.na(colSums(ratings))]

# Eliminate the last column, corresponding to timestamp, 
# and rename the other three appropriately
ratings = ratings[,1:3]
colnames(ratings) = c('userID', 'movieID', 'rating')

# Determine how many movies are represented in our data set
max_movieID  = max(ratings$movieID)
num_movieIDs = length(unique(ratings$movieID))
print(paste0(max_movieID - num_movieIDs,' movie IDs are not represented'))

# Determine how many users are represented in our data set
max_userID  = max(ratings$userID)
num_userIDs = length(unique(ratings$userID))
print(paste0(max_userID - num_userIDs,' user IDs are not represented'))

# Compute the mean of the rating column, first the entire column, 
# then grouped by userID and movieID
overall_mean = mean(ratings$rating)
mean_rating_by_user  = group_by(ratings, userID)  %>% dplyr::summarize(mean(rating))
mean_rating_by_movie = group_by(ratings, movieID) %>% dplyr::summarize(mean(rating))

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

# Set the seed for consistency
# Whenever you rerun code below this line which makes use of a random 
# sampling, rerun everything from here
set.seed(3)

# Split the ratings df into a training set and a test set, with 80% 
# of the ratings data going to the training set
ratings_list = split_data(ratings, train_frac = 0.8)
ratings_train = ratings_list$train
ratings_test  = ratings_list$test

# Create a fake user, with userID 1 greater than max_userID, who has 
# rated every (real) movie with the mean of the entire (real) rating 
# column
fake_userID = max_userID+1
fake_rating = overall_mean

# Make a df with the new fake data and all of the real POSSIBLE 
# movieIDs, not just the ones that have actually been rated
fake_user_df = data.frame(userID  = rep(fake_userID, max_movieID), 
                          movieID = 1:max_movieID, 
                          rating  = rep(fake_rating, max_movieID))

# Now create a fake movie which has been rated by every user 
# Assume that every user has given the fake movie the overall mean 
# rating
fake_movieID = max_movieID+1
fake_movie_df = data.frame(userID  = 1:max_userID,
                           movieID = rep(fake_movieID, max_userID), 
                           rating  = rep(fake_rating, max_userID))


# According the the instructions, the fake user rates every real 
# movie, and every real user rates the fake movie, but the fake 
# user does NOT rate the fake movie.  As such, fake_user_df 
# should have max_movieID (3952) rows, and fake_movie_df should 
# have max_userID (6040) rows.  


# Perturb the fake ratings slightly with a normally distributed 
# error term with mean = 0 and sd = 0.01
fake_user_df$rating = fake_user_df$rating + rnorm(nrow(fake_user_df), mean=0, sd=0.01)
fake_movie_df$rating = fake_movie_df$rating + rnorm(nrow(fake_movie_df), mean=0, sd=0.01)

# Bind the fake data to the ratings data
ratings_train_with_fakes = rbind(ratings_train, fake_user_df, fake_movie_df)

# Create a sparse matrix with the userIDs (including the fake one) 
# as rows and the movieIDs (including the fake one) as columns, 
# using the Incomplete() function from the softImpute package.
user_by_movie = Incomplete(ratings_train_with_fakes$userID, 
                           ratings_train_with_fakes$movieID, 
                           ratings_train_with_fakes$rating)

# Begin collaborative filtering
user_by_movie_scaled = biScale(user_by_movie, maxit = 5, trace = TRUE)
lam0 = lambda0(user_by_movie_scaled)
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
    fits[[i]] = softImpute(user_by_movie_scaled, rank.max = 30, 
                           lambda = lam_vec[i], maxit = 1000)
  } else {
    fits[[i]] = softImpute(user_by_movie_scaled, rank.max = 30, lambda = lam_vec[i], 
                           maxit = 1000, warm.start = fits[[i-1]])
  }
  # Compute the rank of the diagonal matrix generated by softImpute()
  # and populate the appropriate results element
  dround = round(fits[[i]]$d, digits=4)
  results[i, 'rank'] = sum(dround != 0.0)
  # Call impute() to predict the ratings in the test set
  imp = impute(fits[[i]], ratings_test$userID, ratings_test$movieID)
  # Calculate the RMSE between the predicted and actual ratings
  # in the test set and populate the appopriate results element
  results[i, 'rmse'] = rmse(imp, ratings_test$rating)
}

imp = impute(fits[[7]], ratings_test$userID, ratings_test$movieID)
pred_df = data.frame(actual_rating=ratings_test$rating, predicted_rating=imp)

# By inspection, we see that the 7th and 8th lambda values yield the 
# lowest RMSEs.  Let's store the softImpute() model output corresponding 
# to the 7th lambda as best_svd.
best_svd = fits[[7]]

# Add an MAE column to the results df
# First create an MAE function
mae = function(x, y) {
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(mean(abs(x-y)))
}

# Initialize and populate an MAE vector
# Each element will contain the MAE between the actual and predicted 
# ratings in the test set
# Had I known we would be computing this, I would have included it 
# in the prior for loop
mae_vec = rep(NA, num_lambdas)
for (i in 1:num_lambdas) {
  imp = impute(fits[[i]], ratings_test$userID, ratings_test$movieID)
  # Calculate the MAE between the predicted and actual ratings
  # in the test set and populate the appopriate vector element
  mae_vec[i] = mae(imp, ratings_test$rating)
}

# cbind() mae_vec to the results df
results = cbind(results, mae=mae_vec)

# Once again, it appers that a lambda around 20 minimizes our cost 
# function, in this case defined by MAE rather than RMSE

# Treat the ratings prediction as a binary classification problem
# Every rating including and above a threshold constitutes a 
# positive recommendation, and every rating below a threshold 
# constitutes a negative recommendation.
# We'll take the overall_mean of the original movie ratings 
# as our threshold.  
# We'll add precision and recall columns to our results, where 
# precision = true positives / all predicted positives, and 
# recall    = true positives / all actual positives
# Again, this would have been helpful to know before writing the 
# first for loop.
precision_vec = rep(NA, num_lambdas)
recall_vec = rep(NA, num_lambdas)
for (i in 1:num_lambdas) {
  imp = impute(fits[[i]], ratings_test$userID, ratings_test$movieID)
  actual_pos = sum(ratings_test$rating >= overall_mean)
  predicted_pos = sum(imp >= overall_mean)
  true_pos = sum((imp >= overall_mean) & (ratings_test$rating >= overall_mean))
  precision_vec[i] = true_pos/predicted_pos
  recall_vec[i] = true_pos/actual_pos
}
results = cbind(results, precision=precision_vec, recall=recall_vec)

# Once again, it appears that lambda around 20 maximizes recall.  
# However, it appears that lambda between 14 and 18 maximizes precision. 

# Now let's incorporate an asymmetric cost function which penalizes 
# the model more for giving a good rating to an actually badly rated 
# movie than for giving a bad rating to an actually highly rated film.  
cost = matrix(c(0,0,0,3,4, 
                0,0,0,2,3, 
                0,0,0,1,2, 
                7.5,4,1.5,0,0, 
                10,6,3,0,0), nrow=5)
# The rows of the cost matrix correspond to actual ratings, while the 
# columns correspond to predicted ratings

# Now let's add an asymmetric cost column asym to the results
asym_vec = rep(NA, num_lambdas)
for (i in 1:num_lambdas) {
  imp = impute(fits[[i]], ratings_test$userID, ratings_test$movieID)
  imp[imp > 5] = 5
  imp[imp < 1] = 1
  #asym_vec[i] = sum(diag(cost[round(ratings_test$rating),round(imp)]))
  round_rating = round(ratings_test$rating)
  round_imp = round(imp)
  asym_vec[i] = 0
  for (j in 1:length(imp)) {
    asym_vec[i] = asym_vec[i] + cost[round_rating[j], round_imp[j]]
  }
}
results = cbind(results, asym=asym_vec)

# It appears that the 8th lambda value (18.702) minimizes our asymmetric 
# cost function


# Read in the movies data
movies = read.csv('~/Documents/ml-1m/movies-copy.dat', sep = '\t', header = FALSE)

# Rename the columns
colnames(movies) = c('movieID','title','genres')

# Convert the genres column from factors to characters
movies$genres = as.character(movies$genres)

genres1 = movies[1,'genres']

genres_list = strsplit(movies$genres, split='\\|')
movies$genres = genres_list

# Extract the genre names
genre_names = sort(unique(unlist(genres_list)))

# Initialize binary genre columns in the movies df with zeroes
movies[genre_names] = 0


# Loop through each row of the genres column
# Each time a genre name appears in the current row of the genres 
# column, reset the current row of that genre name's column to 1
for (i in 1:nrow(movies)) movies[i,movies$genres[[i]]] = 1

# Filter out the rows of the movies df whose movieID values 
# are not present in the ratings df
movies = movies[movies$movieID %in% unique(ratings$movieID),]

# Extract the "factors" matrix v resulting from the "best" (measured 
# by RMSE) soft-SVD on which softImpute() is based.  Then extract 
# only the rowscorresponding to movieIDs which are present in the 
# ratings df and bind the resulting matrix to the movies df. 
factors = best_svd$v
factors = factors[1:nrow(factors) %in% unique(ratings$movieID),]

# Transform the factors matrix into a df, so we can rename the columns
factors = data.frame(factors)
colnames(factors) = paste0('f',1:30)

movies = cbind(movies, factors)

# Plot the correlations between Drama and the factors
corrplot(cor(dplyr::select(movies, Drama, f1:f30)))

# Generate a GLM binary
drama_model = glm(Drama ~ ., family='binomial',data=dplyr::select(movies, Drama, f1:f30))

drama_model_cv = CVbinary(drama_model)

roc(movies$Drama,drama_model_cv$cvhat, plot=TRUE)

auc(movies$Drama,drama_model_cv$cvhat)












