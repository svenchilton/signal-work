# Sven Chilton and Ivan Dimitrov
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

library('dplyr')
library('softImpute')
library('DAAG')
library('corrplot')
library('pROC')
library('dummies')

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}

# Read in the ratings data
ratings = read.csv('../data/ratings.dat', sep = ':', header = FALSE)

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
# Since R's read functions can't handle multi-character separators, I 
# used emacs to change all of the "::" separators in the .dat file to 
# tabs
movies = read.csv('../data/movies.dat', sep = '\t', header = FALSE)

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
# only the rows corresponding to movieIDs which are present in the 
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

# Generate a new df with columns for: 
# 1) movie titles
# 2) whether or not the film is a drama
# 3) the probability that said film is a drama, according to our model
# Arrange the rows from greatest to smalles probability
drama_df = data.frame(list(title=movies$title, 
                           Drama=movies$Drama, 
                           Drama_Probability=drama_model_cv$cvhat))
drama_df = drama_df[order(drama_df$Drama_Probability, decreasing = TRUE),]

# Let's compute the precision and recall for our drama model
drama_actual_pos = sum(drama_df$Drama == 1)
drama_predicted_pos = sum(drama_df$Drama_Probability >= 0.5)
drama_true_pos = sum((drama_df$Drama == 1) & (drama_df$Drama_Probability >= 0.5))
drama_precision = drama_true_pos/drama_predicted_pos
drama_recall = drama_true_pos/drama_actual_pos
drama_f_score = 2*drama_precision*drama_recall/(drama_precision + drama_recall)

# OK, the drama model isn't so great

# Let's repeat this analysis for 3 other genres
# Let's say Comedy, Musical, and Sci-Fi
comedy_model = glm(Comedy ~ ., family='binomial',data=dplyr::select(movies, Comedy, f1:f30))
comedy_model_cv = CVbinary(comedy_model)
comedy_df = data.frame(list(title=movies$title, 
                           Comedy=movies$Comedy, 
                           Comedy_Probability=comedy_model_cv$cvhat))
comedy_df = comedy_df[order(comedy_df$Comedy_Probability, decreasing = TRUE),]
comedy_actual_pos = sum(comedy_df$Comedy == 1)
comedy_predicted_pos = sum(comedy_df$Comedy_Probability >= 0.5)
comedy_true_pos = sum((comedy_df$Comedy == 1) & (comedy_df$Comedy_Probability >= 0.5))
comedy_precision = comedy_true_pos/comedy_predicted_pos
comedy_recall = comedy_true_pos/comedy_actual_pos
comedy_f_score = 2*comedy_precision*comedy_recall/(comedy_precision + comedy_recall)

# The comedy model has decent precision, but lousy recall


musical_model = glm(Musical ~ ., family='binomial',data=dplyr::select(movies, Musical, f1:f30))
musical_model_cv = CVbinary(musical_model)
musical_df = data.frame(list(title=movies$title, 
                            Musical=movies$Musical, 
                            Musical_Probability=musical_model_cv$cvhat))
musical_df = musical_df[order(musical_df$Musical_Probability, decreasing = TRUE),]
musical_actual_pos = sum(musical_df$Musical == 1)
musical_predicted_pos = sum(musical_df$Musical_Probability >= 0.5)
musical_true_pos = sum((musical_df$Musical == 1) & (musical_df$Musical_Probability >= 0.5))
musical_precision = musical_true_pos/musical_predicted_pos
musical_recall = musical_true_pos/musical_actual_pos
musical_f_score = 2*musical_precision*musical_recall/(musical_precision + musical_recall)

# The musical model is terrible!


scifi_model = glm(`Sci-Fi` ~ ., family='binomial',data=dplyr::select(movies, `Sci-Fi`, f1:f30))
scifi_model_cv = CVbinary(scifi_model)
scifi_df = data.frame(list(title=movies$title, 
                             SciFi=movies$`Sci-Fi`, 
                             SciFi_Probability=scifi_model_cv$cvhat))
scifi_df = scifi_df[order(scifi_df$SciFi_Probability, decreasing = TRUE),]
scifi_actual_pos = sum(scifi_df$SciFi == 1)
scifi_predicted_pos = sum(scifi_df$SciFi_Probability >= 0.5)
scifi_true_pos = sum((scifi_df$SciFi == 1) & (scifi_df$SciFi_Probability >= 0.5))
scifi_precision = scifi_true_pos/scifi_predicted_pos
scifi_recall = scifi_true_pos/scifi_actual_pos
scifi_f_score = 2*scifi_precision*scifi_recall/(scifi_precision + scifi_recall)

# As was the case for comedy, the sci-fi model has good precision, but terrible recall



# Read in the users data
# Since R's read functions can't handle multi-character separators, I 
# used emacs to change all of the "::" separators in the .dat file to 
# tabs
users = read.csv('../data/users.dat', sep = '\t', header = FALSE)

# Rename the remaining columns appropriately, as indicated in 
# movie-readme.txt: 

colnames(users) = c('userID','gender','age','occupation','zip_code')

# Filter the users by restricting users to ages 35+ and 
# eliminating users with occupation codes 0 and 16 
# ('other' and 'self-employed', respectively)

filtered_users = users[(users$age >= 35) & 
                         (users$occupation != 0) & 
                         (users$occupation != 16),]

# Now find the 4 most common occupations among those 
# users and extract the rows corresponding to users in 
# those occupations

occupation_count = 
  group_by(filtered_users, occupation) %>% 
  summarise(n())
colnames(occupation_count)[2] = 'count'
occupation_count = 
  occupation_count[(occupation_count$occupation != 0) & 
                     (occupation_count$occupation != 16),]
occupation_count = 
  occupation_count[order(occupation_count$count, decreasing = TRUE),]
# The +1 below is to account for the occupation codes starting at 0, 
# but the occupations vector starting at 1
most_common_occupations = (occupation_count$occupation[1:4]) + 1
occupations = c("other or not specified",
                "academic/educator",
                "artist",
                "clerical/admin",
                "college/grad student",
                "customer service",
                "doctor/health care",
                "executive/managerial",
                "farmer",
                "homemaker",
                "K-12 student",
                "lawyer",
                "programmer",
                "retired",
                "sales/marketing",
                "scientist",
                "self-employed",
                "technician/engineer",
                "tradesman/craftsman",
                "unemployed",
                "writer")
occupations[most_common_occupations]

filtered_users = 
  filtered_users[filtered_users$occupation %in% most_common_occupations,]

# Use unregularized multinomial logistic regression to predict 
# career in terms of the factors for each user obtained from 
# best_svd

U = data.frame(best_svd$u)

# Apply the same filter to U as to the users df
filtered_U = U[1:6041 %in% filtered_users$userID,]

# Train a multinomial classification model with glmnet()
lambda = 0
multi_fit = glmnet(scale(filtered_U), 
                   scale(filtered_users$occupation), 
                   family="multinomial")
multi_fit_coeffs = coef(multi_fit, s=lambda)
multi_fit_preds  = data.frame(predict(multi_fit, scale(filtered_U), s=lambda))
colnames(multi_fit_preds) = occupations[most_common_occupations]

# Run PCA on the log-odds generated above
user_pca = prcomp(multi_fit_preds, scale.=TRUE)

user_pca$rotation



# I'm not sure the results make sense, but screw it, I'm moving on.


# Let's change the names of some of the genres and the associated 
# columns in movies to make string handling a little easier
genre_names[4]  = 'Childrens'
genre_names[10] = 'Film_Noir'
genre_names[15] = 'Sci_Fi'

colnames(movies)[4:(length(genre_names)+3)] = genre_names

# Now let's make a function which takes a movie genre name, 
# runs unregularized logistic regression with glm to predict 
# whether the movies are of the given genre from the factors 
# of the movies df, multiplies the vector giving the logodds 
# of each movie belonging to the given genre elementwise by 
# each of the factor columns, then performs a column sum to 
# generate scaled loadings relating genres to factors
factor_scores = function(name, df=movies) {
  arg1 = paste0(name,' ~ .')
  model = glm(arg1, 
              family='binomial', 
              data=dplyr::select(df, contains(name), f1:f30))
  set.seed(3); model_cv = CVbinary(model)
  probs = model_cv$cvhat
  logodds = log(probs/(1-probs))
  factors = dplyr::select(df, f1:f30)
  name_factor_scores = logodds*factors
  return(base::colSums(name_factor_scores))
}

# Use the function from above to populate a df
genre_scores = data.frame(lapply(genre_names, factor_scores))
colnames(genre_scores) = genre_names

# Make a dummy users df dummy_users with binarized occupation 
# code columns with names of the form occupation_N
dummy_users = dummy.data.frame(users, names='occupation', sep='_')

# Extract the occupation columns from dummy_users and bind 
# them to the USER factors columns we obtained earlier
# We should make sure to eliminate the fake user
user_factors = head(U, -1)
colnames(user_factors) = colnames(factors)
dummy_careers_factors = 
  cbind(dplyr::select(dummy_users, contains('occupation')),
        user_factors)

# To obtain scores relating careers to user factors, reuse 
# the factor_scores() function, only this time pass in the 
# dummy_careers_factors df and the names of the occupation 
# columns
occ_col_names = paste0('occupation_',0:(length(occupations)-1))
career_scores = data.frame(lapply(occ_col_names, factor_scores, 
                                  df=dummy_careers_factors))
colnames(career_scores) = occupations


# Extract the eigenvalues of our best soft-SVD
eigs = best_svd$d

# Use these eigenvalues to generate a predicted career-genre 
# pairings matrix, which we'll then transform to a df
pairings = 
  as.matrix(t(career_scores)) %*% as.matrix((eigs*genre_scores))
pairings = data.frame(pairings)
rownames(pairings) = colnames(career_scores)
colnames(pairings) = colnames(genre_scores)

corrplot(as.matrix(pairings), is.corr = FALSE)


corrplot(biScale(as.matrix(pairings), 
                 row.scale = FALSE, 
                 col.scale = FALSE), 
         is.corr = FALSE)



# Now that we've gone through the mechanics of imputing a 
# recommender system, let's use the professional-grade 
# tool for the task: 
# Remember, rows for users, columns for movies
complete_ratings = 
  complete(Incomplete(ratings_train_with_fakes$userID, 
                      ratings_train_with_fakes$movieID),
           best_svd)
complete_ratings = complete_ratings[1:6040, 1:3952]



# Function which will let us predict how a user with a 
# given career will, on average, rate each movie
ratings_by_name = function(name, 
                           df=dummy_careers_factors, 
                           c_ratings=complete_ratings) {
  arg1 = paste0(name,' ~ .')
  model = glm(arg1, 
              family='binomial', 
              data=dplyr::select(df, contains(name), f1:f30))
  set.seed(3); model_cv = CVbinary(model)
  probs = model_cv$cvhat
  logodds = log(probs/(1-probs))
  return(base::colSums(logodds*c_ratings)/sum(logodds))
}

# Apply this function to writers, i.e. 'occupation_20'
writer_ratings = ratings_by_name('occupation_20')

# Subtract the mean of each column of complete_ratings 
# from writer_ratings
writer_diff = writer_ratings - base::colMeans(complete_ratings)

# Extract only the movieIDs present in the movies df
writer_diff = 
  writer_diff[1:length(writer_diff) %in% unique(movies$movieID)]

# I'm rather surprised that the predicted average writer 
# ratings are all higher than the overall average ratings 
# for each movie, and that they differ by such small amounts: 
max(writer_diff)
min(writer_diff)

# Let's find out which movies the average writer should 
# like most and least: 
writers_favorite_movie = 
  movies[which.max(writer_diff),'title']
writers_least_favorite_movie = 
  movies[which.min(writer_diff),'title']

# We were told to expect North by Northwest and Toy Story 
# to be the writers' favorite and least favorite movies, 
# respectively, so we're doing something right...

# Function to take a career code (0 - 20), calculate the 
# predicted average rating for that profession minus the 
# overall mean rating for each movie, and return a df 
# containing that difference along with the title and 
# genres of every movie
movies_by_career = function(career_code) {
  name = paste0('occupation_',career_code)
  career_ratings = 
    ratings_by_name(name, dummy_careers_factors, complete_ratings)
  career_diff = career_ratings - base::colMeans(complete_ratings)
  career_diff = 
    career_diff[1:length(career_diff) %in% unique(movies$movieID)]
  movs_by_career = cbind(movies[,c('title','genres')], career_diff)
  colnames(movs_by_career)[3] = paste0(occupations[career_code+1], ' diff')
  movs_by_career = 
    movs_by_career[order(movs_by_career[[3]], decreasing = TRUE),]
  return(movs_by_career)
}

movies_by_writer = movies_by_career(20)

movies_by_farmer = movies_by_career(8)

movies_by_retired = movies_by_career(13)

movies_by_college = movies_by_career(4)















