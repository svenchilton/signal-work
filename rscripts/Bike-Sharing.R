# Kurt Brown and Sven Chilton
# Signal Data Science Cohort 3

# Bike Sharing Demand Kaggle Challenge

setwd('~/GitHub/signal-work/rscripts')

# Load the appropriate packages
library('ggplot2')
library('plyr')
library('dplyr')
library('caret')
library('caretEnsemble')
library('gbm')

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}


# For comparison, load the sampleSubmission df
sampleSubmission = read.csv('../data/bike-sharing-sampleSubmission.csv')

# Load the training and test dfs
train = read.csv('../data/bike-sharing-train.csv')
test  = read.csv('../data/bike-sharing-test.csv')

# Determine the columns to which to apply a Box-Cox transformation
# Then apply it
# Also keep track of the lambdas used (if any) in transforming the 
# columns, and whether those columns are shifted
# For each column of train, apply the same transformation to the 
# corresponding column of test
bcnames = tail(head(colnames(train),-3),-1)
bctrain = train
bctest  = test
shift = rep(0,length(bcnames))
lambdas = rep(NA,length(bcnames))
for (i in 1:length(bcnames)) {
  name = bcnames[i]
  if (!is.factor(train[[name]])) {
    lo = min(train[[name]], na.rm=TRUE)
    hi = max(train[[name]], na.rm=TRUE)
    if (lo < 0) next
    else if (lo == 0) {
      bctrain[[name]] = bctrain[[name]] + 1
      shift[i] = 1
    }
    bctest[[name]] = bctest[[name]] + shift[i]
    bctrans = BoxCoxTrans(bctrain[[name]])
    bctrain[[name]] = predict(bctrans, bctrain[[name]])
    lam = bctrans$lambda
    lambdas[i] = lam
    if (!is.na(lam)) bctest[[name]] = (bctest[[name]]^lam - 1)/lam
  }
}

# Hyperparameter grids
glmnet_grid = expand.grid(alpha=seq(0,1,0.1), 
                          lambda=2^seq(-4,1,length.out = 20))
earth_grid  = expand.grid(degree=1:5, nprune=10:30)
rpart_grid  = data.frame(list(cp=10^seq(-3, 0, length.out = 10)))
knn_grid    = data.frame(list(k=1:20))
ranger_grid = data.frame(list(mtry=2:6))
gbm_grid    = expand.grid(n.trees=500,
                          shrinkage=seq(0.01, 0.1, 0.03), 
                          interaction.depth=c(1, 5, 10, 20, 40, 60),
                          n.minobsinnode=1:3)



# STACKING!
ensemble_methods = c('glmnet', 'earth', 'rpart', 'knn', 'ranger', 'gbm')
ensemble_control = trainControl(method = 'repeatedcv', repeats=1,
                                number = 3, verboseIter=TRUE,
                                savePredictions = 'final')

ensemble_tunes = list(
  glmnet = caretModelSpec(method='glmnet', tuneGrid=glmnet_grid), 
  earth  = caretModelSpec(method='earth',  tuneGrid=earth_grid), 
  rpart  = caretModelSpec(method='rpart',  tuneGrid=rpart_grid),
  knn    = caretModelSpec(method='knn',    tuneGrid=knn_grid),
  ranger = caretModelSpec(method='ranger', tuneGrid=ranger_grid),
  gbm    = caretModelSpec(method='gbm',    tuneGrid=gbm_grid))

ensemble_fits = caretList(bctrain[bcnames], 
                          bctrain[['count']], 
                          trControl=ensemble_control,
                          methodList=ensemble_methods, 
                          tuneList=ensemble_tunes, 
                          preProcess=c("center", "scale"))

ensemble_fits2 = caretList(train[bcnames], 
                           train[['count']], 
                           trControl=ensemble_control,
                           methodList=ensemble_methods, 
                           tuneList=ensemble_tunes, 
                           preProcess=c("center", "scale"))

# We've been running into problems here.  
# caretEnsemble() seems to think that the different models 
# are generating different types of predictions, in this case, 
# numeric and character.  We verified that calling predict() 
# on the output of each model results in numeric predictions, 
# so I have no idea what's going on.  
fit_ensemble  = caretEnsemble(ensemble_fits) 
fit_ensemble2 = caretEnsemble(ensemble_fits2) 

# Given the trouble with caretEnsemble(), for our first 
# attempt at a submission, we'll examine each component 
# model.  We'll use whichever one yields the lowest RMSE 
# to predict bike sharing counts using from the test df.
rmse_vec = rep(NA, 6)
names(rmse_vec) = 
  c('glmnet','earth','rpart','knn','ranger','gbm')
for (name in names(rmse_vec)) {
  rownum = as.numeric(rownames(ensemble_fits[[name]][['bestTune']]))
  rmse_vec[name] = ensemble_fits[[name]][['results']][['RMSE']][rownum]
}

rmse_vec2 = rep(NA, 6)
names(rmse_vec2) = 
  c('glmnet','earth','rpart','knn','ranger','gbm')
for (name in names(rmse_vec2)) {
  rownum = as.numeric(rownames(ensemble_fits2[[name]][['bestTune']]))
  rmse_vec2[name] = ensemble_fits2[[name]][['results']][['RMSE']][rownum]
}

# Let's see if we can tune the hyperparameters of each of 
# the models in ensemble_fits2 a bit better



















