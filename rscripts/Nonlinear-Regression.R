# Kurt Brown and Sven Chilton
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

# Load the appropriate packages
library('ggplot2')
library('dplyr')
library('caret')
library('caretEnsemble')
library('gbm')

# Load the white wine df
white_wine = read.csv('../data/winequality-white.csv', sep = ';')

# Replace the dots in the column names with underscores
# Note that '.' mean wild card in regex, so we'll need to 
# include the escape characters ('\\')
colnames(white_wine) = gsub('\\.','_',colnames(white_wine))

# plotting wine quality against chemical features
g = ggplot(data=white_wine, mapping=aes(fixed_acidity, quality)) + 
  geom_point() + geom_smooth()

plot_list = vector('list', length=ncol(white_wine)-1)
chemical_features = head(colnames(white_wine), -1)
names(plot_list) = chemical_features
for (feature in chemical_features) {
  plot_list[[feature]] = 
    ggplot(mapping = aes(white_wine[[feature]], white_wine[['quality']])) + 
    geom_point() + geom_smooth()
}

plot(plot_list$citric_acid)



feature = 'citric_acid'
ggplot(mapping = aes(white_wine[[feature]], white_wine[['quality']])) + 
  geom_point() + geom_smooth()

# Figure the plotting stuff out later

# Caret utility function
caret_reg = function(x, y, method, grid, ...) { 
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=1,
                         number=3, verboseIter=TRUE) 
  train(x=x, y=y, method=method, tuneGrid=grid, trControl=control, 
        metric="RMSE", preProcess=c("center", "scale"), ...)
}  

# Create a hyperparameter grid over which to run models
alphas  = seq(0,1,0.1)
lambdas = 2^seq(-4,1,length.out = 20)
param_grid = expand.grid(alpha=alphas, lambda=lambdas)


# fitting an elastic net regularized linear model for wine quality
wq_caret_glm = caret_reg(method='glmnet', x = white_wine[chemical_features],
                         y=white_wine[['quality']], grid=param_grid)


# Create the first row in a results df
# One column will indicate the method used
# The other will indicate the minimum RMSE
results = data.frame(list(method=wq_caret_glm$method,
                          rmse=min(wq_caret_glm$results$RMSE)))

# Predict wine quality with kNN, with k in 1:10
# Find the minimum RMSE and rbind() it to the results df
kgrid = data.frame(list(k=1:20))
wq_caret_knn = caret_reg(method = 'knn', x=white_wine[chemical_features],
                         y = white_wine[['quality']], grid=kgrid)
results = rbind(results, 
                data.frame(list(method=wq_caret_knn$method,
                                rmse=min(wq_caret_knn$results$RMSE))))

# Predict wine quality with MARS
# Again, find the minimum RMSE and rbind() it to the results df
mars_grid = expand.grid(degree=1:5, nprune=10:30)
wq_caret_mars = caret_reg(method = 'earth', x=white_wine[chemical_features],
                          y = white_wine[['quality']], grid=mars_grid)
results = rbind(results, 
                data.frame(list(method=wq_caret_mars$method,
                                rmse=min(wq_caret_mars$results$RMSE))))
# Change the name of the appropriate method element in the results 
# df from 'earth' to 'MARS'
# First, we need to change that column from a factor column to a 
# character column
results$method = as.character(results$method)
results$method[3] = 'MARS'

summary(wq_caret_mars)

# Regression trees
cp_grid = data.frame(list(cp=10^seq(-3, 0, length.out = 10)))
wq_caret_rpart = caret_reg(method='rpart', x=white_wine[chemical_features],
                           y = white_wine[['quality']], grid=cp_grid )
results = rbind(results, 
                data.frame(list(method=wq_caret_rpart$method,
                                rmse=min(wq_caret_rpart$results$RMSE))))
summary(wq_caret_rpart)

plot(wq_caret_rpart$finalModel)
text(wq_caret_rpart$finalModel)

# Random Forest
mtry_grid = data.frame(list(mtry=2:6))
wq_caret_ranger = caret_reg(method='ranger', x=white_wine[chemical_features],
                            y = white_wine[['quality']], grid = mtry_grid, importance='impurity')

results = rbind(results, 
                data.frame(list(method=wq_caret_ranger$method,
                                rmse=min(wq_caret_ranger$results$RMSE))))
summary(wq_caret_ranger)

# Out of bag error: 
oob_error = wq_caret_ranger$finalModel$prediction.error

# OOB error is significantly lower than the RMSE obtained from 
# this model

# Gradient boosted trees
gbm_grid = as.matrix(expand.grid(n.trees=500, 
                                 shrinkage=seq(0.01, 0.1, 0.03), 
                                 interaction.depth=c(1, 5, 10, 20, 40, 60), 
                                 n.minobsinnode=1:3))
wq_caret_gbm = caret_reg(method='gbm', x=white_wine[chemical_features],
                         y = white_wine[['quality']], grid = gbm_grid)

# Re-run GBM, this time by varying only n.trees=seq(500,5000,500) and using 
# the bestTune parameters for the rest
gbm_grid = expand.grid(n.trees=seq(500,5000,500),
                       shrinkage=wq_caret_gbm$bestTune$shrinkage,
                       interaction.depth=wq_caret_gbm$bestTune$interaction.depth,
                       n.minobsinnode=wq_caret_gbm$bestTune$n.minobsinnode)
wq_caret_gbm = caret_reg(method='gbm', x=white_wine[chemical_features],
                         y = white_wine[['quality']], grid = gbm_grid)
results = rbind(results, 
                data.frame(list(method=wq_caret_gbm$method,
                                rmse=min(wq_caret_gbm$results$RMSE, na.rm=TRUE))))

# Cubist regression
cubist_grid = expand.grid(committees = seq(30,50,5),
                          neighbors = 5:9)
wq_caret_cubist = caret_reg(method='cubist', x=white_wine[chemical_features],
                            y = white_wine[['quality']], grid=cubist_grid)

results = rbind(results, 
                data.frame(list(method=wq_caret_cubist$method,
                                rmse=min(wq_caret_cubist$results$RMSE, na.rm=TRUE))))

# STACKING!
ensemble_methods = c('glmnet', 'earth', 'rpart', 'knn', 'ranger', 'gbm')
ensemble_control = trainControl(method = 'repeatedcv', repeats=1,
                                number = 3, verboseIter=TRUE,
                                savePredictions = 'final')
 
ensemble_tunes = list(
  glmnet = caretModelSpec(method='glmnet', tuneGrid=param_grid), 
  earth  = caretModelSpec(method='earth',  tuneGrid=mars_grid), 
  rpart  = caretModelSpec(method='rpart',  tuneGrid=cp_grid),
  knn = caretModelSpec(method = 'knn', tuneGrid = kgrid),
  ranger = caretModelSpec(method='ranger', tuneGrid = mtry_grid),
  gbm = caretModelSpec(method='gbm', tuneGrid=gbm_grid))

ensemble_fits = caretList(white_wine[chemical_features], 
                          white_wine[['quality']], 
                          trControl=ensemble_control,
                          methodList=ensemble_methods, 
                          tuneList=ensemble_tunes, 
                          preProcess=c("center", "scale"))


fit_ensemble = caretEnsemble(ensemble_fits) 
print(fit_ensemble)
summary(fit_ensemble)
 

















