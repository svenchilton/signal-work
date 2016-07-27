# Sven Chilton
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

# Load the relevant packages
library('psych')
library('ggplot2')
library('dplyr')
library('dummies')
library('glmnet')
library('caret')
library('pROC')
library('foreign')
library('corrplot')


# Sigmoid function
sigmoid = function(x) 1./(1.+exp(-x))

# National Election Study

# Read in the cleaned elections data
df = read.csv('../data/nes_cleaned_1992.csv')

# Binarize the columns of df which are already effectively 
# binary
df$gender = as.numeric(df$gender == 'male')
df$union  = as.numeric(df$union  == 'yes')
df$vote   = as.numeric(df$vote   == 'yes')


# Exploratory analysis
# The mosaic plot generated below seems to suggest that the 
# richer one is, the more likely one is to vote republican, 
# at least in the election(s) described in this data set.
mosaicplot(table(df$income, df$presvote))


# Predict support for George H.W Bush (republican) in the 1992 
# presidential election, restricting our attention to people 
# who actually voted, of course
# Right, we should also eliminate anyone who voted for a 
# third-party candidate 
dums_1992 = 
  dummy.data.frame(df[df$vote == '1' & 
                        (df$presvote == 'republican' | 
                           df$presvote == 'democrat'),], 
                   names=c('race','educ','urban','region',
                           'income','occup1','religion',
                           'presvote'), 
                   sep='_')
dums_1992 = na.omit(dums_1992)

# Scale the age column
# Storing the scaled column vector as an intermediate 
# quantity will allow us to preserve the scaling 
# parameters
scaled_1992_ages = scale(dums_1992['age'])
dums_1992['age'] = scaled_1992_ages


# Let's get rid of some dummy factor columns to avoid the 
# multicollinearity trap
dums_1992[paste0(c('race','educ','urban','region',
                        'income','occup1','religion',
                        'presvote'),'_NA')] = NULL

# Make the prediction with glm()
#glm_arg1 = paste(presvote_republican,"~ .") # Default separation (' ')
rep_model = glm(presvote_republican ~ ., 
            family='binomial', 
            data=dplyr::select(dums_1992, -vote, -presvote_democrat))
predicted_rep_log_odds = predict(rep_model, dums_1992)
predicted_rep = as.numeric(predicted_rep_log_odds >= 0)
predicted_rep_sigmoid = sigmoid(predicted_rep_log_odds)

roc(dums_1992$presvote_republican,predicted_rep_sigmoid,plot = TRUE)

mean(predicted_rep)

dem_model = glm(presvote_democrat ~ ., 
                family='binomial', 
                data=dplyr::select(dums_1992, -vote, -presvote_republican))
predicted_dem_log_odds = predict(dem_model, dums_1992)
predicted_dem = as.numeric(predicted_dem_log_odds >= 0)
predicted_dem_sigmoid = sigmoid(predicted_dem_log_odds)

roc(dums_1992$presvote_republican,predicted_dem_sigmoid,plot = TRUE)

mean(predicted_dem)



# All right, according the the GLM models I ran above, estimated 
# probabilities of voting republican and democrat are,  
# respectively, 0.404908 and 0.595092.  

# Now let's examine the coefficients of the republican model 
# above to determine which features correlated most strongly 
# with or against voting republican in 1992
# Eliminate the intercept
rep_features = tail(rep_model$coefficients,-1)
strongest_predictors = rep_features[names(sort(abs(rep_features), decreasing = TRUE))][1:10]
# Clean up the names
names(strongest_predictors)[5] = "income_0-16 %ile"
names(strongest_predictors)[9] = "income_17-33 %ile"

# According to this, being Black or Jewish correlated strongly 
# aginst voting republican in 1992.  No surprise there.  
# A positive correlation between being Asian and voting 
# republican is a bit of a surprise, though.  Likwise, it is 
# no surprise that belonging to a non-Judeo-Christian religion 
# (including atheism and agnosticism) correlates against 
# voting republican.  

# The AUC for the overall republican model was 0.7675.  Let's 
# see what we get with only the strongest predictors.  

rep2_model = glm(presvote_republican ~ ., 
                family='binomial', 
                data=dums_1992[,c('presvote_republican',
                                  names(strongest_predictors))])
predicted_rep2_log_odds = predict(rep2_model, dums_1992)
predicted_rep2 = as.numeric(predicted_rep2_log_odds >= 0)
predicted_rep2_sigmoid = sigmoid(predicted_rep2_log_odds)

roc(dums_1992$presvote_republican,predicted_rep2_sigmoid,plot = TRUE)

mean(predicted_rep2)

# The AUC for this model was 0.7487, so we can conclude that 
# the 10 strongest predicting features account for most of 
# the factors behind voting republican.  


# Now let's run the model on people who DIDN'T vote and 
# predict how they would have voted
didnt_vote_1992 = 
  dummy.data.frame(df[df$vote == '0',], 
                   names=c('race','educ','urban','region',
                           'income','occup1','religion'), 
                   sep='_')
# The presvote column is meaningless, so eliminate it
didnt_vote_1992$presvote = NULL
didnt_vote_1992 = na.omit(didnt_vote_1992)

# Scale the age column
# Storing the scaled column vector as an intermediate 
# quantity will allow us to preserve the scaling 
# parameters
scaled_dv_1992_ages = scale(didnt_vote_1992['age'])
didnt_vote_1992['age'] = scaled_dv_1992_ages

# Run the model on the (filtered) people who didn't vote  
# in 1992
predicted_dv_rep_log_odds = predict(rep_model, didnt_vote_1992)
predicted_dv_rep = as.numeric(predicted_dv_rep_log_odds >= 0)
predicted_dv_rep_sigmoid = sigmoid(predicted_dv_rep_log_odds)

mean(predicted_dv_rep)

# Our model predicts that only 25.6% of the study participants 
# who didn't vote in 1992 would have voted republican.




# National Merit Twin Study

# Read in the data frame
nmdf = read.csv('../data/NMSQT.csv')
names(nmdf)
# Convert the values of the ZYG and SEX columns to 0s and 1s
# ZYG = 0 ==> fraternal twins
# ZYG = 1 ==> identical twins
# SEX = 0 ==> female
# SEX = 1 ==> male
nmdf$ZYG = as.numeric(nmdf$ZYG) - 1
nmdf$SEX = as.numeric(nmdf$SEX) - 1


# Create a grid of alpha and lambda parameters over which to 
# run our classification models
alphas  = seq(0,1,by=0.1)
lambdas = 2^seq(0,2,by=0.25)
param_grid = expand.grid(alpha=alphas, lambda=lambdas)

# Set 10-fold cross validation with 3 repeats
control = trainControl(method="repeatedcv", number=10, repeats=3, 
                       verboseIter=TRUE)


# Search over the alpha-lambda grid and make the predictions
caret_fit = train(x=scale(as.matrix(dplyr::select(nmdf, ZYG, SEX, V11093:V11572))), 
                  y=nmdf[['NMSQT']], 
                  method="glmnet", 
                  tuneGrid=param_grid, 
                  trControl=control)

# View the optimal values of alpha and lambda
caret_fit$bestTune
caret_fit$bestTune$lambda

# View the R-squared value for the optimal (alpha, lambda) 
# pair.  This is the fraction of variance in the data which 
# our best model explains.  
caret_fit$results

# Looking at the console's printout of this data reveals 
# that our best R-squared is 0.4965573.  Not great.  

# Now let's make a glmnet() model with the optimal 
# alpha and lambda values determined above
# The goal here is to determine which questions have the 
# greatest predictive power in modeling NMSQT score
glmnet_fit = glmnet(x=scale(as.matrix(dplyr::select(nmdf, ZYG, SEX, V11093:V11572))), 
                     y=nmdf[['NMSQT']],
                     lambda=caret_fit$bestTune$lambda,
                     alpha=caret_fit$bestTune$alpha)
glmnet_coeffs = tail(coef(glmnet_fit),-1)
nm_names = rownames(glmnet_coefs)
glmnet_coeffs = as.vector(glmnet_coeffs)
names(glmnet_coeffs) = nm_names
features_by_coef = glmnet_coeffs[names(sort(abs(glmnet_coeffs), decreasing = TRUE))]
questions_by_coef = features_by_coef[!(names(features_by_coef) %in% c("SEX", "ZYG"))]
sorted_names = names(questions_by_coef)
question_codes = read.csv('../data/NMSQTcodebook.csv')

# Change the var column of the question_codes df to the row names
rownames(question_codes) = question_codes$var
question_codes$var = NULL

# Now we can bind the coeffs vector to the question_codes df 
# in the proper order
question_codes = question_codes[sorted_names,]
question_codes$coef = questions_by_coef

# The question_codes df now tells us how which questions correlate 
# with NMSQT scores, in decreasing order of absolute value of 
# correlation (coefficient from the glmnet fit)


# Now let's do PCA on the question columns of nmdf
# NOT THE ZYG OR SEX COLUMNS!!
p = prcomp(dplyr::select(nmdf, V11093:V11572), scale.=TRUE)

# Plot the eigenvalues of the matrix resulting from the singular 
# value decomposition embedded in the prcomp() function
g = ggplot() + 
  geom_point(aes(x=1:length(p$sdev), y=p$sdev), color='blue') + 
  ylab('# deviations') + 
  xlab('Principal Component')
g

# Looking at the plot generated above, we see that by the 15th 
# principal component, the variance associated with each principal 
# component decreases at roughly the same rate.  As such, I will 
# run factor analysis with 15 factors.

f = fa(scale(dplyr::select(nmdf, V11093:V11572)), nfactors=15, rotate='oblimin')

original_question_codes = read.csv('../data/NMSQTcodebook.csv')

# Factor 1: self-loathing
f$loadings[,'MR1'][names(sort(abs(f$loadings[,'MR1']),decreasing = TRUE))][1:10]

# Factor 2: dependability
f$loadings[,'MR2'][names(sort(abs(f$loadings[,'MR2']),decreasing = TRUE))][1:10]

# Factor 3: social anxiety
f$loadings[,'MR3'][names(sort(abs(f$loadings[,'MR3']),decreasing = TRUE))][1:10]

# Factor 4: stereotypical masculinity
f$loadings[,'MR4'][names(sort(abs(f$loadings[,'MR4']),decreasing = TRUE))][1:10]

# Factor 5: flamboyance
f$loadings[,'MR5'][names(sort(abs(f$loadings[,'MR5']),decreasing = TRUE))][1:10]

# Factor 6: literary scholarship
f$loadings[,'MR6'][names(sort(abs(f$loadings[,'MR6']),decreasing = TRUE))][1:10]

# Factor 7: cynicism
f$loadings[,'MR7'][names(sort(abs(f$loadings[,'MR7']),decreasing = TRUE))][1:10]

# Factor 8: careful consideration
f$loadings[,'MR8'][names(sort(abs(f$loadings[,'MR8']),decreasing = TRUE))][1:10]

# Factor 9: home life
f$loadings[,'MR9'][names(sort(abs(f$loadings[,'MR9']),decreasing = TRUE))][1:10]

# Factor 10: sense of unfairness
f$loadings[,'MR10'][names(sort(abs(f$loadings[,'MR10']),decreasing = TRUE))][1:10]

# Factor 11: anger
f$loadings[,'MR11'][names(sort(abs(f$loadings[,'MR11']),decreasing = TRUE))][1:10]

# Factor 12: organization
f$loadings[,'MR12'][names(sort(abs(f$loadings[,'MR12']),decreasing = TRUE))][1:10]

# Factor 13: authority
f$loadings[,'MR13'][names(sort(abs(f$loadings[,'MR13']),decreasing = TRUE))][1:10]

# Factor 14: certainty
f$loadings[,'MR14'][names(sort(abs(f$loadings[,'MR14']),decreasing = TRUE))][1:10]

# Factor 15: morality
f$loadings[,'MR15'][names(sort(abs(f$loadings[,'MR15']),decreasing = TRUE))][1:10]


# Scale the scores of our factor analysis so that each column has 
# a standard deviation of 1 and mean 0.  Put the result in a df.  
scaled_scores = data.frame(scale(f$scores))

# Oh good, the number of rows of scaled_scores equals the number 
# of rows of nmdf.  
scaled_scores = cbind(dplyr::select(nmdf,ID:NMSQT), scaled_scores)

aggregate(dplyr::select(scaled_scores, NMSQT:MR8), by=list(scaled_scores$SEX), FUN = mean)

# Extract the pairs of fraternal twins and sort by ID
# Then compute the correlation 
fraternal = scaled_scores[scaled_scores$ZYG == 0,]
identical = scaled_scores[scaled_scores$ZYG == 1,]
fraternal = fraternal[order(fraternal$ID),]
identical = identical[order(identical$ID),]
fpairs = nrow(fraternal)/2
feven  = (1:fpairs)*2
fodd   = feven - 1
ipairs = nrow(fraternal)/2
ieven  = (1:ipairs)*2
iodd   = ieven - 1
fcor = cor(dplyr::select(fraternal, NMSQT:MR8)[feven,],
           dplyr::select(fraternal, NMSQT:MR8)[fodd,])
icor = cor(dplyr::select(identical, NMSQT:MR8)[ieven,],
           dplyr::select(identical, NMSQT:MR8)[iodd,])


genetic_variance = diag(2*(icor - fcor))



