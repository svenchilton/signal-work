library(dplyr)
library(glmnet)
library(pROC)
library(dummies)
library(corrplot)
df = read.csv('/Users/svenchilton/GitHub/signal-data-science/R-curriculum/datasets/speed-dating/speeddating-full.csv')

#Create data frame with decisions, average decision frequencies, careers and races
df = select(df, gender, iid, pid, wave, dec, attr, race, career_c)
genders = c("female", "male")
df$gender = factor(df$gender, labels = genders)
careers = c("Lawyer", 
            "Academic", 
            "Psychologist", 
            "Doctor", 
            "Engineer",
            "Creative",
            "Business",
            "RealEstate",
            "IntRelations",
            "Undecided",
            "SocialWork",
            "Speech",
            "Politics",
            "Athletics",
            "Other",
            "Journalism",
            "Architecture")
races = c("Black", "White", "Latino", "Asian", "Other")
# df$gender = factor(df$gender, labels = genders)
# df$race = factor(df$race, labels = races)
# df$career_c = factor(df$career_c, labels = careers)
agged = aggregate(df["dec"], df["iid"], FUN = mean, na.rm = T)

colnames(agged) = c("iid", "decAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("dec", "attr")], df["pid"], FUN = mean, na.rm = T)
colnames(agged) = c("pid", "decPartnerAvg", "attrPartnerAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("race", "career_c")], df["iid"], FUN = mean)
agged$race = factor(agged$race, labels = races)
agged$career_c = factor(agged$career_c, labels = careers)
names(agged)
df = inner_join(df[!(names(df) %in% c("race", "career_c"))], agged)
colnames(agged)[1:3] = c("pid", "race_Partner", "career_c_Partner")
df = inner_join(df, agged)

View(df)
View(agged)

#Cross validate regularized logistic regression at the level of waves

crossValidate = function(features, 
                         target, 
                         waves = df$wave,
                         lambdas = (1.2)^(10:(-30)), 
                         alphas = seq(0, 0.24, 0.03)){
  s = scale(features)
  
  s = s[,!is.nan(colSums(s)) & !is.na(colSums(s))]
  rocs = expand.grid(lambda = lambdas, alpha = alphas)
  rocs$logLoss = 0
  rocs$ROC = 0 
  for(alpha in alphas){
    l = lapply(1:21, function(wave){
      cat("alpha:", alpha, " wave: ", wave, "\n")
      trainFeatures = s[waves != wave,]
      testFeatures = s[waves == wave,]
      set.seed(1); m = glmnet(trainFeatures, target[waves != wave], 
                              alpha = alpha, 
                              lambda = lambdas,
                              family = "binomial")
      as.data.frame(predict(m, testFeatures))
    })
    predictions = do.call(rbind, l)
    predictions = exp(predictions/(1 + predictions))
    rocTemp = sapply(predictions, function(cv){
      as.numeric(roc(target,cv)$auc)
    })
    rocs[rocs$alpha == alpha,"ROC"] = rocTemp[length(rocTemp):1]
  }
  rocs
}


dummy(races)

# Make dummy data frames with the dummies package
# dums1 will binarize the race and career_c columns
# dums2 will binarize the race_Partner and career_c_Partner columns
dums1 = dummy.data.frame(df, names=c('race', 'career_c'))
View(dums1)
dums2 = dummy.data.frame(df, names=c('race_Partner', 'career_c_Partner'))


# Combine the dums1 and dums2 data frames, without repeating overlapping columns
dums = cbind(dums1[,1:(ncol(dums1)-2)], dums2[,11:ncol(dums2)])
View(dums)

# Extract subset vectors of column names which we will use to generate 
# interaction columns
decider_career_names = colnames(dums1)[16:33]
partner_career_names = colnames(dums2)[18:35]
decider_race_names = colnames(dums1)[10:15]
partner_race_names = colnames(dums2)[12:17]

# Function to generate the names of interaction columns
# and populate them appropriately
make_interaction_cols = function(decider_names, partner_names, df=dums) {
  num_decider = length(decider_names)
  num_partner = length(partner_names)
  # Loop over every decider name and partner name create and populate 
  # an interaction column
  for (i in 1:num_decider) {
    for (j in 1:num_partner) {
      cur_name = paste(decider_names[i], partner_names[j], sep = ":")
      df[[cur_name]] = df[[decider_names[i]]] * df[[partner_names[j]]]
    }
  }
  # Return the augmented df
  return(df)
}


# Call make_interaction_cols() to grow the dums df
#– (race of decider) x (attractiveness of partner),
dums = make_interaction_cols(decider_race_names, 'attrPartnerAvg', df=dums)
#– (career of decider) x ( attractiveness of partner),
dums = make_interaction_cols(decider_career_names, 'attrPartnerAvg', df=dums)
#– (race of decider) x (race of partner), and
dums = make_interaction_cols(decider_race_names, partner_race_names, df=dums)
#– (career code of decider) x (career code of partner),
dums = make_interaction_cols(decider_career_names, partner_career_names, df=dums)

# Turn gender into a binary column
dums[['gender']] = as.numeric(dums[['gender']] == 'male')

View(dums)

# Remove columns with 20 or fewer entries
names(dums)
bools = col
n1 = names(colSums(dums) > 20)
n2 = names(dums)
n1[!(n1 %in% n2)]
dums = dums[,n2 %in% n1]
dums$career_c=NULL

# Helpful trick for finding rows and columns of NA values in a df
testdf = data.frame(matrix(1:100, nrow=10))
for (i in 1:9) {
  testdf[i,i+1] = NA
  testdf[i+1,i] = NA
}
which(is.na(testdf), arr.ind=TRUE)

# Extract the features (attrs) and target (dec)
attrs = dums[,7:ncol(dums)]
View(head(features))
dec = dums[['dec']]

male_inds = dums[['gender']] == 1
female_inds = dums[['gender']] == 0

# For each of males and females, use the crossValidate() function 
# (which the instructors were kind enough to write for us) to 
# predict the dec column
# Let's use the default values when possible

# Ladies first ;-)
female_rocs = 
  crossValidate(attrs[female_inds,], dec[female_inds], waves = df[female_inds,'wave'])

# Now the gents
male_rocs = 
  crossValidate(attrs[male_inds,], dec[male_inds], waves = df[male_inds,'wave'])

# Find the lambda, alpha pairs yielding the greatest ROC scores 
# for both females and males
female_max_row = which.max(female_rocs[['ROC']])
male_max_row   = which.max(male_rocs[['ROC']])
female_max_params = female_rocs[female_max_row,c(1,2)]
male_max_params   = male_rocs[male_max_row,c(1,2)]

# Run glmnet() with the lambdas and alphas corresponding to 
# the max ROC scores as calculated above
sf = scale(attrs[female_inds,])
sf = sf[,!is.nan(colSums(sf)) & !is.na(colSums(sf))]
optimum_female_alpha  = female_max_params$alpha
optimum_female_lambda = female_max_params$lambda
optimum_female_model  = glmnet(sf, 
                              dec[female_inds], 
                              alpha = optimum_female_alpha, 
                              lambda = optimum_female_lambda,
                              family = "binomial")
sm = scale(attrs[male_inds,])
sm = sm[,!is.nan(colSums(sm)) & !is.na(colSums(sm))]
optimum_male_alpha  = male_max_params$alpha
optimum_male_lambda = male_max_params$lambda
optimum_male_model  = glmnet(sm, 
                             dec[male_inds], 
                             alpha = optimum_male_alpha, 
                             #lambda = optimum_male_lambda,
                             family = "binomial")

female_coeffs = as.matrix(coef(optimum_female_model))
male_coeffs   = as.matrix(coef(optimum_male_model, s=optimum_male_lambda))

# Combine the 1-column coefficient matrices into a single matrix
coeffs = cbind(female_coeffs, male_coeffs)

# Then clean it up by: 
# Changing the column names
colnames(coeffs) = c('Female','Male')

# Removing the top row, corresponding to the intercept
coeffs = tail(coeffs, -1)

# Removing rows where both coefficients are 0
female_zeros = coeffs[,1] == 0.0
male_zeros   = coeffs[,2] == 0.0
coeffs = coeffs[!(female_zeros & male_zeros),]

coeffs

















