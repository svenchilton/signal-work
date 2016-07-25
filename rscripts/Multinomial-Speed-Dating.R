# Sven Chilton and Nathan Helm-Burger
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

library(dplyr)
library(glmnet)
library(corrplot)

df = read.csv('../data/speeddating-aggregated.csv')

# Find the 4 most common careers
most_common_career_counts = sort(table(df[['career_c']]), decreasing = TRUE)[1:4]
most_common_career_codes  = as.numeric(names(most_common_career_counts))
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
most_common_careers = careers[most_common_career_codes]

# Filter the df by these 4 careers
df = df[df[,'career_c'] %in% most_common_career_codes,]

# Extract the features
features = dplyr::select(df, attr_o:amb_o, sports:yoga)

# Extract the target, in this case, the filtered career_c column 
target = df[['career_c']]

# Train a multinomial classification model with glmnet()
lambda = 0
multi_fit = glmnet(scale(features), scale(target), family="multinomial")
multi_fit_coeffs = coef(multi_fit, s=lambda)
multi_fit_preds  = data.frame(predict(multi_fit, scale(features), s=lambda))
colnames(multi_fit_preds) = careers[c(1,2,6,7)]


# Convert the log-odds ratios in a given row of the prediction df 
# to probabilities
probabilities = function(preds, rownum) {
  logodds = preds[rownum,]
  return(exp(logodds)/sum(exp(logodds)))
}

# Express our predictions in terms of probabilities
multi_fit_probs = data.frame(matrix(nrow=nrow(multi_fit_preds), 
                                    ncol=ncol(multi_fit_preds)))
colnames(multi_fit_probs) = colnames(multi_fit_preds)

for (rownum in 1:nrow(multi_fit_preds)) {
  multi_fit_probs[rownum,] = probabilities(multi_fit_preds, rownum)
}

View(multi_fit_probs)

# Plot the coefficients with corrplot()
multi_fit_coeffs_mat = as.matrix(do.call(cbind, multi_fit_coeffs))
multi_fit_coeffs_mat = tail(multi_fit_coeffs_mat, -1)
colnames(multi_fit_coeffs_mat) = careers[c(1,2,6,7)]
corrplot(multi_fit_coeffs_mat, is.corr=FALSE)
max(multi_fit_coeffs_mat)

# Just for giggles, let's visualize the correlations among 
# the features
corrplot(cor(features))














