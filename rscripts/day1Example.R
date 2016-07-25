# Sven Chilton and Matt Wong

### UN INFANT MORTALITY DATA ###

# Write code here to load packages and data
#install.packages('car')
#install.packages('Ecdat')
#install.packages('HistData')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('Rmisc')
#install.packages('GGally')

library('car')
library('ggplot2')
library('GGally')

# Presumably, the UN data comes from one of 
# the packages we just installed
df <- UN
View(df)

head(df2)
head(df$infant.mortality)

# Write code here to calculate correlations
cor(df$gdp, df$infant.mortality, use='pairwise.complete.obs')

cor2 <- function(df) {
  return(round(100*cor(df, use='pairwise.complete.obs')))
}
cor2(df)

# Write code here to make a new dataframe with incomplete rows omitted
df2 = na.omit(df)

# Write code here to examine the distribution of the data
distribution = ggpairs(df2)
distribution
# Write code here to take the log transformation of the data
toldf = log(df2)

# Write code here to examine the distribution of the log-transformed data
dist_log = ggpairs(df2_log)
dist_log

# Calculate linear fit of infant mortality vs. GDP
linear_fit = lm(infant.mortality ~ gdp, df2)
summary(linear_fit)
# Calculate linear fit of log(infant mortality) vs. log(GDP)
loglog_fit = lm(infant.mortality ~ gdp, toldf)
summary(loglog_fit)
# Plot the linear fit of infant mortality vs. GDP
ggplot(toldf, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")

# Plot of linear fit residuals
qplot(df2$gdp, linear_fit$residuals)
qplot(toldf$gdp, loglog_fit$residuals)

# Plot of linear fit residuals after log transformation of GDP and infant mortality
qplot(df2$gdp, df2$infant.mortality - exp(fitted(loglog_fit)))

