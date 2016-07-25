# Anscombe's Quartet

# Load in the packages
library('car')
library('ggplot2')
library('GGally')

# Set working directory
setwd('~/GitHub/signal-work/rscripts')

# Read in the data
df <- read.csv('../data/anscombe.csv')
df

# Compute mean and variance of each column
means <- c()
vars <- c()
for (i in 1:ncol(df)) {
  l <- length(means)
  means[l+1] <- mean(df[[i]])
  vars[l+1] <- var(df[[i]])
}
means
vars

# Compute the correlations of each x-y pair of columns in df
cor1 <- cor(df$x1, df$y1)
cor2 <- cor(df$x2, df$y2)
cor3 <- cor(df$x3, df$y3)
cor4 <- cor(df$x4, df$y4)
cor1
cor2
cor3
cor4

# Calculate the linear fit parameters of each x-y pair of columns
lf1 = lm(y1 ~ x1, df)
lf2 = lm(y2 ~ x2, df)
lf3 = lm(y3 ~ x3, df)
lf4 = lm(y4 ~ x4, df)
summary(lf1)
summary(lf2)
summary(lf3)
summary(lf4)

# Plots
ggplot(df, aes(x4, y4)) + geom_point() + geom_smooth(method = "lm")
plot(df$x1, df$y1)


