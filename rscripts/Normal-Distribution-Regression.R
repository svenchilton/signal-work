setwd('/Users/svenchilton/GitHub/signal-data-science/R-curriculum/assignments/2-basic-topics/linear-regression/2-simulated-data')

getSamples = function(a,n) {
  x = rnorm(n, mean=0, sd=1)
  b = sqrt(1-a^2)
  error = rnorm(n, mean=0, sd=b)
  y = a*x + error
  return(data.frame(x=x, y=y))
}

df = getSamples(0.1, 500)

ggplot(df, aes(x, y)) + geom_point() + geom_smooth(method = "lm")


lf = lm(y ~ x, df)
coef(lf)[[2]]
coef(lf)['x']
# This extracts the p-value of the fit
summary(lf)$coefficients[2,4]

estimateSlopes = function(a, n, numTrials=500) {
  # Initialize the vector of computed slopes
  slopes = rep(0, numTrials)
  # Call getSamples() numTrials times
  # At each iteration, set the appropriate element of 
  # the slopes vector to the computed slope
  for (i in 1:numTrials) {
    df = getSamples(a,n)
    lf = lm(y ~ x, df)
    slopes[i] = coef(lf)[[2]]
  }
  return(slopes)
}

typeof(estimateSlopes(0.7,200))

slope_df = data.frame(estimateSlopes(0.7,500))
ggplot(slope_df, aes(slopes)) + geom_histogram(bins=50)

sd(slope_df$slope)

dfSD = data.frame(matrix(ncol = 4, nrow = 9))
colnames(dfSD) = c(100,500,2500,10000)
rownames(dfSD) = 1:9/10
for (n in c(100,500,2500,10000)) {
  cc = as.character(n) 
  for (a in 1:9/10) {
    rr = as.character(a)
    dfSD[rr,cc] = sd(estimateSlopes(a,n))
  }
}

View(dfSD)

dfSD2 = data.frame(matrix(ncol = 11, nrow = 1))
colnames(dfSD2) = c(100,1:10*1000)
rownames(dfSD2) = 0.1
for (cc in colnames(dfSD2)) {
  n  = as.numeric(cc) 
  a  = 0.1
  rr = as.character(a)
  dfSD2[rr,cc] = sd(estimateSlopes(a,n))
}

View(dfSD2)

as.numeric('0.1')

colnames(dfSD2)

estimateSlopesWithPVals = function(a, n, numTrials=500) {
  # Initialize the lists of computed slopes and pvals
  slopes = rep(0, numTrials)
  pvals  = rep(0, numTrials)
  # Call getSamples() numTrials times
  # At each iteration, set the appropriate element of 
  # the slopes vector to the computed slope
  for (i in 1:numTrials) {
    df = getSamples(a,n)
    lf = lm(y ~ x, df)
    slopes[i] = coef(lf)[[2]]
    pvals[i]  = summary(lf)$coefficients[2,4]
  }
  return(data.frame(slopes,pvals))
}

temp_df = estimateSlopesWithPVals(0.7,200)
View(temp_df)

tdf2 = getSamples(0.7,200)
tlf = lm(y ~ x, tdf2)
summary(tlf)
ggplot(tdf2, aes(x,y)) + geom_point() + geom_smooth(method='lm')

dfx = estimateSlopesWithPVals(0.0001,1000,numTrials = 10000)
View(dfx)
ggplot(dfx, aes(slopes)) + geom_histogram(bins=50)
ggplot(dfx, aes(pvals)) + geom_histogram(bins=50)
median(dfx$pvals)
# Find the fraction of slopes less than or equal to 0
sum(dfx$slopes <= 0.0)/dim(dfx)[1]
