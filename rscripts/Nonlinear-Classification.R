# Sven Chilton and Gabe Sanchez
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts/')

# Load the appropriate packages
library(ggplot2)
library(dplyr)
library(klaR)
library(MASS)
library(tictoc)
library(e1071)

# Linear pair function 
# Takes in a slope m, intercept b, and label +/- 1, and returns a
# pair c(x,y).  x is chosen randomly from a uniform distribution 
# between 0 and 1 (inclusive), and y is then chosen randomly from 
# a uniform distribution in the range [m*x+b, 1] if label == 1, 
# and in the range [0, m*x+b] if label == -1
lin_pair = function(m, b, label) {
  if (label != 1 & label != -1) stop('Argument "label" must equal 1 or -1')
  x = runif(1, min=0, max=1)
  y = runif(1, min=0, max=1)
  # Make sure that the point is above the line if label == 1
  # or below the line if label == -1
  # AND STILL IN THE ORIGINAL BOX
  if (label == 1) {
    while (y <= m*x+b) {
      x = runif(1, min=0, max=1)
      y = runif(1, min=0, max=1)
    }
  } else if (label == -1) {
    while (y >= m*x+b) {
      x = runif(1, min=0, max=1)
      y = runif(1, min=0, max=1)
    }
  }
  return(c(x,y))
}

df_test = data.frame(matrix(nrow=200, ncol=2))
colnames(df_test) = c('x','y')
for (i in 1:nrow(df_test)) df_test[i,] = lin_pair(2, -0.3, -1)
ggplot(data = df_test) + geom_point(aes(x,y)) 

# Quadratic pair function
# y = a*(x − b)^2 + c
quad_pair= function(a, b, c, label) {
  if (label != 1 & label != -1) stop('Argument "label" must equal 1 or -1')
  x = runif(1, min=0, max=1)
  y = runif(1, min=0, max=1)
  # Make sure that the point is above the line if label == 1
  # or below the line if label == -1
  # AND STILL IN THE ORIGINAL BOX
  if (label == 1) {
    while (y <= a*(x - b)^2 + c) {
      x = runif(1, min=0, max=1)
      y = runif(1, min=0, max=1)
    }
  } else if (label == -1) {
    while (y >= a*(x - b)^2 + c) {
      x = runif(1, min=0, max=1)
      y = runif(1, min=0, max=1)
    }
  }
  return(c(x,y))
}

dfquad_test = data.frame(matrix(nrow=200, ncol=2))
colnames(dfquad_test) = c('x', 'y')
for (i in 1:nrow(dfquad_test)) dfquad_test[i,] = quad_pair(-1, .5 , .25, -1)
  
ggplot(data = dfquad_test) + geom_point(aes(x,y))
  
# Load in the iris df, included in the R base package
df_iris = iris

# It's easy to separate setosa from the other two species, but not 
# versicolor and virginica from each other
sl_sw_plot = 
  ggplot(df_iris, aes(Sepal.Length, Sepal.Width, color=Species)) + 
  geom_point()

# Here it's easy to distinguish all 3 species
sl_pl_plot = 
  ggplot(df_iris, aes(Sepal.Length, Petal.Length, color=Species)) + 
  geom_point()

# There's a little overlap between versicolor and virginica, but 
# the three species are, for the most part, obviously clustered
sl_pw_plot = 
  ggplot(df_iris, aes(Sepal.Length, Petal.Width, color=Species)) + 
  geom_point()

# There's a little overlap between versicolor and virginica, but 
# the three species are, for the most part, obviously clustered
sw_pl_plot = 
  ggplot(df_iris, aes(Sepal.Width, Petal.Length, color=Species)) + 
  geom_point()

# There's a little overlap between versicolor and virginica, but 
# the three species are, for the most part, obviously clustered
sw_pw_plot = 
  ggplot(df_iris, aes(Sepal.Width, Petal.Width, color=Species)) + 
  geom_point()

# Setosa is very well spaced from the other two species.  It's 
# also mostly easy to distinguish versicolor from virginica.
pl_pw_plot = 
  ggplot(df_iris, aes(Petal.Length, Petal.Width, color=Species)) + 
  geom_point()

# Load in the wine data set
wine_df = read.csv('../data/wine.data')

wine_features = tail(colnames(wine_df),-1)

# Determine the mean of every feature column of the wine df, 
# grouped by Type
# The bottom command, i.e. with aggregate(), works just as well 
# as dplyr::summarize_each() for computing the means of each 
# column of a df grouped by type, but the dlpyr function is 
# more powerful, as it allows the user to compute multiple 
# grouped summary statistics simultaneously
col_means_by_type = group_by(wine_df, Type)  %>% dplyr::summarize_each(funs(mean))
#col_means_by_type = aggregate(x=wine_df[,wine_features],by=as.list(wine_df['Type']),FUN=mean)

# Generate simulated data with a linear and a quadratic separator
lin_df  = data.frame(matrix(nrow=200, ncol=3))
quad_df = data.frame(matrix(nrow=200, ncol=3))
colnames(lin_df)  = c('x','y','class')
colnames(quad_df) = c('x','y','class')
for (i in 1:200) {
  print(paste0('Starting i = ',i))
  if (i <= 100) {
    label = 1
    b = 0.05
    c = 0.4
  } else {
    label = -1
    b = -0.1
    c = 0.38
  }
  lin_df[i,]  = c(lin_pair(0.75, b, label), label)
  quad_df[i,] = c(quad_pair(4, 0.5, c, label), label)
}

lin_plot  = ggplot(lin_df,  aes(x,y, color=class)) + geom_point()
quad_plot = ggplot(quad_df, aes(x,y, color=class)) + geom_point()


# Use partimat() to run both LDA and QDA on both lin_df and quad_df

lda_partimat = 
  partimat(lin_df[,c('x','y')], as.factor(lin_df[['class']]), method="lda")
qda_partimat = 
  partimat(quad_df[,c('x','y')], as.factor(quad_df[['class']]), method="qda")

# Use lda() (from the MASS package) to make a classification model for the 
# three different wine cultivars in terms of the chemical properties of the 
# sampled wines.

wine_lda = lda(x=wine_df[,wine_features], grouping=as.factor(wine_df$Type))
wine_predict = predict(wine_lda)
wine_hist = ldahist(data=wine_predict$x, g=wine_df$Type)
wine_lda_df = cbind(data.frame(wine_predict$x), Type=wine_df$Type)

# Looking at wine_hist and wine_lda_df, we see that LDA isn't capable 
# of drawing perfect decision boundaries for the classes in wine_df

wine_lda_plot = ggplot(wine_lda_df, aes(LD1, LD2, color=Type)) + geom_point()

wine_pca = prcomp(wine_df[,wine_features])
wine_pca_df = cbind(data.frame(wine_pca$x)[,1:2], Type=wine_df$Type)
wine_pca_plot = ggplot(wine_pca_df, aes(PC1, PC2, color=Type)) + geom_point()


# Perceptrons
# First, let's get a feel for how they work
# To that end, let's generate 1000 points above the line 
# y = 1.5x + 0.2 and 1000 points below the line 
# y = 1.5x + 0.05

lin_mat  = matrix(nrow=2000, ncol=3)
View(lin_mat)
colnames(lin_mat)  = c('x','y','class')

for (i in 1:2000) {
  print(paste0('Starting i = ',i))
  if (i <= 1000) {
    label = 1
    b = 0.2
  } else {
    label = -1
    b = .05
  }
  lin_mat[i,]  = c(lin_pair(1.5, b, label), label)
}

lin_qplot  = qplot(x=x, y=y, data=as.data.frame(lin_mat), color=class)

# Add an intercept column, consisting only of 1s, to lin_mat
lin_mat = cbind(lin_mat, 'intercept'=rep(1,nrow(lin_mat)))

# Dot product of two vectors of the same length
dot = function(x,y) {
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sum(x*y))
}

# Perceptron function
# xs = matrix
# y  = vector of labels (all values +/- 1);
#      Must have length(y) = nrow(xs)
# w  = weights vector which parametrizes a line or
#      hyperplane dividing the two classes of data
#      Must have length(y) = ncol(xs)
# rate = single number which characterizes how much 
#        w can be altered
# seed = single number which seeds a random number 
#        generator
perceptron = function(xs, y, w, rate, seed) {
  if (!is.matrix(xs)) stop('xs must be a matrix')
  if (!is.vector(y))  stop('y must be a vector')
  if (!is.vector(w))  stop('w must be a vector')
  if (nrow(xs)  != length(y)) stop('nrow(xs) must equal length(y)')
  if (ncol(xs)  != length(w)) stop('ncol(xs) must equal length(w)')
  if (all(unique(y) != c(-1, 1)) & all(unique(y) != c(1, -1))) {
    stop('y must contain only values of +/- 1')
  }
  # Set the seed for reproducibility
  set.seed(seed)
  # Generate a random shuffling of the rows of xs
  shuff_rows = sample(1:nrow(xs), nrow(xs), replace=FALSE)
  # Iterate through the shuffled rows
  for (i in shuff_rows) {
    xi = xs[i,]
    yi = y[i]
    si = sign(dot(xi, w))
    # WARNING: sign(0) = 0!
    # Make logical vectors of false positives and false negatives
    # If false positive, update w one way
    # If false negative, update w a different way
    if (si != yi & yi == -1) w = w - rate*xi
    else if (si != yi & yi ==  1) w = w + rate*xi
  }
  return(w)
}


# Switch the 3rd and 4th columns of lin_mat (class and intercept, 
# respectively)
lin_mat = lin_mat[,c(1,2,4,3)]

# Function which takes points in a 2D feature space, colors them 
# according to actual class, and overlays a dividing line 
# parametrized by w, as computed by perceptron()
perceptron_plot = function(xs, y, w) {
  pp = 
    ggplot() + xlab('x') + ylab('y') +
    geom_point(aes(x=xs[,1], y=xs[,2], color=y)) + 
    geom_abline(slope = -w[1]/w[2], 
                intercept = -w[3]/w[2], 
                color = 'red') 
  return(pp)
}


# Run perceptron() with rate = 1 and seed = 6 repeatedly until
# w converges.  Call perceptron_plot() after each call to 
# perceptron() to view the intermediate results.
winit = c(0,0,0)
w = perceptron(lin_mat[,1:3], lin_mat[,'class'], winit, rate=1, seed=6)
perceptron_plot(lin_mat[,1:3], lin_mat[,'class'], w)
winit

# Function which initializes the weights vector w to 0, then 
# updates w with perceptron() until it converges
perceptron_conv = function(xs, y, rate, seed) {
  w0 = rep(0,ncol(xs))
  w  = perceptron(xs, y, w0, rate, seed)
  while (!all(w == w0)) {
    w0 = w
    w  = perceptron(xs, y, w0, rate, seed)
  }
  return(w)
}


rr_20 = sample(1:nrow(lin_mat), 20, replace=FALSE)
w = perceptron_conv(lin_mat[rr_20,1:3], lin_mat[rr_20,'class'], rate=1, seed=4)
perceptron_plot(lin_mat[rr_20,1:3], lin_mat[rr_20,'class'], w)



# Write a function perceptron_time(xs, y, rate) which runs perceptron_conv() 
# 10 times, with the random seed set to 1 through 100, and times each 
# iteration with the tictoc package. It should return the average time elapsed 
# over the 100 iterations. Use perceptron_time() to explore how the average 
# convergence time varies for both your large and small simulated datasets as 
# you vary the rate parameter over the range 10ˆseq(-1, 2, length.out=20).
perceptron_time = function(xs, y, rate) {
  time_vec = rep(NA, 100)
  for (i in 1:100) {
    tic(quiet = TRUE)
    w = perceptron_conv(xs, y, rate, seed=i)
    stoptime = toc(quiet = TRUE)
    elapsed_time = stoptime$toc - stoptime$tic
    names(elapsed_time) = NULL
    time_vec[i] = elapsed_time
  }
  return(mean(time_vec))
}

# Examine the effect of the rate parameter on the average convergence time
rate_vec = 10^seq(-1, 2, length.out=20)
avg_time_vec = rep(NA,20)
for (i in 1:20) {
  #print(paste0('Starting i = ',i))
  avg_time_vec[i] = 
    perceptron_time(lin_mat[,1:3], lin_mat[,'class'], rate_vec[i])
}
rate_time_df = data.frame(rate=rate_vec, time=avg_time_vec)

rate_time_plot = 
  ggplot(data=rate_time_df) + 
  geom_point(aes(x=rate, y=time), color='blue')

# For testing purposes, initialize and populate a matrix of 
# 2000 x-y pairs, half of them above a quadratic curve, and 
# half below a slightly lower quadratic curve, i.e. half 
# above and half below a quadratic buffer zone
quad_mat  = matrix(nrow=2000, ncol=4)
View(quad_mat)
colnames(quad_mat)  = c('x','y','intercept','class')
for (i in 1:2000) {
  if (i <= 1000) {
    label = 1
    c = 0.3
  } else {
    label = -1
    c = 0.25
  }
  quad_mat[i,]  = c(quad_pair(-1, .5 , c, label), 1, label)
}


# Run perceptron() with rate = 1 and seed = 6 repeatedly to show
# that w either never converges or converges improperlly when the 
# data has a nonlinear decision boundary.  Call perceptron_plot() 
# after each call to perceptron() to view the intermediate results.
winit = c(0,0,0)
w = perceptron(quad_mat[,1:3], quad_mat[,'class'], winit, rate=1, seed=6)
perceptron_plot(quad_mat[,1:3], quad_mat[,'class'], w)
w
winit = w


# SVMs!! 
# Return to the linearly separable data with 2000 points which you 
# initially used for the perceptron. Remove the column of 1s, convert 
# the matrix into a data frame, add on a column with the class labels 
# (which should be ±1), and convert the class label column into a 
# factor.
# Overwrite lin_df, which has 200 rows.  Maybe reintialize and rename 
# it later.  
lin_df = data.frame(lin_mat)
lin_df$intercept = NULL
lin_df$class = as.factor(lin_df$class)
lin_svm = svm(class ~ ., data=lin_df, kernel='linear')
plot(lin_svm, lin_df)

lin_df2  = data.frame(matrix(nrow=10000, ncol=3))
colnames(lin_df2)  = c('x','y','class')
for (i in 1:10000) {
  #print(paste0('Starting i = ',i))
  if (i <= 5000) {
    label = 1
    b = 0.2
  } else {
    label = -1
    b = 0.05
  }
  lin_df2[i,]  = c(lin_pair(1.5, b, label), label)
}

lin_df2$class = as.factor(lin_df2$class)
lin_svm = svm(class ~ ., data=lin_df2, kernel='linear')
plot(lin_svm, lin_df2)

# For each cost in cost_vec, plot the result of an SVM fit 
# with a linear kernel
cost_vec  = 10^seq(-1,2,by=1)
for (i in 1:length(cost_vec)) {
  cost = cost_vec[i]
  lin_svm_i = svm(class ~ ., data=lin_df, cost=cost, kernel='linear')
  plot(lin_svm_i, lin_df)
  title(sub=paste0('cost = ',cost))
}

# Quadratic decision boundary
quad_df2 = data.frame(matrix(nrow=2000, ncol=3))
colnames(quad_df2) = c('x','y','class')
for (i in 1:2000) {
  #print(paste0('Starting i = ',i))
  if (i <= 1000) {
    label = 1
    c = 0.55
  } else {
    label = -1
    c = 0.4
  }
  quad_df2[i,] = c(quad_pair(3, 0.5, c, label), label)
}

quad_df2$class = as.factor(quad_df2$class)

# DO NOT use kernel='linear' here. svm() will try to draw 
# a linear decision boundary, which doesn't make sense here.
quad_svm = svm(class ~ ., data=quad_df2)
plot(quad_svm, quad_df2)


# Let's see if svm() supports multiclass classification by trying 
# to predict the iris species from just the sepal and petal lengths
iris_sl_pl_svm = svm(Species ~ ., 
                     data=df_iris[,c('Sepal.Length','Petal.Length','Species')])
plot(iris_sl_pl_svm, df_iris[,c('Sepal.Length','Petal.Length','Species')])

# Now let's try it with wine
# Hmm, for some reason, plot() isn't generating any output
ggplot(wine_df, aes(Alcohol, Color, color=Type)) + geom_point()
wine_alc_dil_svm = 
  svm(Type ~ ., data=wine_df[,c('Alcohol','Dilution','Type')])
plot(wine_alc_dil_svm, wine_df[,c('Alcohol','Dilution','Type')])


# Higher gamma values ==> model is prone to overfitting
# Default is 1/(number of feature columns)
iris_sl_pl_svm_2 = svm(Species ~ ., 
                       data=df_iris[,c('Sepal.Length','Petal.Length','Species')],
                       gamma=4)
plot(iris_sl_pl_svm_2, df_iris[,c('Sepal.Length','Petal.Length','Species')])

# Let's see what happens when we change the kernel type, gamma value, 
# and cost for the data with a quadratic decision boundary
quad_svm2 = svm(class ~ ., data=quad_df2, kernel='radial', gamma=1/4, cost=10)
plot(quad_svm2, quad_df2)

# For each gamma in gamma_vec and each cost in cost_vec, plot the result 
# of an SVM fit with a radial (Gaussian) kernel
gamma_vec = 2^seq(-3,3,by=1)
cost_vec  = 10^seq(-2,0,by=1)
for (i in 1:length(gamma_vec)){
  gammai = gamma_vec[i]
  for (j in 1:length(cost_vec)) {
    cost = cost_vec[j]
    quad_svm_ij = 
      svm(class ~ ., data=quad_df2, kernel='radial', cost=cost, gamma=gammai)
    plot(quad_svm_ij, quad_df2)
    title(sub=paste0('gamma = ',gammai,', cost = ',cost))
  }
}

# All right, it's clear from looking at the plot below that this svm() 
# function isn't smart enough to find the coefficients of a polynomial 
# kernel, making it impractical to use in a predictive model.  
# The entire point is to find a model that classifies well even when 
# we don't have a priori knowledge of the distribution of that data.
quad_svm_poly = svm(class ~ ., data=quad_df2, kernel='polynomial', degree=2)
plot(quad_svm_poly, quad_df2)



