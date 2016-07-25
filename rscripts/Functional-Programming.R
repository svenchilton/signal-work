# Kurt Brown and Sven Chilton
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')
lapply_test = function() {
  l = lapply(mtcars, class)
  return(unlist(l))
}

lapply_test()

standardize_mtcars = function() data.frame( lapply( mtcars, function(x) ( ( x - mean(x) ) /sd(x) ) ) )
x = standardize_mtcars()
is.data.frame(x)
dim(x) == dim(mtcars)
mean(mtcars$mpg)
sd(mtcars$mpg)
(mtcars[1,'mpg'] - mean(mtcars$mpg))/sd(mtcars$mpg)
x

df = data.frame(matrix(1:100, nrow=10))
df
df[1:5] = lapply(df[1:5], as.character)
df
df[,1]
df[,6]

standardize_df = function() data.frame( lapply( df, function(x) ( ( x - mean(x) ) /sd(x) ) ) )
standardize_df()

is.numeric(as.matrix(df))


nums = lapply(df, is.numeric)
unlist(nums)
df[nums]

stdize_numeric_cols = function(df) {
  # Find the numeric columns
  nums = unlist(lapply(df, is.numeric))
  # Run lapply() on the numeric columns to standardize them, i.e. 
  # subtract the mean and divide by the standard deviation
  df[nums] = data.frame( lapply( df[nums], function(x) ( ( x - mean(x) ) /sd(x) ) ) )
  return(df)
}

stdize_numeric_cols(df)

basic_lapply = function(argz, func) {
  l = vector('list',length(argz))
  for (i in 1:length(argz)) {
    l[[i]] = func(argz[[i]])
    names(l)[i] = names(argz[i])
  }
  return(l)
}

basic_lapply(1:5, function(x) 2*x)


basic_std_mtcars = function() data.frame( basic_lapply( mtcars, function(x) ( ( x - mean(x) ) /sd(x) ) ) )
bsm = basic_std_mtcars()
bsm

# Here's a situation where you need a loop rather than lapply()
df = data.frame(matrix(1:100, nrow=10))

# This function takes a df and for all columns after the first, subtracts the 
# values of the (original) previous column from the current column
# Once one column is modified, it does not affect subsequent column modifications
minus_prev_col = function(df) {
  for (i in ncol(df):2){
    df[,i] = df[,i] - df[,(i - 1)]
  }
  return(df)
}

minus_prev_col(df)


mat = matrix(1:9, nrow=3)
mat

sapply(mat, function(x) x^2)
unlist(lapply(mat, function(x) x^2))
vapply(mat, function(x) x^2, c(0,0,0))
vapply(data.frame(mat), function(x) x^2, c(0,0,0))
vapply(data.frame(mat), function(x) x^2, 0)

outer(1:3, 1:5)


L = lapply(1:5, function(x) sample(c(1:4, NA)))

mean_lv = function(lv) sapply(lv, function(x) mean(x, na.rm = TRUE))
mean_lv(L)

multiply = function(x, k=2) k*x

sapply(1:10, multiply, k=10)
vapply(1:10, multiply, 0, k=10)

mat^2
square = function(x) x^2
square(mat)


# No for loop needed 
df
colnames(df) = sapply(colnames(df), function(x) paste(x, 'n', sep='_'))
df

x = 10:100
y = x^3 + 4*x^2
sum(y)

x = (30:60)/10

sapply(x, function(x) exp(x)*cos(x))
sum(sapply(y, function(x) sum(x)))


sum(sapply(10:100, function(i) (i^3 + 4*i^2)))



m = matrix(1:9, nrow=3)
apply(m, 1, mean)
apply(m, c(1,2), function(x) x^2)


values = lapply(1:10, function(x) rnorm(10))
weights = lapply(1:10, function(x) rnorm(10))

values
weights

weighted_mean_func = function(values, weights, na.rm=FALSE) {
  if (!is.list(values))  stop('Argument "values" must be a list')
  if (!is.list(weights)) stop('Argument "weights" must be a list')
  wmean = Map(weighted.mean, values, weights, na.rm=na.rm)
  return(wmean)
}

wmean = weighted_mean_func(values, weights)
wmean

weighted.mean(values[[10]], weights[[10]])

# Reduce
reduce_sum = function(vec) Reduce(function(x, y) x+y, vec)
reduce_sum(1:10)
reduce_sum(values[[10]])
sum(values[[10]])


union(c(1,2), c(3,4))
intersect(c(1,2,3,4,5), c(2,4,6,8))
my_union = function(L) Reduce(union, L)
my_intersect = function(L) Reduce(intersect, L)
L = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12))
L = list(c(1,2,3,4), c(6, 3, 1, 7, 9), c(1, 3, 5, 7))
my_union(L)
my_intersect(L)

bi_reduce = function(x) {
  left_reduce  = Reduce(x, right=FALSE) # Default
  right_reduce = Reduce(x, right=TRUE)
  if (left_reduce == right_reduce) {
    return(left_reduce)
  } else {
    return(NA)
  }
}

# Basic reduce function
basic_reduce = function(func, argz, right=FALSE) {
  if (right) argz=rev(argz)
  num_args = length(argz)
  if (num_args < 2) {
    stop('argument list "argz" must contain at least two arguments')
  } else if (num_args == 2) {
    a1 = argz[[1]]
    a2 = argz[[2]]
    return(func(a1,a2))
  } else {
    a1 = argz[[1]]
    a2 = argz[[2]]
    a1 = func(a1, a2)
    for (i in 3:num_args) {
      a2 = argz[[i]]
      a1 = func(a1,a2)
    }
    return(a1)
  }
}

L
my_basic_intersect = function(L) basic_reduce(intersect, L)
my_basic_intersect(L)


# Filter(), Find(), and Position()
is.even = function(xint) {
  if(!is.double(xint) & !is.integer(xint)) stop('argument "xint" must be a double or integer literal')
  rem = xint %% 2
  if (rem == 0) return(TRUE)
  else return(FALSE)
}
is.even(9)
is.even(8)

x = (1:10)*3
Filter(is.even,x)
Find(is.even,x)
Position(is.even,x)
Find(is.even,x,right=TRUE)
Position(is.even,x,right=TRUE)

y = (1:10)*2-1
y
is.na(Position(is.even,y))

Any = function(f, x) {
  if (is.na(Position(f,x))) return(FALSE)
  else return(TRUE)
}

All = function(f, x) {
  if (length(Filter(f,x)) == length(x)) return(TRUE)
  else return(FALSE)
}

z = y+1
All(is.even,z)











