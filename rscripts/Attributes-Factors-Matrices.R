# Kurt Brown and Sven Chilton
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

attributes(mtcars)
attr(mtcars)
attr(mtcars, 'names')
?factor

double_names = function(vec) {
  dvec = vec
  for (i in 1:length(vec)) {
    name = vec[i]
    dvec[i] = paste(name,name,sep = '')
  }
  return(dvec)
}

double_names(attr(mtcars, 'names'))

double_names(names(mtcars))

bgvec = c('boy','girl','girl',NA)
factor(bgvec, levels='boy')

f1 = factor(letters)
f1

f2 = rev(factor(letters))
f2

f3 = factor(letters, levels = rev(letters))
f3

fruits = c("apple", "grapefruit", "NA", "apple", "apple", "-", "grapefruit", "durian")

factor(fruits,exclude=c('NA','-'))


factor_inc_na = function(vec) {
  fac = factor(vec)
  fac = addNA(fac,ifany=TRUE)
  return(fac)
}

factor_inc_na(bgvec)

df = data.frame(matrix(1:100,nrow=10))
df

low_cols_to_factors = function(df) {
  max_col = floor(ncol(df)/2)
  for (i in 1:max_col) {
    df[,i] = factor(df[,i])
  }
  return(df)
}

df = low_cols_to_factors(df)
levels(df[,1])

length(unique(bgvec))


factorize_boring = function(df) {
  for (i in 1:ncol(df)) {
    if (length(unique(df[,i]) <= 5)) {
      df[,i] = factor(df[,i])
    }
  }
  return(df)
}

df = data.frame(matrix(1:25, nrow=5))
df

df = factorize_boring(df)
df
levels(df$X1)

df = data.frame(gender=c('m','f','f',NA,'m','f'), 
                first_name=c('Kurt', 'Angelina', NA, 'Kurt','Kurt','Angelina'), 
                last_name=c('Brown', 'Jolie', 'Jolie', 'Jolie', NA, 'Brown'))
df
levels(df[,1])
levels(df[,2])
levels(df[,3])

is.factor(df[,3])

names(sort(table(df[,1]), decreasing = TRUE)[1])

impute = function(df) {
  for (i in 1:ncol(df)) {
    if (is.factor(df[,i])) {
      # Replace any NA's in the given factor column with the most common value
      # in that column
      df[,i][is.na(df[,i])] = names(sort(table(df[,i]), decreasing = TRUE)[1])
    }
  }
  return(df)
}

impute2 = function(df) {
  for (i in 1:ncol(df)) {
    if (is.factor(df[,i])) {
      # Create a logical vector of the NA values in the column
      nans = is.na(df[,i])
      # Count the NA values
      na_count = sum(is.na(df[,i]))
      # Replace any NA's in the given factor column with random values chosen 
      # from the levels of that column, distributed according to their frequency 
      # in the the column
      df[,i][nans] = sample(df[,i][!nans], na_count, replace = TRUE)
    }
  }
  return(df)
}

df
dfa = df
dfa[,2] = data.frame(last_name_Brown=c(1,0,0,0,0,1), last_name_Jolie=c(0,1,1,1,0,0))


factor_to_binary = function(df) {
  l = list()
  for (i in 1:ncol(df)) {
    cc = df[,i]
    cc_name = colnames(df)[i]
    if (!is.factor(df[,i])) {
      l[[cc_name]] = cc
    } else {
      levs = levels(cc)[-1]
      cc_df = data.frame(matrix(nrow=nrow(df), ncol=length(levs)))
      for (j in 1:length(levs)) {
        lev = levs[j]
        cc_df[,j] = as.numeric(as.character(cc) == lev)
        colnames(cc_df)[j] = paste(cc_name, lev, sep='_')
      }
      l[[cc_name]] = cc_df
    }
  }
  df = do.call(cbind, l)
  #df = data.frame(lapply(l, cbind))
  # Replace the erroneous column names
  colnames(df) = gsub(".*\\.","",colnames(df))
  return(df)
}

df = factor_to_binary(df)
df

load('/Users/svenchilton/GitHub/signal-data-science/R-curriculum/datasets/time/time.dat')
View(df)
levels(df[,1])
levels(df[,2])

as.character(df[,1])
for (i in 1:ncol(df)) {
  df[,i] = as.character(df[,i])
}

View(df)
df[1,1]

dfa = df

# Make sure only to call this on a string of the format 
# 'XX:XX', where X is an integer, and the third X ranges 
# from 0 to 5
# The output will be a floating point number with the 
# integer component representing the hour and the decimal 
# representing the fraction of the hour (rather than the minute)
time_string_to_number = function(time_str) {
  time_str = gsub(':','.',time_str)
  time_num = as.numeric(time_str)
  minute = time_num %% 1
  hour = time_num - minute
  # Convert the minute to a decimal fraction
  minute = minute*5/3
  time_num = hour + minute
  return(time_num)
}

for (j in 1:ncol(dfa)) {
  for (i in 1:nrow(dfa)) {
    bedtime = dfa[i,j]
    hour = as.numeric(substr(bedtime,1,2))
    ampm = substr(bedtime,nchar(bedtime),nchar(bedtime))
    if (hour == 12) {
      if (ampm == 'A') {
        bedtime = gsub('A','',bedtime)
        bedtime = time_string_to_number(bedtime)
        bedtime = bedtime - 12
      } else if (ampm == 'P') {
        bedtime = gsub('P','',bedtime)
        bedtime = time_string_to_number(bedtime)
      }
    } else if (hour < 12) {
      if (ampm == 'A') {
        bedtime = gsub('A','',bedtime)
        bedtime = time_string_to_number(bedtime)
      } else if (ampm == 'P') {
        bedtime = gsub('P','',bedtime)
        bedtime = time_string_to_number(bedtime)
        bedtime = bedtime + 12
      }
    } else {
      bedtime = NA
    }
    dfa[i,j] = bedtime
  }
  # Convert the column to a vector of numeric values
  dfa[,j] = as.numeric(dfa[,j])
}

# Remove the NA's from our converted data frame
dfa = na.omit(dfa)

# Verify that our data makes sense
max(dfa[,1])
max(dfa[,2])
min(dfa[,1])
min(dfa[,2])

# Make a new data frame diff defined such that each entry is the number of hours 
# before (negative) or after (positive) 8:00 PM the person went to bed
diff = dfa
for (j in 1:ncol(dfa)) {
  diff[,j] = dfa[,j] - 20
  diff[,j][diff[,j] < -12] = diff[,j][diff[,j] < -12] + 24
}

ggplot() + 
  geom_histogram(aes(diff[,1]), fill='red', alpha=0.5, bins=48) + 
  geom_histogram(aes(diff[,2]), fill='blue', alpha=0.5, bins=48)





# Matrices!  Hallelujah!

m = matrix(1:100, nrow=10)
as.numeric(m)
m1 = matrix(1:100, nrow=4)
m1
ncol(m1)

dim_swap = function(x) {
  m = nrow(x)
  n = ncol(x)
  x = as.numeric(x)
  x = matrix(x, nrow=n)
  return(x)
}

dim_swap(m1)

df = data.frame(matrix(1:100, nrow=10)) 
df[5, 5] = NA
df[6, 6] = NA

df
is.numeric(df[,1])
is.numeric(df[,5])

df[,3][(df[,3] %% 3) == 0]

vec = c(1,2,3)
subvec = c(4,5,6)
vec[(length(vec)+1):(length(vec)+length(subvec))] = subvec
vec

multiples_of_k = function(df, k) {
  vec = c()
  for (j in 1:ncol(df)) {
    if (is.numeric(df[,j])) {
      subvec = df[,j][(df[,j] %% k) == 0]
      subvec = na.omit(subvec)
      if (length(subvec) > 0) {
        vec[(length(vec)+1):(length(vec)+length(subvec))] = subvec
      }
    }
  }
  return(vec)
}

vec = multiples_of_k(df,8)
vec


min_matrix = function(m, n) {
  mat = matrix(nrow=m, ncol=n)
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      mat[i,j] = min(i,j)
    }
  }
  return(mat)
}

min_matrix(8,8)

is_symmetric = function(mat) {
  return(identical(mat, t(mat)))
}

mm_88 = min_matrix(8,8)
is_symmetric(mm_88)
identical(mm_88, t(mm_88))


mm_34 = min_matrix(3,4)
sum(diag(mm_34))

trace = function(mat) {
  return(sum(diag(mat)))
}


trace(2*mm_34)
mm_34_mod = mm_34 + 5
mm_34_mod
trace(mm_34+mm_34_mod)
trace(mm_34) + trace(mm_34_mod)

i_3 = diag(3)
i_3 * mm_34
mm_33 = min_matrix(3,3)
i_3 * mm_33
mm_33

mm_33 * i_3

mm_33 %*% i_3

matrix_mult = function(A,B) {
  m = nrow(A)
  n = ncol(A)
  o = nrow(B)
  p = ncol(B)
  C = matrix(nrow=m, ncol=p)
  if (n == o) {
    for (i in 1:m) {
      for (k in 1:p) {
        C[i,k] = sum(A[i,] * B[,k])
      }
    }
    return(C)
  } else {
    stop('The number of columns of A must equal the number of rows of B')
  }
}

matrix_mult(i_3, mm_33)

num_rows = 100
num_cols = 100
rmat1 = matrix(runif(num_rows*num_cols), nrow=num_rows)
rmat2 = matrix(runif(num_rows*num_cols), nrow=num_rows)

their_time = system.time({
  rmat1 %*% rmat2
})
our_time = system.time({
  matrix_mult(rmat1, rmat2)
})
their_time
our_time
