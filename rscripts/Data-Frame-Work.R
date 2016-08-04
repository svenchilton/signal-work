# Kurt Brown and Sven Chilton
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')
# Names
vec1 = c(name1=1, name2=2, name3=3)
names(vec1)

# Apparently, multiple values can have the same name
# That seems like a really bad idea
vec2 = c(name1=1, name1=2)
names(vec2)

# If you haven't specified the names of your vector's components, 
# calling names() on the vector will yield NULL
vec3 = c(1,2,3)
names(vec3)

# Doesn't look like you can use logical variables as names in vectors
vec4 = c(TRUE=1, FALSE=0, 123=100000, 'asdf'=1234)

# You can't use numbers as names either
vec5 = c(123=100000, 'asdf'=1234)

# It appears not to matter whether you put quotation marks around 
# non-numerical vector component names
vec6 = c('asdf'=1234, 'hjkl'=7890)
names(vec6)
vec7 = c(asdf=1234, hjkl=7890)
names(vec7)

# What happens if you convert a number to a string and use that 
# as a component name?
# Cool, that works
vec8 = c('123'=123, '789'=789)
names(vec8)

# What happens when the vector has fewer names than components?
# Looks like any unnamed components are interpreted as having empty 
# strings as names
vec9 = c(name1 = 1, name2 = 2, 3)
names(vec9)

# What about more names than components?
vec10 = c(1, 2)
names(vec10) = c("a", 'b', 'c')
vec10

# We can assign names to the components after the fact, 
# but the number of names must be less than or equal to the
# number of components.  In the former case, any unnamed 
# components will be given null names.

# Subsetting
vec8['123']
vec1[c('name3', 'name1')]

# What happens when more than 1 element has the same name?
# Apparently, we get the first value with that name
vec2['name1']

# Lists

# You can convert a list to a vector with unlist()
l1 = list(1,2)
v1 = unlist(l1)
typeof(v1)

# This even works if the list contains elements of different types, 
# though they will be converted according to the usual c() rules
l2 = list(1,'a')
v2 = unlist(l2)
v2

# as.vector() won't convert a list to a vector because it creates 
# column rather than row vectors

# Running unlist() on a nested list will flatten the list
l3 = list(1,2,list(3,4))
unlist(l3)

# List indexing
# list_name['index_name'] returns both the index_name and the value
# list_name$index_name returns only the value
x = list(a = 1, b = 2, c = 3)
x['a']
x$a

# Lists inside vectors
linv = c(list(1,2), list(3,4,5))
linv

# Vectors inside lists
vinl = list(c(1,2), c(3,4,5))
vinl

# Subsetting lists
ll = list(4,3,1,7,8)
ll[2:4]
ll2 = list(4,3,list(1,2),'a', 'b', list('c', 5))
names(ll2) = c('name1', 'name2', 'name3', 'name4', 'name5','name6')
ll2[2:4]
ll2[2:4][2]
ll2[2:4][[2]]

# Data frames

df = data.frame(matrix(1:100, nrow=10))
View(df)
df$X1
df['X1']
df[[1]]
df[2,]
typeof(df)
class(df)

# Names of columns and rows
names(df) # This appears to be the same as colnames() for our purposes
colnames(df)
rownames(df)

# Dimensions
# Number of rows
nrow(df)
# Number of columns
ncol(df)

# Converting lists and vectors to data frames
lca = list(col1=c(11,21), col2=c(12, 22), col3=c(13,23))
lca
dfa = data.frame(lca)
#colnames(dfa) = c('col1','col2','col3')
View(dfa)

# We cannot have differing numbers of rows in the list 
# which we convert to a data frame
# I.e. all columns must have the same length
# I had thought that the data frame constructor would 
# fill in missing values with NA's, but apparently not
lcb = list(col1=c(11,21), col2=c(12, 22), col3=c(13,23,33))
dfb = data.frame(lcb)
View(dfb)

# Column and row binding
df1020 = cbind(df,df)
dim(df1020)
df4010 = rbind(df,df,df,df)
dim(df4010)
df10100 = do.call(cbind, rep(df,10))
dim(df10100)

# What happens when one of your columns consists of characters?
lcc = list(col1=c(1,2), col2=c('a','b'))
dfc = data.frame(lcc)
View(dfc)

# Empty data frame
# Calling View() on it won't work
el_no_rows = list(col1=c())
df_no_rows = data.frame(el_no_rows)
df_no_rows

# Subsetting
x = 1:5
x[c(3,1)]
x[-c(3,1)]
x[c(-3,-1)]
x[-1]
x[c(TRUE, FALSE, TRUE, FALSE,TRUE)]
x[c(2,2)]
x[c(1,1.5)]
x[c(1,1.8)]
x[c(1,2.8)]
# This doesn't work.  Only 0's may be mixed with negative subscripts.
x[c(1,2,-3)]
# When you subset with a vector of logicals shorter than the original vector, 
# the T/F pattern specified in the subset repeats until the end of the 
# original vector is reached
x[TRUE]
x[c(TRUE,FALSE)]
x[c(TRUE,TRUE,FALSE)]
# Using NA as an index
x[NA]
x[c(1,3,NA)]
names(x) = c('a','b','c','d','e')
x['a']
x[c('a','e')]
x[c('a','e','f')]


lx = list(1,2,3,4,5)
lx[3] = list(1:5)
lx

ly = list(1:10)
ly[20] = 20
ly

# If you extract a column this way, 
# it looks like a column
df[2]
# If you extract a column this way, 
# it looks like a row vector
df[,2]

# Supplemental exercises

# Lists

test_list = list(1, list(2,3), list(4,list(5,list(6,7))) )

for (el in test_list) {
  print(class(el))
}
class(test_list)

# Function for finding the nesting depth of a list
nesting_depth = function(L, depth=1) {
  # Set the max depth to the current depth
  max_depth = depth
  for (el in L) {
    if (class(el) == 'list') {
      # If the depth of the current branch is greater than 
      # the current max depth, update it
      n_depth = nesting_depth(el, depth+1)
      if (n_depth > max_depth) {
        max_depth = n_depth
      }
    }
  }
  return(max_depth)
}

nesting_depth(test_list)




# Function for computing the set of unique n-dominoes
ndomino = function(n) {
  doms = list()
  for (i in 1:n) {
    for (j in i:n) {
      l = length(doms)
      doms[[l+1]] = list(i,j)
    }
  }
  return(doms)
}

dom4 = ndomino(4)
dom4[[6]]

# This is a faster way to calculate the set of n-dominoes, 
# since we're initializing a list with the appropriate 
# number of elements rather than appending at each iteration
new_ndomino = function(n) {
  # The number of elements in the list we return 
  # will equal the nth triangular number, n*(n+1)/2
  num_doms = n*(n+1)/2
  # Initialize a list with num_doms elements
  doms = vector("list", num_doms)
  # Populate the list of dominos
  el = 1
  for (i in 1:n) {
    for (j in i:n) {
      doms[[el]] = list(i,j)
      el = el+1
    }
  }
  return(doms)
}

new_dom4 = new_ndomino(4)
new_dom4[[6]]

# Just to verify that new_ndomino() is faster than ndomino()
t_ndom = system.time({
  n20 = ndomino(200)
})

t_new_ndom = system.time({
  nn20 = new_ndomino(200)
})

t_ndom
t_new_ndom

testl = list(list(1, 2), list(2, 3), list(3, 1))
testv = unlist(testl)
testv
testv = c(tail(testv,1), head(testv, -1))
odd = (1:length(testl))*2 - 1
testv[odd]
even = odd+1
all(testv[even] == testv[odd])
max(testv)
length(testl)
testl[-1]
testl[-length(testl)]

testl[-length(testl)][[1]]
testl[-length(testl)][[1]][[1]]

# This is wrong.  If the list has length 2 or greater, it only 
# checks that the ends of the circle and the first link (second 
# element of the first domino and first element of the second 
# domino) are equal.
## Function for determining whether a list L is a valid circle 
## of dominoes
#is_circle = function(L) {
#  # Check whether the list contains more than 1 domino
#  # If it only contains 1 domino, the list is a circle 
#  # if the domino is a double domino, e.g. list(2,2)
#  l = length(L)
#  if (l == 1) {
#    d1 = L[[1]]
#    return(d1[[1]] == d1[[2]]) 
#  } else {
#    first_el = L[[1]][[1]]
#    last_el  = L[[l]][[2]]
#    for (i in 1:(l-1)) {
#      d1  = L[-l][[i]]
#      d2  = L[-1][[i]]
#      d12 = d1[[2]]
#      d21 = d2[[1]]
#      if ( (d12 == d21) & (first_el == last_el) ) {
#        return(TRUE)
#      } else {
#        return(FALSE)
#      }
#    }
#  }
#}

# Function for determining whether a list L is a valid circle 
# of dominoes
is_circle = function(L) {
  # Ensure that the argument L is a list
  if (!is.list(L)) stop('Argument "L" must be a list')
  # Check that each element of L is either a list or 
  # vector of length 2
  for (i in length(L)) {
    el = L[[i]]
    if (!is.list(el) & !is.vector(el)) {
      error_message = paste0('Element ',i,' of argument "L" must be a list or vector')
      stop(error_message)
    }
    if (length(el) != 2) {
      error_message = paste0('Element ',i,' of argument "L" must be of length 2')
      stop(error_message)
    }
  }
  # Flatten the list
  v = unlist(L)
  # Bring the last element of v to the front
  v = c(tail(v,1), head(v, -1))
  # Find the even and odd indices of the new v
  even = (1:length(L))*2
  odd  = even-1
  # Return true iff every odd element in v equals every even element
  return(all(v[odd] == v[even]))
}

L = list(c(1, 2), c(2, 3), c(3, 1))
is_circle(L)

L = list(list(5,5), list(5,4), list(4,4), list(4,3), list(3,3), 
         list(3,2), list(2,2), list(2,1), list(1,1), list(1,3),
         list(3,5), list(5,2), list(2,4), list(4,1), list(1,5))
is_circle(L)

# Supplemental data frame work
x = 1:5
x[1:2] = c(10,11)
x

x = 1:10
x[c(FALSE, TRUE)] = 100
x

dim(mtcars)
mtcars[1:20]
mtcars[1:20,]
mtcars[1:10]
class(mtcars)

df = data.frame(c(1,2,NA), c(4,5,6))
df
df[is.na(df)] = 0
df
colnames(df) = c('col2','col1')
df
colnames(df)
colnames(df)[order(colnames(df))]
df[colnames(df)[order(colnames(df))]]

# Function which returns a data frame with the columns reordered 
# alphabetically
# Note: this returns a copy of df.  It does not reorder the columns 
# in place.
order_column_names = function(df) {
  # First get the vector of the numbers of the columns in their 
  # proper order
  ordered_col_nums = order(colnames(df))
  # Then subset the column name vector with the vector above 
  # to get the column names in order
  ordered_col_names = colnames(df)[ordered_col_nums]
  # Then subset the data frame with the column names in the proper order
  return(df[ordered_col_names])
}

order_column_names(df)
df

x = c("a", "b", "a", "a", "b", "x", "b", "a")
fruits = c("a"='apple', 'b'='banana')
fruits[x]

reorder_df = function(df, rows=FALSE) {
  if (rows) {
    return(df[sample(rownames(df)),sample(colnames(df))])
  } else {
    return(df[,sample(colnames(df))])
  }
}

k_random_cols = function(df, k) {
  return(sample(df, k, replace=TRUE))
}

m_continuous_rows = function(df, m) {
  max_row = nrow(df) - (m-1)
  start_row = sample(1:max_row, 1)
  return(df[start_row:(start_row+m-1),])
}

m_continuous_rows(df,4)

dfa = df
colnames(dfa) = c('a','b','c','d','e','a','b','c','d','e')
dfa

'a' %in% colnames(dfa)
dfa[colnames(dfa) != 'a']

remove_col_by_name = function(df, colname) {
  return(df[colnames(df) != colname])
}

remove_col_by_name(dfa, 'b')

grep('a', 'name')
grep('e', c('pepper', 'steak', 'x'))
grep('e', strsplit('pepper', ''))
pvec = unlist(strsplit('pepper', ''))
pvec
length(grep('e', pvec))

colnames(df)
xvec = colnames(df)
xvec
char_vec = unlist(strsplit(xvec, ''))
char_vec
length(grep('x', char_vec, ignore.case = TRUE))

char_count = function(df) {
  # Initialize the vector which keeps count of the number of times 
  # each letter appears in the column names
  count_vec = rep(0,26)
  names(count_vec) = letters
  # Create a vector with each character in the column names 
  # of df as a separate element
  char_vec = unlist(strsplit(colnames(df), ''))
  # For each character in letters (a pre-set vector of characters
  # 'a':'z'), count the number of times the character appears in 
  # the character vector created above and append it to the character 
  # count vector
  for (l in letters) {
    cc = length(grep(l, char_vec, ignore.case = TRUE))
    count_vec[l] = cc
  }
  return(count_vec)
}

char_count(df)

dfa
cat('x','y')
paste('x','y', sep='')

cols = as.character(1:10)
for (i in 1:length(cols)) {
  cols[i] = paste('col', cols[i])
}
cols
colnames(dfa) = cols
dfa

# Note: this modifies the column names in place
mod_col_names = function(df) {
  colnames(df) = gsub(' ','.',colnames(df))
  colnames(df) = paste(colnames(df), '_mod', sep='')
  return(df)
}

mod_col_names(dfa)

# Spiral function and constituent functions
get_edge = function(mat, dir, clockwise=FALSE) {
  if (!is.matrix(mat)) stop('mat must be a matrix')
  if ((!is.double(dir) & 
       !is.integer(dir)) | 
      (0 > dir) | (dir > 3)) {
    stop('dir must be a double or integer between 0 and 3, inclusive')
  }
  if (!clockwise) {
    # dir = c(0,1,2,3) <==> (down,right,up,left)
    if (dir == 0) vec = mat[,1]
    else if (dir == 1) vec = mat[nrow(mat),]
    else if (dir == 2) vec = rev(mat[,ncol(mat)])
    else if (dir == 3) vec = rev(mat[1,])
  } else {
    # dir = c(0,1,2,3) <==> (right,down,left,up)
    if (dir == 0) vec = mat[1,]
    else if (dir == 1) vec = mat[,ncol(mat)]
    else if (dir == 2) vec = rev(mat[nrow(mat),])
    else if (dir == 3) vec = rev(mat[,1])
  }
  names(vec) = NULL
  return(vec)
}

slice_edge = function(mat, dir, clockwise=FALSE) {
  if (!is.matrix(mat)) stop('mat must be a matrix')
  if ((!is.double(dir) & 
       !is.integer(dir)) | 
      (0 > dir) | (dir > 3)) {
    stop('dir must be a double or integer between 0 and 3, inclusive')
  }
  if (!clockwise) {
    # dir = c(0,1,2,3) <==> (down,right,up,left)
    if (dir == 0) mat = mat[,-1,drop = FALSE]
    else if (dir == 1) mat = mat[-nrow(mat),,drop = FALSE]
    else if (dir == 2) mat = mat[,-ncol(mat),drop = FALSE]
    else if (dir == 3) mat = mat[-1,,drop = FALSE]
  } else {
    # dir = c(0,1,2,3) <==> (right,down,left,up)
    if (dir == 0) mat = mat[-1, , drop = FALSE]
    else if (dir == 1) mat = mat[,-ncol(mat), drop = FALSE]
    else if (dir == 2) mat = mat[-nrow(mat),,drop = FALSE]
    else if (dir == 3) mat = mat[,-1,drop = FALSE]
  }
  return(mat)
}

spiral = function(mat, clockwise=FALSE) {
  if (!is.matrix(mat)) stop('mat must be a matrix')
  # Initialize the spiral vector
  vec = rep(NA,nrow(mat)*ncol(mat))
  # Initialize the vector element counter
  i = 0
  # Initialize the direction
  dir = 0
  # Run the spiral until all the vector elements have been filled
  while(i < length(vec)) {
    edge_vec = get_edge(mat, dir, clockwise)
    vec[(i+1):(i+length(edge_vec))] = edge_vec
    i = i+length(edge_vec)
    mat = slice_edge(mat, dir, clockwise)
    dir = (dir + 1) %% 4
  }
  return(vec)
}


















