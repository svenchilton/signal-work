# Sven Chilton and Gabe Sanchez
# Signal Data Science Cohort 3

setwd('~/GitHub/signal-work/rscripts')

library('readr')
library('dplyr')
library('cluster')
library('pvclust')
library('fpc')
library('ggplot2')
library('mixtools')
library('mclust')

# Load in the European protein consumption data
df = data.frame(read_delim('../data/protein.txt', delim='\t'))

# Scale the non-Country columns
scaled_df = cbind(df['Country'], scale(dplyr::select(df, -Country)))

# Set Countries as the row names of the data frame
rownames(scaled_df) = scaled_df[['Country']]

# Now eliminate the Country column, since that information is 
# represented in the row names of the df
scaled_df = dplyr::select(scaled_df, -Country)

# Create Euclidean distance matrix

euclid_dist = dist(scaled_df, method = "euclidean")
View(euclid_dist)

sqrt(sum((scaled_df['Albania',] - scaled_df['Austria',])^2))

# Exploratory hierarchical clustering with Ward's method
ward_hclust = hclust(euclid_dist, method='ward.D2')
plot(ward_hclust)

# Cut the dendogram off at a specific number of clusters: 
# cutree(hclust(...), k)

# Convenience function for printing information on a dendogram 
# cut off at k clusters
# Make sure to pass in the original, unscaled df, with a Country 
# column rather than country row names
print_clusters = function(df, labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(df[labels == i, c("Country", "RedMeat", "Fish", "Fr.Veg")])
  } 
}

# Let's try 5 clusters
# We're passing in the original, unscaled df so as to print out 
# the actual consumption values
clusters5 = cutree(ward_hclust, 5)
print_clusters(df, clusters5, 5)


# Wrapper function 
# Performs hierarchical clustering on the distance matrix by the 
# given method, cuts off the dendrogram at k clusters, and plots 
# the result
# scaled_df = obvious
# d = (scaled) distance matrix
# method = clustering method, e.g. which distance metric to use
# k = number of clusters
hclust_plot = function(scaled_df, d, method, k) {
  dendrogram = hclust(d, method=method)
  kclusters  = cutree(dendrogram, k)
  clusplot(scaled_df, kclusters, color=TRUE, shade=TRUE, labels=2, lines=0)
}

hclust_plot(scaled_df, euclid_dist, 'ward.D2', 4)

# Assign statistical significance to our dendrogram from above 
# with pvclust()
# NOTE: pvrect() overlays boxes on dendrogram plots.  
# It will generate meaningless output if there isn't a pre-existing 
# plot on which to overlay the boxes.
bootdend = pvclust(t(scaled_df), method.hclust="ward.D2", method.dist="euclidean")
plot(bootdend); pvrect(bootdend, alpha=0.95)


# K-means clustering
# Run K-means clustering on the scaled protein consumption data, 
# setting K = 5
kmeans_plot = function(scaled_data, k) {
  clusters = kmeans(scaled_data, k)
  clusplot(scaled_data, clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
}

# Note that this does not produce the same result with every call, 
# presumably because the K cluster centers are randomly chosen 
# each time
kmeans_plot(scaled_df, 5)


# Validating a choice of K
kmr_ch  = kmeansruns(scaled_df, krange=1:10, criterion='ch')
kmr_asw = kmeansruns(scaled_df, krange=1:10, criterion='asw')

kmr_crit_plot = 
  ggplot() + 
  geom_point(aes(1:10, kmr_ch$crit), color='blue', alpha=0.5, size=4) + 
  geom_point(aes(1:10, kmr_asw$crit), color='red', alpha=0.5, size=3) + 
  xlab('K') + ylab('Criterion')

# Since a higher criterion value means better clustering quality, 
# K=2 results in the best clustering by the CH criterion
# The ASW criterion, in turn, results in a best K of 3

# Let's try bootstrapping to validate our K
kcb = clusterboot(scaled_df, clustermethod=kmeansCBI, runs=100, iter.max=100, krange=3)

kcb$result$partition
kcb$bootmean
kcb$bootbrd

# Univariate mixture models

# Load the df on Old Faithful eruptions and plot a histogram 
# of the waiting times
df_faithful = faithful
wait_plot = 
  ggplot(data = df_faithful) + 
  geom_histogram(aes(waiting), bins=50, fill='blue', alpha=0.5)

eruption_plot = 
  ggplot(data = df_faithful) + 
  geom_histogram(aes(eruptions), bins=50, fill='blue', alpha=0.5)

# Fit the waiting times to a mix of Gaussians and plot the 
# result
em_wait2 = normalmixEM(df_faithful$waiting, k=2)
summary(em_wait2)
plot(em_wait2, density=TRUE, which=2)

em_wait3 = normalmixEM(df_faithful$waiting, k=3)
summary(em_wait3)
plot(em_wait3, density=TRUE, which=2)

# Semiparametric fitting
spfit_wait = spEMsymloc(df_faithful$waiting, mu0 = 2)
plot(spfit_wait)

# Let's see what happens when we add outliers to the data
df_faithful_with_outliers = df_faithful
mean(df_faithful$waiting)
sd(df_faithful$waiting)
range(df_faithful$waiting)
range(df_faithful$eruptions)
df_faithful_with_outliers = rbind(df_faithful_with_outliers, c(0.8, 150))
df_faithful_with_outliers = rbind(df_faithful_with_outliers, c(10.0, 20))
tail(df_faithful_with_outliers)
nrow(df_faithful_with_outliers)
nrow(df_faithful)

em_wait_out = normalmixEM(df_faithful_with_outliers$waiting, k=2)
plot(em_wait_out, density=TRUE, which=2)

spfit_wait_out = spEMsymloc(df_faithful_with_outliers$waiting, mu0 = 2)
plot(spfit_wait_out)

# It appears that spEMsymloc() is less susceptible to outlying 
# data than normalmixEM()

# Multivariate mixture models

# First, let's look at a scatter plot of Old Faithful waiting 
# times against eruptions
scattered_eruptions = 
  ggplot(df_faithful) + geom_point(aes(eruptions, waiting), color='blue')

# To my eyes, there appear to be 2 clusters in the plot above

# Fit the Old Faithful data to families of multivariate Gaussians
# After calling plot(), go to the console and enter 2 to see the 
# clusters
# Don't forget to enter 0 to exit the plot display interface 
# before running anything else
of_mclust = Mclust(df_faithful)
plot(of_mclust)

# Let's run Mclust() on the scaled protein consumption data
sp_mclust = Mclust(scaled_df)
plot(sp_mclust)

# That plot is difficult to read.  It would help to know which 
# countries are in each cluster.  Fortunately, there's an 
# attribute for that.
sort(sp_mclust$classification)

# Nonparametric models

of_npem = npEM(df_faithful, mu0 = 1)
plot(of_npem)


sp_npem = npEM(scaled_df, mu0 = 4)
plot(sp_npem)























