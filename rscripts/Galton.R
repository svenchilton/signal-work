# Sven Chilton and Matt Wong
# Signal Data Science Cohort 3

library("HistData")
library("dplyr")
library("Rmisc")

setwd("/Users/svenchilton/GitHub/signal-data-science/R-curriculum/assignments/2-basic-topics/linear-regression/1-intro")

df <- GaltonFamilies
View(df)
head(df, 10)
dim(df)
(names(df))[1]

as.numeric(df$gender) - 1
# Males are 1, females are 0 in this example
df$gender <- as.numeric(df$gender) - 1
head(df)

ave_heights = 
  df %>%
  group_by(family) %>%
  dplyr::summarize(
    father = mean(father), 
    mother = mean(mother), 
    midparentHeight = mean(midparentHeight),
    meanChildHeight = mean(childHeight))
ave_heights
View(ave_heights)

p1 = ggplot(ave_heights, aes(father, meanChildHeight)) + 
  geom_point() + 
  geom_smooth(method = "lm")

p2 = ggplot(ave_heights, aes(mother, meanChildHeight)) + 
  geom_point() + 
  geom_smooth(method = "lm")

p3 = ggplot(ave_heights, aes(midparentHeight, meanChildHeight)) + 
  geom_point() + 
  geom_smooth(method = "lm")

multiplot(p1, p2, p3, cols=1)

cor(select(ave_heights, -family))

# For later
lfChildFather = lm(father ~ meanChildHeight, ave_heights)
lfChildFather
summary(lfChildFather)$adj.r.squared
lfChildMother = lm(mother ~ meanChildHeight, ave_heights)
lfChildMother
summary(lfChildMother)$adj.r.squared
lfChildMid = lm(midparentHeight ~ meanChildHeight, ave_heights)
lfChildMid
summary(lfChildMid)$adj.r.squared

kids_per_family = 
  df %>%
  group_by(family) %>%
  dplyr::summarize(
    father = mean(father), 
    mother = mean(mother), 
    midparentHeight = mean(midparentHeight),
    meanChildHeight = mean(childHeight),
    children = mean(children))
kids_per_family
View(kids_per_family)

#
lfChildrenFather = lm(father ~ children, kids_per_family)
lfChildrenFather
summary(lfChildrenFather)
lfChildrenMother = lm(mother ~ children, kids_per_family)
lfChildrenMother
summary(lfChildrenMother)
lfChildrenMid = lm(midparentHeight ~ children, kids_per_family)
lfChildrenMid
summary(lfChildrenMid)


ggplot(kids_per_family, aes(children)) + geom_histogram()
