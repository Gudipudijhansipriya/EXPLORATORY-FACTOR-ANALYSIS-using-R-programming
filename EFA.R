### Exploratory-factor-analysis 

### install.packages("tidyverse")
library(tidyverse)

### read the data

data=read.csv("C:\\Users\\Lenovo\\OneDrive\\Desktop\\housecricket.csv")
data
summary(data)
library(psych)
psych::describe(data)

# A trimmed mean removes a smalle designated percentage of the largest and smallest values before calculating the average.
# mad: the average distance between each data value and the mean
# Skewness: A measure of the asymmetry of a distribution. 
# Kurtosis: A measure of the tailedness of a distribution. Tailedness is how often outliers occur.
# The standard error(SE): a statistic is the approximate standard deviation of a statistical sample population. 

dim(data)

### clean the data
# In our data frame, we have an ID variable in the first column. So, we can use a -1 in the column index to remove the first column and save our data to a new object.

df <- data[ , -1] 
head(df)

### Correlation matrix
# We also should take a look at the correlations among our variables to determine if factor 
# analysis is appropriate.

#install.packages("corrplot")
library(corrplot)
datamatrix = cor(df[,c(-13)])
corrplot(datamatrix, method="number")

# -1 indicates a perfectly negative linear correlation between two variables
# 0 indicates no linear correlation between two variables
# 1 indicates a perfectly positive linear correlation between two variables

### The Factorability of the Data

X=df[,-c(13)]
Y=df[,13]
head(X)
Y

### Kaiser-Meyer-Olkin (KMO)

# The Kaiser-Meyer-Olkin (KMO) used to measure sampling adequacy is a better measure of factorability.

KMO(r=cor(X))

# According to Kaiser's guidelines, a suggested cutoff for determining the factorability of the sample data is KMO ??? 60. The total KMO is 0.83, indicating that, based on this test, 
# we can probably conduct a factor analysis.

### Bartlett's Test of Sphericity

# Bartlett's test is used to test if k samples have equal variances. 

cortest.bartlett(X)

# A p-value, or probability value, is a number describing how likely it is that your data would have occurred under the null hypothesis of your statistical test.
# Small values (8.84e-290 < 0.05) of the significance level indicate that a factor analysis may be useful with our data.

library(ggplot2)
fafitfree = fa(df,nfactors = ncol(X), rotate = "none")
n_factors = length(fafitfree$e.values)
scree = data.frame(Factor_n =  as.factor(1:n_factors),Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

### Parallel Analysis
# 
# We can use the parallel() function from the nFactors package to perform a parallel analysis.

parallel= fa.parallel(X)

## Parallel analysis suggests that the number of factors =  4 and the number of components =  3

### Conducting the Factor Analysis
# Factor analysis using fa method

fa.none = fa(r=X,nfactors=4, fm="pa",max.iter=100,rotate="varimax") 
print(fa.none)

### Factor analysis using the factanal method

factanal.none = factanal(X, factors=4, scores = c("regression"), rotation = "varimax")
print(factanal.none)

### Graph Factor Loading Matrices

fa.diagram(fa.none)

fa.graph(fa.none)


