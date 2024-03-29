# Ryan Kaplan

# ISLR: Introduction to Statistical Learning with R 
# Validation set example 
library(ISLR)
library(MASS)
library(boot)
set.seed(1)

# Read the cv.glm documentation
help(cv.glm)

# read the documentation for sample() function
help("sample")

train <- sample(392,196)
# We use the subset option in the lm() function to fit a linear regression using,
# only the observations corresponding to the training set.
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)

# use predict() function to estimate the response for all 392 observations, and mean() to calculate the MSE of the 196 observations 
# in the validation set. Note that the -train selects only the observations that are not in the training set.
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# estimated test MSE for the linear regression fit is 26.83

# use poly() to estimate test error for the quadratic and cubic regression.

# Quadratic regression line
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# Cubic regression line
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# The error rates are: 19.56 for quadratics and 19.62 for cubic
# If we choose different training set instead, then we will obtain somewhat different errors on the validation set.
set.seed(2)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# the rate is 25.72

lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# the rate is 20.43

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# the rate is 20.38

# the model that predicts mpg using a quadratic function of horsepower performs better than a linear function,
# and both are better than using a cubic function


# k-Fold Cross Validation
# The cv.glm() function can also be used to implement k-fold CV.
# We once again, set a random seed and initialize a vector in to store the CV errors corresponding to the polynomial fits 
# of orders one to ten. here we use K = 10

help(cv.glm)
set.seed(17)
help("rep") # read the documentation for the rep() function in R.
cv.error.10 = rep(0,10) # read documentation, help("rep")

for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1]
}
cv.error.10

# We still see little evidence that using cubic or higher-order polynomials terms
# leads to lower test error than simply using a quadratic fit.

# Diamonds
library(ggplot2)
data(diamonds)
head(diamonds)

ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~cut)
ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept = 12000)

ggplot(data=diamonds, mapping=aes(color=cut_number(carat,5),x=price)) + geom_freqpoly() + labs(x="Price", y="Count", color="Carat")

ggplot(diamonds, aes(x=cut_number(price, 10), y=carat)) + geom_boxplot() + coord_flip() + xlab("Price")

ggplot(diamonds, aes(x=cut_number(carat, 5), y=price, colour=cut)) + geom_boxplot()
