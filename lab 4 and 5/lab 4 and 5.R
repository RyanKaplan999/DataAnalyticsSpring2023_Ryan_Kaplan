#Ryan Kaplan

# Outlier Example
data(cars)
View(cars)

# 1st 30 rows of cars
cars1 <- cars[1:30,] 
head(cars1)

# add some outlier data points
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))
head(cars_outliers)

cars2 <- rbind(cars1, cars_outliers)
help(par)

par(mfrow=c(1,2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), 
     main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist~speed, data=cars2), col='blue', lwd=3, lty=2)

# plot the original data without outliers, not the change in slope of best fit line
plot(cars1$speed, cars1$dist, xlim=c(0,28), ylim=c(0,230), main="Outliers removed \n A much better fit!", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist~speed, data=cars1), col="blue", lwd=3, lty=2)

# Cook's Distance using mtcars
data(mtcars)
mtcars
head(mtcars)
str(mtcars)
model1 <- lm(mpg~cyl+wt, data=mtcars)
model1
help("cooks.distance")
plot(model1, pch=18, col="red", which=c(4))

# use cooks.distance() to identify the cook's distance to each observation
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)

# round off values to 5 decimal points to make it easier to read
round(CooksDistance,5)

# sort the values in ascending order
sort(round(CooksDistance, 5))

# detect outliers/multivariate regression using cooks distance
# cook's distance is an estimate of the influence of a data point
# in other words, a summary of how much a regression model changes when
# the ith observation is removed from the data
library(ISLR)
library(dplyr)

#using the baseball hitters dataset from ISLR
head(Hitters)
dim(Hitters)
is.na(Hitters) # check for missing values
# remove them using na.omit()
HittersData <- na.omit(Hitters)
# checking the dimensions after removing na's
dim(HittersData)
glimpse(HittersData)
head(HittersData)

# do multivariate regression model using all features to predict salary
SalaryPredictModel1 <- lm(Salary ~., data=HittersData)
summary(SalaryPredictModel1)
# adjusted r-squared is 0.5106

# Cook's distance
cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm=TRUE)))]
influential

# we see that 18 players have a cook's distance over 3x the mean
# try excluding them and rerunning the model to see if the fit improves

#checking the names of the influential/outlier players
names_of_influential <- names(influential)
names_of_influential
outliers <- HittersData[names_of_influential,]
Hitters_Without_Outliers <- anti_join(HittersData, outliers)

# 2nd model without the outliers
SalaryPredictModel2 <- lm(Salary ~., data= Hitters_Without_Outliers)
summary(SalaryPredictModel2)
# adjusted r-squared is now 0.6445, removing only 18 observations

plot(SalaryPredictModel2)


