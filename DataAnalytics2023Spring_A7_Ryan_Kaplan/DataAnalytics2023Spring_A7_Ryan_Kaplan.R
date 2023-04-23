library(XLS)
library(xlsx)
library(readxl)

install.packages('nortest')
library(nortest)

#Install class package
install.packages('class')
# Load class package
library(class)


#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

# load in our first dataset, work absenteeism
absent <- read.csv(file = 'Absenteeism_at_work.csv',head = TRUE, sep=";")
abs_data <- data.frame(absent)
View(abs_data)

summary(abs_data)

# absenteeism time in hours is our target variable, good to look at his 
absenteeism_time_in_hours <- abs_data$Absenteeism.time.in.hours
summary(absenteeism_time_in_hours)

boxplot(absenteeism_time_in_hours, main = "Absenteeism Time In Hours")

# remove the outliers in absentee hours
cleaned_abs_data <- abs_data[abs_data$Absenteeism.time.in.hours <= 20 ,]
View(cleaned_abs_data)

# show the distribution minus outliers
cutoff_hours <- cleaned_abs_data$Absenteeism.time.in.hours
boxplot(cutoff_hours, main = "Absenteeism Time In Hours")
summary(cutoff_hours)


# predict the absentee hours
ID <- as.numeric(cleaned_abs_data$ID)
absence_reason <- as.numeric(cleaned_abs_data$Reason.for.absence)
month <- as.numeric(cleaned_abs_data$Month.of.absence)
day <- as.numeric(cleaned_abs_data$Day.of.the.week)
season <- as.numeric(cleaned_abs_data$Seasons)
transport_expense <- as.numeric(cleaned_abs_data$Transportation.expense)
distance_to_work <- as.numeric(cleaned_abs_data$Distance.from.Residence.to.Work)
service_time <- as.numeric(cleaned_abs_data$Service.time)
age <- as.numeric(cleaned_abs_data$Age)
average_workload <- as.numeric(cleaned_abs_data$Work.load.Average.day)
hit_target <- as.numeric(cleaned_abs_data$Hit.target)
discipline_failure <- as.numeric(cleaned_abs_data$Disciplinary.failure)
education <- as.numeric(cleaned_abs_data$Education)
number_of_children <- as.numeric(cleaned_abs_data$Son)
drinker <- as.numeric(cleaned_abs_data$Social.drinker)
smoker <- as.numeric(cleaned_abs_data$Social.smoker)
num_pets <- as.numeric(cleaned_abs_data$Pet)
weight <- as.numeric(cleaned_abs_data$Weight)
height <- as.numeric(cleaned_abs_data$Height)
BMI <- as.numeric(cleaned_abs_data$Body.mass.index)

hours <- as.numeric(cleaned_abs_data$Absenteeism.time.in.hours)

frame <- data.frame(ID, absence_reason, month, day, season, transport_expense, distance_to_work, service_time, age, average_workload, hit_target, discipline_failure, education, number_of_children, drinker, smoker, num_pets, weight, height, BMI)
View(frame)

# Create a linear model
linear_model <- lm(hours~ID+ absence_reason+ month+ day+ season+ transport_expense+ distance_to_work+ service_time+ age+ average_workload+ hit_target+ discipline_failure+ education+ number_of_children+ drinker+ smoker+ num_pets+ weight+ height+ BMI, data = frame)
# Check our results
summary(linear_model) 

normed <- as.data.frame(lapply(frame, normalize))

# create a train set with 70% of the data, and a test set with 30%
train_set <- normed[1:487, ]
test_set <- normed[487:696, ]

#View(train_set)

train_labels <- hours[1:487]
test_labels <- hours[487:696]

#Find the number of observation
NROW(train_labels) # we set k to sqrt(n)

knn_22 <- knn(train=train_set, test=test_set, cl=train_labels, k=22)

#Calculate the proportion of correct classification for k = 22
acc <- 100 * sum(test_labels == knn_22)/NROW(test_labels)
acc

# repeat only using the most significant variables
better_frame <- data.frame(ID, absence_reason, transport_expense,  service_time, discipline_failure, number_of_children,  smoker, num_pets, weight, height, BMI)
normed <- as.data.frame(lapply(better_frame, normalize))

# create a train set with 70% of the data, and a test set with 30%
train_set <- normed[1:487, ]
test_set <- normed[487:696, ]

#View(train_set)

train_labels <- hours[1:487]
test_labels <- hours[487:696]

knn_22 <- knn(train=train_set, test=test_set, cl=train_labels, k=22)

#Calculate the proportion of correct classification for k = 22
acc <- 100 * sum(test_labels == knn_22)/NROW(test_labels)
acc



# load in our second, wine quality
red <- read.csv(file = 'winequality-red.csv',head = TRUE, sep=";")
red_data <- data.frame(red)
View(red_data)
summary(red_data)

white <- read.csv(file = 'winequality-white.csv',head = TRUE, sep=";")
white_data <- data.frame(white)
View(white_data)
summary(white_data)

# compare the boxplots for red and white wine alcohol 
red_alcohol <- red_data$alcohol
boxplot(red_alcohol, main = "Red Wine Alcohol")
white_alcohol <- white_data$alcohol
boxplot(white_alcohol, main = "White Wine Alcohol")


# compare the boxplots for red and white wine quality
red_quality <- red_data$quality
boxplot(red_quality, main = "Red Wine Quality")
white_quality <- white_data$quality
boxplot(white_quality, main = "White Wine Quality")


# predict the red quality using regression
fixed_acidity <- as.numeric(red_data$fixed.acidity)
volatile_acidity <- as.numeric(red_data$volatile.acidity)
citric_acid <- as.numeric(red_data$citric.acid)
residual_sugar <- as.numeric(red_data$residual.sugar)
chlorides <- as.numeric(red_data$chlorides)
free_sulfur_dioxide <- as.numeric(red_data$free.sulfur.dioxide)
total_sulfur_dioxide <- as.numeric(red_data$total.sulfur.dioxide)
density <- as.numeric(red_data$density)
ph <- as.numeric(red_data$pH)
sulphates <- as.numeric(red_data$sulphates)
alcohol <- as.numeric(red_data$alcohol)

quality <- as.numeric(red_data$quality)

red_frame <- data.frame(fixed_acidity, volatile_acidity, citric_acid, residual_sugar, chlorides, free_sulfur_dioxide, total_sulfur_dioxide, density, ph, sulphates, alcohol)
View(red_frame)

# Create a linear model
linear_model <- lm(quality~fixed_acidity+ volatile_acidity+ citric_acid+ residual_sugar+ chlorides+ free_sulfur_dioxide+ total_sulfur_dioxide+ density+ ph+ sulphates+ alcohol, data = red_frame)
# Check our results
summary(linear_model) 


# do knn on the red wine
red_normed <- as.data.frame(lapply(red_frame, normalize))

# create a train set with 70% of the data, and a test set with 30%
train_set <- red_normed[1:1119, ]
test_set <- red_normed[1119:1599, ]

train_labels <- quality[1:1119]
test_labels <- quality[1119:1599]

#Find the number of observation
NROW(train_labels) # we set k to sqrt(n)


knn_33 <- knn(train=train_set, test=test_set, cl=train_labels, k=33)

#Calculate the proportion of correct classification for k = 33
acc <- 100 * sum(test_labels == knn_33)/NROW(test_labels)
acc

# repeat only using the most significant variables
better_frame <- data.frame(volatile_acidity, chlorides, free_sulfur_dioxide, total_sulfur_dioxide, ph, sulphates, alcohol)
normed <- as.data.frame(lapply(better_frame, normalize))

# create a train set with 70% of the data, and a test set with 30%
train_set <- red_normed[1:1119, ]
test_set <- red_normed[1119:1599, ]

train_labels <- quality[1:1119]
test_labels <- quality[1119:1599]

knn_33 <- knn(train=train_set, test=test_set, cl=train_labels, k=33)

#Calculate the proportion of correct classification for k = 33
acc <- 100 * sum(test_labels == knn_33)/NROW(test_labels)
acc



# predict the white quality using regression
fixed_acidity <- as.numeric(white_data$fixed.acidity)
volatile_acidity <- as.numeric(white_data$volatile.acidity)
citric_acid <- as.numeric(white_data$citric.acid)
residual_sugar <- as.numeric(white_data$residual.sugar)
chlorides <- as.numeric(white_data$chlorides)
free_sulfur_dioxide <- as.numeric(white_data$free.sulfur.dioxide)
total_sulfur_dioxide <- as.numeric(white_data$total.sulfur.dioxide)
density <- as.numeric(white_data$density)
ph <- as.numeric(white_data$pH)
sulphates <- as.numeric(white_data$sulphates)
alcohol <- as.numeric(white_data$alcohol)

quality <- as.numeric(white_data$quality)

white_frame <- data.frame(fixed_acidity, volatile_acidity, citric_acid, residual_sugar, chlorides, free_sulfur_dioxide, total_sulfur_dioxide, density, ph, sulphates, alcohol)
View(white_frame)

# Create a linear model
linear_model <- lm(quality~fixed_acidity+ volatile_acidity+ citric_acid+ residual_sugar+ chlorides+ free_sulfur_dioxide+ total_sulfur_dioxide+ density+ ph+ sulphates+ alcohol, data = white_frame)
# Check our results
summary(linear_model) 

# do knn on the white wine
white_normed <- as.data.frame(lapply(white_frame, normalize))

# create a train set with 70% of the data, and a test set with 30%
train_set <- white_normed[1:1119, ]
test_set <- white_normed[1119:1599, ]

train_labels <- quality[1:1119]
test_labels <- quality[1119:1599]

#Find the number of observation
NROW(train_labels) # we set k to sqrt(n)

knn_33 <- knn(train=train_set, test=test_set, cl=train_labels, k=33)

#Calculate the proportion of correct classification for k = 33
acc <- 100 * sum(test_labels == knn_33)/NROW(test_labels)
acc

# repeat only using the most significant variables
better_frame <- data.frame(fixed_acidity, volatile_acidity, residual_sugar, free_sulfur_dioxide, density, ph, sulphates, alcohol)
normed <- as.data.frame(lapply(better_frame, normalize))

# create a train set with 70% of the data, and a test set with 30%
train_set <- white_normed[1:1119, ]
test_set <- white_normed[1119:1599, ]

train_labels <- quality[1:1119]
test_labels <- quality[1119:1599]

knn_33 <- knn(train=train_set, test=test_set, cl=train_labels, k=33)

#Calculate the proportion of correct classification for k = 33
acc <- 100 * sum(test_labels == knn_33)/NROW(test_labels)
acc


