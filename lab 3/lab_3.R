library(rpart)
library(party)
# Ryan Kaplan Lab 3

# Ctree
require(rpart)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) 
text(swiss_rpart) 

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)
# find "prettier" ways to plot the tree
plot(tr, type='uniform')
text(tr, pretty= 0)

# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)


fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)

plot(fitK, main="Conditional Inference Tree for Kyphosis ")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")

# Knn
library(kknn)
require(kknn)

data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 

iris.learn <- iris[-val,] 	# train
iris.valid <- iris[val,]	# test

iris.kknn <- train.kknn(Species~., iris.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )
summary(iris.kknn)
table(predict(iris.kknn,iris.valid),iris.valid$Species)

head(iris.kknn$response)
head(iris.kknn$distance)
head(iris.kknn$call)

head(iris.kknn$fitted.values)

# random forest
library(randomForest)

require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
#
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
help(rfcv)

# other data....
data(imports85)

# perform randomForest and other tree methods.....

install.packages('caret', dependencies = TRUE)

# we're going to predict price from imports85 using random forest
repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3)

imports <- na.omit(imports85)

# put the data into train and test set
train_index <- createDataPartition(y=imports$price, p=0.7, list=FALSE)
training_set <- imports[train_index, ]
testing_set <- imports[-train_index, ]

# train it using RMSE, since this is regression
forest <- train(price~., data=training_set, method='rf', trControl=repeat_cv, metric='RMSE')

# get the model details
forest$finalModel

plot(forest)


# r part
library(rpart)
require(rpart)

# build the  tree
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) # display the results
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) # visualize cross-validation results
# plot tree
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.6)
# prune the tree
pfitM<- prune(fitM, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)
post(pfitM, file = "ptree2.ps", title = "Pruned Regression Tree for Mileage")


library(e1071)
library(rpart)
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
plotcp(rpart.model)

rsq.rpart(rpart.model)
print(rpart.model)

plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE)
plot(rpart.pred)

fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) # display the results
plotcp(fitK) # visualize cross-validation results
summary(fitK) # detailed summary of splits
# plot tree
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postscript plot of tree
post(fitK, file = "kyphosistree.ps", title = "Classification Tree for Kyphosis") # might need to convert to PDF (distill)


# Titanic Data
data(Titanic)

# do rpart, ctree, hclust for: Survived ~ .
View(Titanic)

cT <-ctree(Survived ~ ., data=Titanic)
plot(cT)

fitT <- rpart(Survived~Class + Sex + Age + Freq, method="class", data=Titanic)
printcp(fitT) # display the results

plotcp(fitT)
summary(fitT)
par(mfrow=c(1,2)) 
rsq.rpart(fitT) # visualize cross-validation results
# plot tree
plot(fitT, uniform=TRUE, main="Regression Tree for Titanic ")
text(fitT, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfitT<- prune(fitT, cp=0.01160389) 
# plot the pruned tree
plot(pfitT, uniform=TRUE, main="Pruned Regression Tree for Titanic")
text(pfitT, use.n=TRUE, all=TRUE, cex=.8)

