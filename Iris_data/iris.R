## loading the data set.
iris_dataset = read.csv("irisdata.csv",header = FALSE)
iris_dataset
#iris_set = read.table("irisdata.csv",header = FALSE,colnames = c("sep.length","sep.width","pet.length","pet.width","species"))

colnames(iris_dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# creating sample with 80% of data and test data.
val_index <- createDataPartition(iris_dataset$Species,p=0.8, list = FALSE) #packages needed to for create data partition.

# loading required packages
library(caret)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

#create validation and sample
val_index <- createDataPartition(iris_dataset$Species,p=0.8, list = FALSE)
val_index
val_index[2,3]
val_index[2,]
validation <- iris_dataset[-val_index]
validation <- iris_dataset[-val_index,]
iris_dataset <- iris_dataset[val_index,]
val_dataset <- validation

#to remove/delete a variable.
rm(validation)

# Peeking to the data: understandings its attributes, classes, class distribution, dimensions and levels.
dim(iris_dataset)
class(iris_dataset)
sapply(iris_dataset,class)
head(iris_dataset)
tail(iris_dataset)
levels(iris_dataset$Species)
levels(iris_dataset)
percentage = prop.table(table(iris_dataset$species)) * 100
percentage
iris_dataset$species

table(iris_dataset$species)
iris_dataset

cbind(freq=table(iris_dataset$Species), percentage = percentage)
percentage <- prop.table(table(iris_dataset$species)) * 100
cbind(freq=table(iris_dataset$Species), percentage = percentage)
summary(iris_dataset)

## Visualizing data set.

x <- iris_dataset[,1:4]
y <- iris_dataset[,5]

#box plot for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
boxplot(x[,i], main=names(iris)[i])
}

#barplot for species.
plot(y)

#box and whisker plot for each attribute
featurePlot(x=x, y=y, plot="box")

#density plot for each attribute by species
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

savehistory("E:/Sindhu/projects/R_prog/Machine-Learning-usingR/iris.R")

#Evaluating with Machine Learning Algorithms.

#Tried implementing linear model and logit: as this is multi class data set these models cannot be implemented.

set.seed(10)
fit.lm <- train(Species~., data=iris_dataset, method="lm", metric="Accuracy", trControl=train_con) #error
fit.glm <- train(Species~., data=iris_dataset, method="glm", metric="Accuracy", trControl=train_con) #error
warnings()

#so going with :
#linear discriminant analysis(LDA), 
#Support Vector Machines(SVM),  
#Classification and Regression techniques (CART) and 
#K- Nearest neighbors(KNN)

# implementing 10-fold cross validation method
train_con <- trainControl(method="cv", number=10)

set.seed(10)
fit.lda <- train(Species~., data=iris_dataset, method="lda", metric="Accuracy", trControl=train_con)

set.seed(10)
fit.cart <- train(Species~., data=iris_dataset, method="rpart", metric="Accuracy", trControl=train_con)
install.packages('e1071', dependencies=TRUE) # needed for cart
set.seed(10)
fit.cart <- train(Species~., data=iris_dataset, method="rpart", metric="Accuracy", trControl=train_con)

set.seed(10)
fit.svm <- train(Species~., data=iris_dataset, method="svmRadial", metric="Accuracy", trControl=train_con)

set.seed(10)
fit.knn <- train(Species~., data=iris_dataset, method="knn", metric="Accuracy", trControl=train_con)

#summarizing the results
results <- resamples(list(lda=fit.lda, cart=fit.cart, svm=fit.svm, knn=fit.knn))
summary(results)

#comparing accuracy and kappa plots
dataplot(results)
dotplot(results)

#printing the bestmodel results
print(fit.lda)

#estimating LDA results on validation data set
est <- predict(fit.lda, validation)
confusionMatrix(est, validation$Species)


