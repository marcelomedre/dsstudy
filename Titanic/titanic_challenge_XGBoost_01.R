# Clear workspace
rm(list = ls())
# Setting wd
getwd()
setwd("C:/Users/Marcelo/Desktop/Data/Study/Titanic")

# Loading libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Matrix)
library(randomForest)
library(xgboost)

# Loading data

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

str(train)
summary(train)

# Changing the complexity of Fare Column
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20 - 30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10 - 20'
train$Fare2[train$Fare < 10 ] <- '< 10'

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)
fancyRpartPlot(fit)

# Changing the complexity of Fare Column in the test data
test$Fare2 <- '30+'
test$Fare2[test$Fare < 30 & test$Fare >= 20] <- '20 - 30'
test$Fare2[test$Fare < 20 & test$Fare >= 10] <- '10 - 20'
test$Fare2[test$Fare < 10 ] <- '< 10'


Prediction <- predict(fit, test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "MLT_1002_1.csv", row.names = FALSE)


train$Sex[train$Sex == "male"] <- 1
train$Sex[train$Sex == "female"] <- 0

test$Sex[test$Sex == "male"] <- 1
test$Sex[test$Sex == "female"] <- 0

train[is.na(train)]<-0
test[is.na(test)]<-0

train$Sex = as.numeric(train$Sex)
test$Sex = as.numeric(test$Sex)

train$SibSp = as.numeric(train$SibSp)
test$SibSp = as.numeric(test$SibSp)

train$Parch = as.numeric(train$Parch)
test$Parch = as.numeric(test$Parch)

str(train)

param = list("objective" = "multi:softmax", # multi class classification
                   "num_class"= 2 ,  		# Number of classes in the dependent variable.
                   "eval_metric" = "mlogloss",  	 # evaluation metric 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 16,    		 # maximum depth of tree 
                   "eta" = 0.3,    			 # step size shrinkage 
                   "gamma" = 0,    			 # minimum loss reduction 
                   "subsample" = 0.7,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 12  		 # minimum sum of instance weight needed in a child 
)


label = as.numeric(train[,"Survived"])
predictors = colnames(train[,-ncol(train)])
str(predictors)

nums <- sapply(train, is.numeric)

train_num = train[, nums]

bst = xgboost(
        param=param,
        data = data.matrix(train_num),
        label = label,
        nrounds=1000)

numst <- sapply(test, is.numeric)

testn_num = test[, numst]

# Make prediction on the testing data.
train.prediction = predict(bst, as.matrix(testn_num))

output<-data.frame(test$PassengerId,train.prediction)

colnames(output)<-cbind("PassengerId","Survived")

write.csv(output, file = 'xgboost_Solution_01.csv', row.names = F)
# Submission scored 0.37





## Using your controls
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data=train,
               method="class",
               control=rpart.control( your controls ))

## An interactive version of the decision tree will appear in the plot tab
## where you simply click on the nodes that you want to kill.
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
Prediction2 <- predict(new.fit, test, type = 'class')

# analysing the titles of the persons Miss, Mrs, Mr, Master....

test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split = '[,.]')

# the Title is the second item in the matrix name

strsplit(combi$Name[1], split ='[,.]')[[1]][2]

## We have isolated the title we wanted at last.
## how to apply this transformation to every row of the combined train/test dataframe

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

## we may wish to strip off those spaces from the beginning of the titles
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

# Combining some titles creating a single category to Mme and Mlle
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

##  change the variable type back to a factor
combi$Title <- factor(combi$Title)

# We are done with passengers title, now let's take a look at the family size
# two variables SibSb and Parch that indicate the number of family members 
# the passenger is travelling with

#let's combine the two variables into a new one, FamilySize:
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Another thing is combine surname and family size
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

## We then want to append the FamilySize variable to the front of it, but as we saw 
## with factors, string operations need strings. So let's convert the FamilySize 
## variable temporarily to a string and combine it with the Surname to get our new FamilyID variable:

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

## We used the function paste to bring two strings together, and told it to separate
## them with nothing through the sep argument.

## Given we were originally hypothesising that large families might have trouble
## sticking together in the panic, let's knock out any family size of two or less
## and call it a "small" family. 

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

# cleaning the families with less than 3 members
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

## We then need to overwrite any family IDs in our dataset for groups that were
## not correctly identified and finally convert it to a factor
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# let's break them apart and do some predictions on our new fancy engineered variables:
train <- combi[1:891,]
test <- combi[892:1309,]

#time to make some predictions

# Random Forest trees approach
# Baggins takes a randomized sample of the rows in your training set, with replacement.
sample(1:10, replace = TRUE)
summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title +
                        FamilySize, data = combi[!is.na(combi$Age),],
                method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)

summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = 'S'
combi$Embarked <- factor(combi$Embarked)

summary(combi$Fare)

which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)

library(randomForest)
# the process has the two sources of randomness
# it is a good idea to set the random seed in R
#before you begin.

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]
set.seed(415)

#Build inference tree random Forest
library(party)
set.seed(415)

#model
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)

str(train)

train$Cabin <- NULL
train$Name <- NULL
train$FamilyID2 <- NULL

nums <- sapply(train, is.numeric)

train_num = train[, nums]

# Normalization using built_in scale() 
maxs <- apply(train[,nums], 2, max) # only the numeric columns
mins <- apply(train[,nums], 2, min)

train_scaled <- as.data.frame(scale(train[,nums], center = mins, scale = maxs - mins))
head(train_scaled)

feats <- names(train_scaled[,-(1:2)])
feats
# concatenated strings
f <- paste(feats, collapse = " + ")
f <- paste("Survived ~", f)
# 2 steps above is the same of writing Private ~ Apps + Accept + Enroll + ...

f <- as.formula(f)
f

library(neuralnet)
nn <- neuralnet(f, train_scaled, hidden = 15, stepmax=1e6)

plot(nn)

nn$response

plot(train$PassengerId, train_scaled$Survived)
library(ggplot2)

ggplot(train_scaled, aes(x = 1:891, y = Survived))+
        geom_point()+
        geom_point(aes(y = nn$response), col = "red")

# Setting test dataset
test$Cabin <- NULL
test$Name <- NULL
test$FamilyID2 <- NULL

numst <- sapply(test, is.numeric)

test_num = test[, numst]

# Normalization using built_in scale() 
maxs <- apply(test[,numst], 2, max) # only the numeric columns
mins <- apply(test[,numst], 2, min)

test_scaled <- as.data.frame(scale(test[,nums], center = mins, scale = maxs - mins))
head(test_scaled)

# Compute Predictions off Test Set
predict.nn.values <- compute(nn, test_scaled[3:9])

# Prediction scored

library(scales)
results <- predict.nn.values$net.result
rescale(results)
results = round(results, digits = 0)

submit_ann <- data.frame(PassengerId = test$PassengerId, Survived = results)
write.csv(submit_ann, file = "Titanic_ann.csv", row.names = FALSE)

# scored 0.74163


bst = xgboost(
        param=param,
        data = data.matrix(train_num),
        label = label,
        nrounds=1000)

numst <- sapply(test, is.numeric)

testn_num = test[, numst]

# Make prediction on the testing data.
train.prediction = predict(bst, as.matrix(testn_num))

output<-data.frame(test$PassengerId,train.prediction)

colnames(output)<-cbind("PassengerId","Survived")

write.csv(output, file = 'xgboost_Solution_01.csv', row.names = F)
# Submission scored 0.37
