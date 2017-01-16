## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
## Starting with Learning Machines
## decision trees - They are what’s known as a glass-box model, 
## after the model has found the patterns in the data you can 
## see exactly what decisions will be made for unseen data that you want to predict.

## conceptually, the algorithm starts with all of the data at the root node (drawn at the top)
## and scans all of the variables for the best one to split on. The way it measures this
## is to make the split on the variable that results in the most pure nodes below it,
## ie with either the most 1’s or the most 0’9s in the resulting buckets.
## But let’s look at something more familiar to get the idea.

# "Recursive Partitioning and Regression Trees” and uses the CART decision tree algorithm
getwd()
setwd("C:/Users/Marcelo/Desktop/Data/Study/Titanic")
library(rpart)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# You feed it the equation, headed up by the variable of interest and followed by the
# variables used for prediction. You then point it at the data, and for now, follow 
# with the type of prediction you want to run (see ?rpart for more info). If you wanted 
# to predict a continuous variable, such as age, you may use method="anova". This would 
# run generate decimal quantities for you. But here, we just want a one or a zero,
# so method="class" is appropriate

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = 'class')
submit4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit4, file = "myfirstdtree.csv", row.names = FALSE)

## CP parameter is the metric that stops splits that aren't deemed important enough.
## minsplit governs how many passengers must sit in a bucket before even looking for a split
## Let's max both out and reduce cp to zero and minsplit to 2 (no split would obviously be 
## possible for a single passenger in a bucket)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

## THIS FIT SCORED 0.74163, AGAINST 0.78469 (PREVIOUS AND SIMPLE ONDE) .... 
## THE PROBLEM WITH THIS FIT IS THAT Welcome to overfitting

## Overfitting is technically defined as a model that performs better on a 
## training set than another simpler model, but does worse on unseen data.

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


### Titanic: Getting Started With R - Part 4: Feature Engineering
## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-4-feature-engineering/

## Feature engineering is so important to how your model performs, 
## that even a simple model with great features can outperform a 
## complicated algorithm with poor ones.

train$Name[1]

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

fit2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")
fancyRpartPlot(fit2)

Prediction3 <- predict(fit, test, type = 'class')
submit5 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit5, file = "my2nddtree.csv", row.names = FALSE)

## Improving the model with an ensemble

# Take a large collection of individually imperfect models, 
# and their one-off mistakes are probably not going to be made 
# by the rest of them. If we average the results of all these models,
#we can sometimes find a superior model from their combination than
#any of the individual parts. That's how ensemble models work, they
#grow a lot of different models, and let their outcomes be averaged 
#or voted across the group

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

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

library(randomForest)
# the process has the two sources of randomness
# it is a good idea to set the random seed in R
#before you begin.

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]
set.seed(415)

#building our model
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                            Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
importance(fit)

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