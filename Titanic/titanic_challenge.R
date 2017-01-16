library(dplyr)
library(tidyverse)
library(tibble)
library(jsonlite)
library(readr)
## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read_csv("test.csv", stringsAsFactors=FALSE)

## table command is one of the most basic summary statistics functions in R, 
## it runs through the vector you gave it and simply counts the occurrence of each value in it. 

table(train$Survived)

## How about a proportion?
prop.table(table(train$Survived))
## 38% sobreviveram

## Let's sayy that everyone in the test file died

test$Survived <- rep(0, 418)

## Since there was no “Survived” column in the dataframe, it will create one for us and repeat our
## “0” prediction 418 times, the number of rows we have. If this column already existed, 
## it would overwrite it with the new values, so be careful!

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theallperish.csv", row.names = FALSE)


## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-2-the-gender-class-model/
## We’ll start with the gender of the passengers

table(train$Sex)

prop.table(table(train$Sex, train$Survived))

prop.table(table(train$Sex, train$Survived),1)

## now can see that the majority of females aboard survived,
## and a very low percentage of males did

#updating our prediction

# Let's say that in our test sample the females have survived
test$Survived <- 0
test$Survived[test$Sex =='female'] <- 1

# Writing the submission "ladies first"

submit2 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit2, file = "ladiesfirst.csv", row.names = FALSE)
## let’s start digging into the age variable now

summary(train$Age)
## let’s create a new variable, “Child”, to indicate whether the passenger is below the age of 18:
train$Child <- 0
train$Child[train$Age < 18] <- 1

# Creating a table with both gender and age to see the survival proportions for different subsets

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
aggregate(Survived ~ Child + Sex, data = train, FUN = function (x) {sum(x)/length(x)})

# Reducing the complexity of the continuous fare variable into 4 values by creating a new column

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20 - 30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10 - 20'
train$Fare2[train$Fare < 10 ] <- '< 10'

## let’s run a longer aggregate function to see if there’s anything interesting to work with 
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#we notice that most of the class 3 women who paid more than $20 for their ticket actually
# also miss out on a lifeboat

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1 
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit3 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit3, file = "highclassladies.csv", row.names = FALSE)


## Titanic: Getting Started With R - Part 3: Decision Trees
## http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/