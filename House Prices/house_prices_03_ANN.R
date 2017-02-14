setwd("C:/Users/Marcelo/Desktop/DsStudy/House Prices")
rm(list = ls())
library(data.table)
# Loading data
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

SalePrice <- train$SalePrice
train$SalePrice <- NULL

# Combining data sets
full_data <- rbind(train, test)

#Removing missing NA values
for(col in colnames(full_data)){
        if(typeof(full_data[,col]) == "character"){
                new_col = full_data[,col]
                new_col[is.na(new_col)] = "missing"
                full_data[col] = as.factor(new_col)
        }
}

#Separating data sets
train <- full_data[1:nrow(train),]
train$SalePrice <- SalePrice
test <- full_data[(nrow(train)+1):nrow(full_data),]

summary(train)

#Remaining NA's
train[is.na(train)] = -1
test[is.na(test)] = -1

#one can use de # o bathrooms and the total sq_footage ans 1 variable
train$tot_sq_footage <- train$GrLivArea + train$TotalBsmtSF
test$tot_sq_footage <- test$GrLivArea + test$TotalBsmtSF

#Variable to combine the # of baths
train$total_baths = train$BsmtFullBath + train$FullBath + (0.5 * (train$BsmtHalfBath + train$HalfBath))
test$total_baths = test$BsmtFullBath + test$FullBath + (0.5 * (test$BsmtHalfBath + test$HalfBath))

train$garagevar <- train$GarageCars + train$GarageArea
test$garagevar <- test$GarageCars + test$GarageArea

# Determining the correlation between variables and SalePrice
for (col in colnames(train)){
        if(is.numeric(train[,col])){
                if( abs(cor(train[,col],train$SalePrice)) > 0.4){
                        print(col)
                        print( cor(train[,col],train$SalePrice) )
                }
        }
}

# Linear regression model
lm.fit <- glm(SalePrice ~., data = train)
summary(lm.fit)

# Preparing the data to the MLP
maxs <- apply(full_data, 2, max) 
mins <- apply(Full_data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]
        