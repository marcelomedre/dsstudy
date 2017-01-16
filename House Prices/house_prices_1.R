setwd("C:/Users/Marcelo/Desktop/DsStudy/House Prices")

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



# Determining the correlation between variables and SalePrice
for (col in colnames(train)){
        if(is.numeric(train[,col])){
                if( abs(cor(train[,col],train$SalePrice)) > 0.5){
                        print(col)
                        print( cor(train[,col],train$SalePrice) )
                }
        }
}


library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(SalePrice ~ OverallQual + GrLivArea + GarageCars,
             data=train,
             control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
             
plot(fit)
text(fit)

fancyRpartPlot(fit)

Prediction1 <- predict(fit, test)
submit1 <- data.frame(Id = test$Id, SalePrice = Prediction1)
write.csv(submit1, file = "myfirstdtree.csv", row.names = FALSE)


#model using all the variables

fit2 <- rpart(SalePrice ~ .,
                     data=train,
                     method = "class")
fancyRpartPlot(fit2)

Prediction2 <- predict(fit2, test)
submit2 <- data.frame(Id = test$Id, SalePrice = Prediction1)
write.csv(submit2, file = "my2ndtree.csv", row.names = FALSE)

# improving the model
## looking for correlations among variables
cors = cor(train[ , sapply(train, is.numeric)])
high_cor = which(abs(cors) > 0.6 & (abs(cors) < 1))
rows = rownames(cors)[((high_cor-1) %/% 38)+1]
cols = colnames(cors)[ifelse(high_cor %% 38 == 0, 38, high_cor %% 38)]
vals = cors[high_cor]

cor_data = data.frame(cols=cols, rows=rows, correlation=vals)
cor_data

# ordering the df by using the correlation value
cor_data2 <- cor_data[order(-cor_data$correlation),]

head(cor_data2)

#one can use de # o bathrooms and the total sq_footage ans 1 variable
train$tot_sq_footage <- train$GrLivArea + train$TotalBsmtSF
test$tot_sq_footage <- test$GrLivArea + test$TotalBsmtSF

#Variable to combine the # of baths
train$total_baths = train$BsmtFullBath + train$FullBath + (0.5 * (train$BsmtHalfBath + train$HalfBath))
test$total_baths = test$BsmtFullBath + test$FullBath + (0.5 * (test$BsmtHalfBath + test$HalfBath))

library(randomForest)
require(caret)
set.seed(415)
fit3 <- randomForest(SalePrice ~ .,
                    data=train, 
                    importance=TRUE, 
                    ntree=10)
importance(fit3)

imp <- varImp(fit3)
head(rownames(imp)[order(imp$Overall, decreasing = TRUE)], n = 10)

#modelling with the 6 best variables
set.seed(415)
fit4 <- randomForest(SalePrice ~ BsmtUnfSF + tot_sq_footage + Neighborhood +
                             OverallQual + BsmtFinSF1 + TotalBsmtSF,
                     data=train, 
                     importance=TRUE, 
                     ntree=500)
importance(fit4)

plot(fit4)

Prediction3 <- predict(fit4, test)
submit4 <- data.frame(Id = test$Id, SalePrice = Prediction3)
write.csv(submit4, file = "myrandomtree.csv", row.names = FALSE)

#modelling with the 15 best variables
head(rownames(imp)[order(imp$Overall, decreasing = TRUE)], n = 15)

set.seed(415)
fit5 <- randomForest(SalePrice ~ BsmtUnfSF + tot_sq_footage + Neighborhood +
                             OverallQual + BsmtFinSF1 + TotalBsmtSF + YearRemodAdd +
                             GarageType + YearBuilt + OpenPorchSF + KitchenQual +
                             LandContour + HouseStyle + FireplaceQu + GarageYrBlt,
                     data=train, 
                     importance=TRUE, 
                     ntree=1500)
importance(fit5)

plot(fit5)

Prediction4 <- predict(fit5, test)
submit5 <- data.frame(Id = test$Id, SalePrice = Prediction3)
write.csv(submit5, file = "my2ndrandomtree.csv", row.names = FALSE)

# Using an inference tree random Forest
##### CONTINUE FROM HERE
#library(party)
#set.seed(415)

#model using the 6 best variables
#fit5 <- cforest(SalePrice ~ BsmtUnfSF + tot_sq_footage + Neighborhood +
#                       OverallQual + BsmtFinSF1 + TotalBsmtSF,
#               data = train, 
#               controls=cforest_unbiased(ntree=1000, mtry=3))
#sapply(train, class)
#sapply(test, class)

# Now let's make a prediction and write a submission file
#Prediction5 <- predict(fit5, test, OOB=TRUE)
#submit5 <- data.frame(Id = test$Id, SalePrice = Prediction3)
#write.csv(submit5, file = "ciforest.csv", row.names = FALSE)