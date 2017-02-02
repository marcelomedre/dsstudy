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

train$garagevar <- train$GarageCars + train$GarageArea
test$garagevar <- test$GarageCars + test$GarageArea

library(randomForest)
require(caret)
set.seed(415)
fit3 <- randomForest(SalePrice ~ BsmtUnfSF + tot_sq_footage + Neighborhood +
                      OverallQual + BsmtFinSF1 + TotalBsmtSF + YearRemodAdd +
                      GarageType + YearBuilt + OpenPorchSF + KitchenQual +
                      LandContour + HouseStyle + FireplaceQu + GarageYrBlt,
              data=train,
              importance = TRUE,
              ntree = 1000)
importance(fit3)

imp <- varImp(fit3)
head(rownames(imp)[order(imp$Overall, decreasing = TRUE)], n = 10)

Prediction3 <- predict(fit3, test)
submit3 <- data.frame(Id = test$Id, SalePrice = Prediction3)
write.csv(submit3, file = "Randomtree_02.csv", row.names = FALSE)

## BART Machine model

SEED <- 123
set.seed(SEED)
options(java.parameters="-Xmx500m")
library(bartMachine)
set_bart_machine_num_cores(4)
y <- log(SalePrice)
X <- train
X$SalePrice <- NULL
bart <- bartMachine(X, y, num_trees = 10, seed = SEED)

rmse_by_num_trees(bart, tree_list = c(seq(5,35, by = 5)),
                  num_replicates = 2)

#num_trees = 25 works better perhaps

bart2 <- bartMachine(X, y, num_trees = 25, seed = SEED)
par(mar = rep(2,4))
plot_convergence_diagnostics(bart2)

par(mar = c(0.1,0.1,0.1,0.1))
check_bart_error_assumptions(bart2)

var_selection_by_permute(bart2, num_reps_for_avg = 2)

log_pred <- predict(bart2, X_test)
pred <- exp(log_pred)
submit4 <- data.frame(Id = test$Id, SalePrice = pred)
write.csv(submit4, file = "bart_01.csv", row.names = FALSE)
