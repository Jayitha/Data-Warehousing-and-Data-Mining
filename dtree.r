#http://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
#install.packages('carat')
#install.packages('rplot.plot')
#Classification script for DWDM
library(caret)
library(rpart.plot)
s <- "Information"
homicide_df <- read.csv("homicide.csv", sep = ',', header = TRUE)
#To take a look at the data frame
str(homicide_df)
#To see the first 5-6 rows of data
print(head(homicide_df))
#Slice data to create training and testing data
intrain <- createDataPartition(y = homicide_df$Year, p= 0.8, list = FALSE)
training <- homicide_df[intrain,]
testing <- homicide_df[-intrain,]
#Checking dimensions of training and testing data
print(dim(training))
print(dim(testing))
#Preprocessing, checking for NULL values
print(anyNA(homicide_df))
#To look at the summary
print(summary(homicide_df))
#Training
#Setting Training control parameters
#repeatedcv = repeated cross validation
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#Training
#split=information for information gain
#split=gini for gini index
dtree_fit <- train(Year ~., data = training, method = "rpart",
                   parms = list(split = s),
                   trControl=trctrl,
                   tuneLength = 10)
print(dtree_fit)
#Visualisation
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
#Predict for testing data
test_pred <- predict(dtree_fit, newdata = testing)
#Confusion matrix
print(confusionMatrix(test_pred, testing$Year ))