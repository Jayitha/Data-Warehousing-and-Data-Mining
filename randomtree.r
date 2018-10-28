#http://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
#install.packages('carat')
#install.packages('rplot.plot')
#Classification script for DWDM
library(randomForest)
library(caret)
library(rpart.plot)
homicide_df <- read.csv("homicide.csv", sep = ',', header = TRUE)
#To take a look at the data frame
str(homicide_df)
shuffle_index <- sample(1:nrow(homicide_df))
homicide_df <- homicide_df[shuffle_index, ]
print(head(homicide_df))
intrain <- createDataPartition(y = homicide_df$disposition, p= 0.8, list = FALSE)
training <- homicide_df[intrain,]
testing <- homicide_df[-intrain,]
print(dim(training))
print(dim(testing))
print(anyNA(homicide_df))
print(prop.table(table(training$disposition)))
print(prop.table(table(training$disposition)))
print(summary(homicide_df))
rf = randomForest(disposition~., training)
print(rf)
print(importance(rf))
