#Importing necessary libraries for the decision tree
library(rpart)
library(rpart.plot)

#Reading the titanic CSV
df <- read.csv('titanic.csv', header = TRUE, sep = ',')

#Splitting the dataset into train/test
#Removing "Survived" column from test set
train <- head(df, n = 800)
test <- subset(df, select = -c(Survived))
test <- tail(test, n = 91)

#Creating and visualizing decision tree model using train data
dectree <- rpart(Survived~., data = train, method = 'class')
rpart.plot(dectree, nn=TRUE)

#Predicting
t_pred <- predict(dectree, test, type=c('class'))
t_prob <- predict(dectree, test, type=c('prob'))

#Added "Survived" values from original data. Adding prediction and probability info.
test <- cbind(test, Survived=tail(df$Survived, n=91))
test <- cbind(test, Prediction=t_pred)
test <- cbind(test, Probability=t_prob)
t_accu <- sum(tail(df$Survived, n=91) == test$Prediction) / length(tail(df$Survived, n=91))

#Output for decision tree accuracy and comparison for first 20 rows of test dataset
print(paste('Model Accuracy', t_accu))
print(head(test, n=20))
