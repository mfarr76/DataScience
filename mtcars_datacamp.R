rm(list = ls())
library('caret')

data("mtcars")

model <- lm(mpg ~ hp,  mtcars[1:20,])
predicted <- predict(model, mtcars[1:20, ])

actual <- mtcars[1:20, "mpg"]
sqrt(mean((predicted - actual)^2))

predicted2 <- predict(model, mtcars[21:32, ], type = "response")

actual2 <- mtcars[21:32, "mpg"]
sqrt(mean((predicted2 - actual2)^2))

model2 <- lm(mpg ~ hp, mtcars)
predicted_full <- predict(model2, mtcars, type = "response")

actual_full <- mtcars[, "mpg"]
sqrt(mean((predicted_full - actual_full)^2))

set.seed(42)
model_caret <- train(mpg ~ hp, mtcars, 
                     method = "lm", 
                     trControl = trainControl(
                       method = "cv", number = 10,
                       verboseIter = TRUE))

model_caret
predicted_caret <- predict(model_caret, mtcars)
sqrt(mean((predicted_caret - actual_full)^2))
predict.train(model_caret, newdata = mtcars)

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(mtcars$hp, p=0.75, list=FALSE)
trainSet <- mtcars[ index,]
testSet <- mtcars[-index,]

actual_train <- mtcars[-index, "mpg"]

set.seed(42)
model_caret <- train(mpg ~ hp, trainSet, 
                     method = "lm", 
                     trControl = trainControl(
                       method = "cv", number = 10,
                       verboseIter = TRUE))

predict_train <- predict.train(model_caret, newdata = testSet)
sqrt(mean((predict_train - actual_train)^2))



