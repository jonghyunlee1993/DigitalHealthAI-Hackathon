library(caret)
library(tidyverse)

setwd("~/GitRepo/Personal/DigitalHealth/")

train_data = read.csv("./data/train_data.csv")
train_data = train_data[, -c(1, 21)]

test_data = read.csv("./data/test_data.csv")
test_data = test_data[, -c(1)]

X = train_data[, -c(19)]
y = train_data[, 19]
X$Action = as.factor(X$Action)

train_index = createDataPartition(y, p = .8, list = FALSE, times = 1)

X_train = X[train_index, ]
X_test  = X[-c(train_index), ]

y_train = y[train_index]
y_test  = y[-c(train_index)]

train = cbind(X_train, y_train)
model = lm(y_train ~ ., data=train)
final_model = step(model, by='both')

pred = predict(final_model, X_test)
RMSE(pred, y_test)
