# analysis of boston dataset from MASS package

# direcory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Artificial neural nets\\practices\\boston house data")

# libraries
library(pacman)
p_load(MASS, neuralnet)

# replicate randomizing
set.seed(1)

# data
data = Boston
View(data)

# normalizing the data
max_data <- apply(data, 2, max)
min_data <- apply(data, 2, min)
data_scaled <- scale(data,center = min_data, scale = max_data - min_data)

# randomizing data
index <- sample(1:nrow(data),round(0.70*nrow(data)))  #train-test on 70:30

# dividing data to train and test
train_data <- as.data.frame(data_scaled[index,])
test_data <- as.data.frame(data_scaled[-index,])

#n<-names(data)
#f = as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

net_data <- neuralnet(medv~. ,# the formulae, dor specifies the rest
                      data=train_data, # the dataframe
                      hidden=10, # hidden layers
                      linear.output=T
                      )

plot(net_data)

# we let it predict in the test data ommitting the 14th column which
# has the dependent variable
predict_net_test <- compute(net_data,test_data[,1:13])

# since we had scaled data , we return it back to normal
predict_net_test_start <- predict_net_test$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

# returning test data to the original way it was
test_start <- as.data.frame((test_data$medv)*(max(data$medv)-min(data$medv))+min(data$medv))

# finding the MEAN SQUARED ERROR
MSE.net_data <- sum((test_start - predict_net_test_start)^2)/nrow(test_start)


# We want to compare the accuracy of the neural net to a linear regression algorithm
Regression_Model <- lm(medv~., data=data)
summary(Regression_Model)
test <- data[-index,]
predict_lm <- predict(Regression_Model,test)
MSE.lm <- sum((predict_lm - test$medv)^2)/nrow(test)


MSE.net_data # mse of the neuralnet model
MSE.lm # mse of the linear regression model
