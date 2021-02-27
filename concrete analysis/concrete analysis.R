# concrete strenth using ANN

# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Artificial neural nets")

# data
concrete<-read.csv("concrete.csv", as.is=T)
View(concrete)

# summary
summary(concrete)
str(concrete)
dim(concrete)

# normalizing the data columns
normalize <- function(x)
{
  return((x - min(x)) / (max(x) - min(x)))
}

# applying normalization
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# dividing data to train and test
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# loading neuralnet pakage
library(neuralnet)

# model
concrete_model <- neuralnet(strength ~ ., data = concrete_train)

# plotting the data
plot(concrete_model)

# evaluating model performance
model_results <- compute(concrete_model, concrete_test[1:8])

# predicting
predicted_strength <- model_results$net.result

# correlation 
cor(predicted_strength, concrete_test$strength)

# improving the model
concrete_model <- neuralnet(strength ~ ., data = concrete_train, hidden=5)

# plotting the data once again
plot(concrete_model)

