## neural network to predict wine data

# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Artificial neural nets\\wine anlysis")

# data
wine<-read.csv("wine quality.csv", as.is=T)
View(wine)

# summary and structure
dim(wine)
summary(wine)
str(wine)

# loading required packages
library(neuralnet)

# preparing data for neural networks
max_data <- apply(wine, 2, max) # maximum of each column
min_data <- apply(wine, 2, min) # minimum for each column

data_scaled <- scale(wine,center = min_data, scale = max_data - min_data)
data_scaled<-as.data.frame(cbind(data_scaled, wine[12]))

# splittind data to test and train
index <- sample(1:nrow(wine),round(0.70*nrow(wine)))  #train-test on 70:30

# dividing data to train and test
train_data <- as.data.frame(data_scaled[index,])
test_data <- as.data.frame(data_scaled[-index,])

wine_model_ann<-neuralnet(quality~.,
                          data=train_data,
                          hidden=c(2,3,2,3),
                          linear.output = T
  
)

plot(wine_model_ann)

# tring to predict on the test data
View(test_data)
predicted_quality_ann<-compute(wine_model_ann, test_data[-12])

table(predicted_quality_ann$net.result, test_data[,12])

# correlation test
cor(predicted_quality_ann$net.result, test_data[,12])

# rescaling
quality_analysis<-data.frame()
quality_analysis[1:nrow(test_data), 1]<-test_data[, 12]
quality_analysis[1:nrow(test_data), 2]<-predicted_quality_ann$net.result
colnames(quality_analysis)<-c("actual quality", "predicted quality")
View(quality_analysis)

quality_analysis[, 3]<-(quality_analysis[, 1]*(max_data[12]-min_data[12]))+min_data[12]
quality_analysis[, 4]<-(quality_analysis[, 2]*(max_data[12]-min_data[12]))+min_data[12]
quality_analysis[, 4]<-trunc(quality_analysis[, 4]) # truncating

colnames(quality_analysis)<-c("actual quality", "predicted quality", "actual", "prediction")

# creating a table/confusion matrix
table(quality_analysis$prediction, quality_analysis$actual)
