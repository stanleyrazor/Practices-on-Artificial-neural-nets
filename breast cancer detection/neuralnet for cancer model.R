## using neuralnets to predict cancer cells

# directory
setwd("C:\\Users\\stanley\\Desktop\\MISCELLANEOUS R\\ml projects\\Artificial neural nets\\breast cancer detection")

# data
data<-read.csv("data.csv", stringsAsFactors = T, na.strings = "")
View(data)

# loading required libraries
library(neuralnet)
library(class)

# omitting ID column
data<-data[-1]

# summary and structure
summary(data)
str(data)
dim(data)

# preparing data for neural network
norm_fn<-function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

data_norm<-as.data.frame(lapply(data[-1], norm_fn))
View(data_norm)

data_labs<-data[1]
# benign takes a -1 and malignant takes a 1
data_labs_num<-ifelse(data_labs=="B", -1, 1)

data_norm<-as.data.frame(cbind(data_norm, as.data.frame(data_labs_num)))

# splitting data to train and test
# randomizing data
index <- sample(1:nrow(data_norm),round(0.70*nrow(data_norm)))  
#train-test on 70:30

# dividing data to train and test
train_data <- as.data.frame(data_norm[index,])
test_data <- as.data.frame(data_norm[-index,])

# creating the neural net
cancer_model<-neuralnet(diagnosis~., # formulae
                        data=train_data, # dataset
                        hidden=15 #hidden layers
                        )

plot(cancer_model)


##################################################
# USING DEEP LEARNING
cancer_model_deep<-neuralnet(diagnosis~., # formulae
                        data=train_data, # dataset
                        hidden=c(3,3,3),
                        linear.output=F
                        #hidden layers
)

plot(cancer_model_deep)

predict_test_d <- compute(cancer_model_deep, test_data[,1:30])

pred_test_output_d<-ifelse(predict_test_d$net.result<5.893656e-05, "B", "M")

actual_test_output<-ifelse(test_data$diagnosis==-1, "B", "M")

# creating a "confusion matrix"
table(pred_test_output_d, actual_test_output)

##################################################

# letting the model predict the data
predict_test <- compute(cancer_model, test_data[,1:30])

pred_test_output<-ifelse(predict_test$net.result<0, "B", "M")

actual_test_output<-ifelse(test_data$diagnosis==-1, "B", "M")

# creating a "confusion matrix"
table(pred_test_output, actual_test_output)

## with hidden = 15, its 96 % accurate
########################################################
# comparing it with a knn model
trainlabs<-ifelse(train_data$diagnosis==-1, "B", "M")
trainlabs<-factor(trainlabs, levels=c("B","M"), labels=c("B", "M"))
actual_test_output<-factor(actual_test_output, 
                           levels=c("B", "M"), labels=c("B", "M"))

knnpred<-knn(train_data[, 1:30], test_data[, 1:30], cl=trainlabs, 
             k=5)

analyze_k(train_data[, 1:30], test_data[, 1:30], trainlabs, 
          actual_test_output, k=20)

table(knnpred, actual_test_output)
## with k=5, its 97% accurate
