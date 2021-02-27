## deep net
# fetching data from ISLR
library(ISLR)
library(neuralnet)
data("College")

# data
dat<-College
View(dat)


# visualizing data
par(mfrow=c(3,6))
for (i in 2:ncol(dat))
{
  hist(dat[,i], main=colnames(dat[i]))
}
# columns 6,9,10,11,12,15,17,18 are normal distributed

# preparing data for neuralnet
max.data<-apply(dat[, 2:18], 2, max)
min.data<-apply(dat[, 2:18], 2, min)

data.sc <- scale(dat[,2:18],center = min.data, scale = max.data - min.data)
sum<-summary(data.sc)

Private = as.numeric(College$Private)-1
data.sc = cbind(Private,data.sc)

index = sample(1:nrow(dat),round(0.70*nrow(dat)))
train_data <- as.data.frame(data.sc[index,])
test_data <- as.data.frame(data.sc[-index,])

deep.net = neuralnet(Private~.,data=train_data,hidden=c(5,5,5,2),linear.output=F)
plot(deep.net)

predicted_data <- compute(deep.net,test_data[,2:18])
print(head(predicted_data$net.result))
predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0)
table(test_data$Private,predicted_data$net.result)

################################################################
############# comparison to knn
library(class)

pred<-knn(train_data[, 2:18], test_data[, 2:18], train_data[, 1], k=5)
table(pred, test_data[, 1])
analyze_k(train_data[, 2:18], test_data[, 2:18], train_data[, 1], test_data[, 1], k=100)

####### at k=5, level of accuracy is 95 %%
################################################################
