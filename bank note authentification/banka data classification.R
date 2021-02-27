# banknote authentification

# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Artificial neural nets\\bank note authentification")

# loading data
bank_data<-read.csv("data_banknote_authentication.txt", sep=",", 
                    as.is=T, header=F)
colnames(bank_data)<-c("Variance", "Skewness", "Kurtosis", "Entropy", "Class")
View(bank_data)

# randomizing the data
random<-sample(1:nrow(bank_data))
bdata<-data.frame()
for (i in 1:length(random))
{
  bdata[i, 1:ncol(bank_data)]<-bank_data[random[i], 1:ncol(bank_data)]
}
View(bdata)

## changing to factor
unique(bdata$Class)
bdata$Class<-factor(bdata$Class, levels=c(0,1), labels=c(0,1))

## dividing into train and test
bdata_train<-bdata[1:1271, ]
bdata_test<-bdata[1272:1372, ]

norm_z<-function(x)
{ ## normalization
  return((x-mean(x))/sd(x))
}

bdata_train<-as.data.frame(lapply(bdata_train[1:4], norm_z))
bdata_train$Class<-bdata[1:1271, 5]
bdata_test<-as.data.frame(lapply(bdata_test[1:4], norm_z))
bdata_test$Class<-bdata[1:1271, 5]


bank_model <- neuralnet(Class ~ ., data = bdata_train, hidden = 4)
plot(bank_model)

model_eval <- compute(bank_model, bdata_test[1:4])

# predicting
predicted_banknote <- model_eval$net.result

# correlation 
cor(predicted_banknote, bdata_test$Class)

table(predicted_banknote, bdata_test[5])
