## predicting tip/no tip from customer feedback

# directory
setwd("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/Artificial neural nets")

# loading data
custpred <- read.csv("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/Artificial neural nets/practices/customer prediction for tip.txt",
                     sep="", na.strings="")
View(custpred)
str(custpred)

# libraries
library(nnet)
library(NeuralNetTools)

names(custpred)

# train model based on output from input
customer_model<-nnet(CustomerWillTip~Service+Ambience+Food,
                     data=custpred,# data frame containing data
                     size =5,# hidden layers
                     rang=0.1,
                     decay=5e-2,
                     maxit=5000)

plotnet(customer_model,
        neg_col="black",
        bord_col="black",
        circle_col="white",
        alpha_val=0.75,
        prune_lty=2) # requires a certain library
garson(customer_model)  # requires a certain library

# trying with neural nets
customer_md<-neuralnet(CustomerWillTip~Service+Ambience+Food,
                       data=custpred,
                       hidden=10)

plot(customer_md)
cor(custpred$CustomerWillTip, customer_md$net.result[[1]])
analyze<-as.data.frame(cbind(custpred$CustomerWillTip, as.data.frame(customer_md$net.result)))
colnames(analyze)<-c("actual", "predicted will tip")
View(analyze)
