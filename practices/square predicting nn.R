### neural net

# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Artificial neural nets")

# data
x<-0:10
y<-x**2
mydata<-as.data.frame(cbind(x,y))
colnames(mydata)<-c("Input", "Output")
View(mydata)

# loading required libraries
library(neuralnet)

# trainig the model
model=neuralnet(formula = Output~Input,
                data = mydata,
                hidden=10,
                threshold=0.01 )
print(model)
plot(model)

#Check the data - actual and predicted
final_output=as.data.frame(cbind(mydata$Input, mydata$Output,
                    as.data.frame(model$net.result)))

colnames(final_output) = c("Input", "Expected Output",
                           "Neural Net Output")
View(final_output)



####################################################################
x<-0:10
y<-x**2
z<-sin(y)
q<-tan(y)
w<-cos(y)
mydata1<-as.data.frame(cbind(x,y,z,q,w))

model_1<-neuralnet(w~., hidden=5, data=mydata1)

per_i<-cbind(mydata1$w, as.data.frame(model_1$net.result))
colnames(per_i)<-c("real values", "predicted values")
View(per_i)
plot(model_1)
cor(mydata1$w, model_1$net.result[[1]])

model_10<-neuralnet(w~., hidden=10, data=mydata1)
plot(model_10)
cor(mydata1$w, model_10$net.result[[1]])
