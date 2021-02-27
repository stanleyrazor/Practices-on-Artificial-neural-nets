# deep learning using h20

# libraries
library(h2o)

# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Artificial neural nets\\iris data")

# data

df<-(iris[sample(nrow(iris)), ])
View(df)

c1=h2o.init(max_mem_size = "2G",
            nthreads = 2,
            ip = "localhost",
            port = 54321)

iris_d1 <- h2o.deeplearning(1:4,5, as.h2o(iris), hidden=c(5,5),
                            export_weights_and_biases=T)
## deeplearning framework hidden layers(5,5)

iris_d1
plot(iris_d1)

h2o.weights(iris_d1, matrix_id=1)
h2o.weights(iris_d1, matrix_id=2)
h2o.weights(iris_d1, matrix_id=3)
h2o.biases(iris_d1, vector_id=1)
h2o.biases(iris_d1, vector_id=2)
h2o.biases(iris_d1, vector_id=3)

#plot weights connecting `Sepal.Length` to first hidden neurons
plot(as.data.frame(h2o.weights(iris_d1, matrix_id=1))[,1])


## plotting the data to observe trends

pairs(iris[1:4], main = "Scatterplot matrices of Iris Data",
      pch = 21, bg = c("red", "green3", "blue"))

## looking at their distributions
par(mfrow=c(1,4))
for (i in 1:4)
{
  hist(df[,i], main=colnames(df[i]))
}


###################################################
###################################################
#### UNSUPERVISED MACHINE LEARNING
## set autoencoder to true, remove the labels, realize 1:4

anomaly_model <- h2o.deeplearning(1:4,
                                  training_frame = as.h2o(iris),
                                  activation = "Tanh",
                                  autoencoder = TRUE,
                                  hidden = c(50,20,50),
                                  sparse = TRUE,
                                  l1 = 1e-4,
                                  epochs = 100)

anomaly_model
