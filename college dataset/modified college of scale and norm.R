# a continuation and performance analysis of the latter college dataset.R

# visualizing data
par(mfrow=c(3,6))
for (i in 2:ncol(dat))
{
  hist(dat[,i], main=colnames(dat[i]))
}
# columns 6,9,10,11,12,15,17,18 are normal distributed
# we therefore need to normalize them using a norm function, while scaling the rest

# scale
scalefun<-function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

# normalize
norm.z<-function(x)
{
  return((x-mean(x))/sd(x))
}

to_be_normalized<-c(6,9,10,11,12,15,17,18)
to_be_scaled<-c(2,3,4,5,7,8,13,14,16)

sidedata<-dat
sidedatanorm<-sidedata[, to_be_normalized]
sidedatascale<-sidedata[, to_be_scaled]

for (i in 1:ncol(sidedatascale))
{
  sidedatascale[i]<-as.data.frame(lapply(sidedatascale[i], scalefun))
}

for (j in 1:ncol(sidedatanorm))
{
  sidedatanorm[j]<-as.data.frame(lapply(sidedatanorm[j], norm.z))
}

sidedata.new<-as.data.frame(cbind(sidedata[1], sidedatascale, sidedatanorm))
View(sidedata.new)
summary(sidedata.new)

# dividing data to train and test
set.seed(5)
index = sample(1:nrow(sidedata.new),round(0.80*nrow(sidedata.new)))
trdata <- as.data.frame(sidedata.new[index,])
tedata <- as.data.frame(sidedata.new[-index,])

######################################################
## trying knn
knnpred<-knn(trdata[, 2:18],tedata[, 2:18],trdata[, 1], k=5)
table(knnpred, tedata[, 1])
analyze_k(trdata[, 2:18],tedata[, 2:18],trdata[, 1], tedata[, 1], k=100)
# setseed 5 yields 92% accuracy
########################################################

######################################################
## trying neuralnets
library(neuralnet)
## in private factor, No-1 Yes-2, thus itll be No-0, Yes-1
trdata$Private<-as.numeric(trdata$Private)-1
tedata$Private<-as.numeric(tedata$Private)-1

datamodel<-neuralnet(Private~., 
                     data=trdata,
                     linear.output = F,
                     hidden=10)
netpred<-compute(datamodel, tedata[, 2:18])
netpred$net.result<-sapply(netpred$net.result,round,digits=0)
table(netpred$net.result, tedata[, 1])



library(nnet)
library(NeuralNetTools)
datamodelnnet<-nnet(Private~.,
                    data=trdata,
                    size=10,
                    maxit=1258
                    )
plotnet(datamodelnnet)
garson(datamodelnnet)

nnetpred<-predict(datamodelnnet, tedata[, 2:18])
nnetpredval<-sapply(nnetpred,round,digits=0)
table(nnetpredval, tedata[, 1])
### at 
