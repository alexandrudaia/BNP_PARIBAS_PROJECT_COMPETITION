set.seed(1234)
train <- read.csv("d:/paris/expandedTrain.csv",header=T,sep=",")
proportion=(20/100)*dim(train)[1]
proportion=trunc(proportion)
split=sample(1:dim(train)[1],proportion)
trainEns1=train[-split,]
optimization1=train[split,]
write.csv(trainEns1,"d:/paris/trainEns1.csv",row.names = F)
write.csv(optimization1,"d:/paris/optimization1.csv",row.names = F)
write.csv(split,"d:/paris/spilt1.csv",row.names = F)
