train <- read.csv("d:/paris/kineticTrain.csv",header=T,sep=",")
 
 
train[,2:dim(train)[2]]=sapply(train[,2:dim(train)[2]],as.numeric)
mean=c()
for( i in  1:nrow(train))
{
  avg=mean(as.numeric(train[i,2:131]))
  mean=c(mean,avg)
}
train[,"mean"]=mean


#to do  feature  2  mode
mode=c()
for( i in  1:nrow(train))
{
  x=as.numeric(train[i,2:131])
  m=as.numeric(names(sort(-table(x)))[1])
  mode=c(mode,m)
}

train[,"mode"]=mode

#to do  feature  3  standard  deviation
sd=c()
for( i in  1:nrow(train))
{
  std=sd(as.numeric(train[i,2:131]))
  sd=c(sd,std)
}
train[,"sd"]=sd



# to do   feature  4  median
median=c()
for( i in  1:nrow(train))
{
  me=median(as.numeric(train[i,2:131]))
  median=c(median,me)
}
train[,"median"]=median

# compute kmean centroids
centroid=c()
for( i in 1:nrow(train))
{
   kmeansObj=kmeans(as.numeric(train[i,2:131]),centers=1)
  centroid=c(centroid,as.numeric(kmeansObj$centers))
}

train[,"centroid"]=log(centroid+1)


#compute euclidean distance
eu_dist=c()
for( i in 1:nrow(train))
{
  origin=rep(0,131)
  point=as.numeric(train[i,2:131])
  d=as.numeric(dist(rbind(origin,point)))
  eu_dist=c(eu_dist,d)
}


train[,"eu_dist"]=((eu_dist-1.618)/1.618)


max=c()
 
for( i in  1:nrow(train))
{
  mx=max(as.numeric(train[i,2:31]))
  max=c(max,mx)
 
 
}
train[,"max"]=max
temp=train
temp[,2:131]=sapply(temp[,2:131],trunc)
newKin=c()
for( i in  1:nrow(temp))
{
  
  freq=table(as.numeric(temp[i,2:131]))
  probs=freq/130
  newKin=c(newKin,sum(probs^2))
  
  
}

train[,"NEWKINETIC"]=newKin

write.csv(train,"d:/paris/expandedTrain",row.names=F)
 
