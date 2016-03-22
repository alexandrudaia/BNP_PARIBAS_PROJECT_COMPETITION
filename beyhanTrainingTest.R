test <- read.csv("d:/paris/kineticTest.csv",header=T,sep=",")
 
 

# beyhaan  features :
#example feature 1 :
test[,2:dim(test)[2]]=sapply(test[,2:dim(test)[2]],as.numeric)
mean=c()
for( i in  1:nrow(test))
{
  avg=mean(as.numeric(test[i,2:131]))
  mean=c(mean,avg)
}
test[,"mean"]=mean


#to do  feature  2  mode
mode=c()
for( i in  1:nrow(test))
{
  x=as.numeric(test[i,2:131])
  m=as.numeric(names(sort(-table(x)))[1])
  mode=c(mode,m)
}

test[,"mode"]=mode

#to do  feature  3  standard  deviation
sd=c()
for( i in  1:nrow(test))
{
  std=sd(as.numeric(test[i,2:131]))
  sd=c(sd,std)
}
test[,"sd"]=sd



# to do   feature  4  median
median=c()
for( i in  1:nrow(test))
{
  me=median(as.numeric(test[i,2:131]))
  median=c(median,me)
}
test[,"median"]=median

# compute kmean centroids
centroid=c()
for( i in 1:nrow(test))
{
  kmeansObj=kmeans(as.numeric(test[i,2:131]),centers=1)
  centroid=c(centroid,as.numeric(kmeansObj$centers))
}

test[,"centroid"]=log(centroid+1)


#compute euclidean distance
eu_dist=c()
for( i in 1:nrow(test))
{
  origin=rep(0,131)
  point=as.numeric(test[i,2:131])
  d=as.numeric(dist(rbind(origin,point)))
  eu_dist=c(eu_dist,d)
}


test[,"eu_dist"]=((eu_dist-1.618)/1.618)


max=c()

for( i in  1:nrow(test))
{
  mx=max(as.numeric(test[i,2:31]))
  max=c(max,mx)
  
  
}
test[,"max"]=max
temp=test
temp[,2:131]=sapply(temp[,2:131],trunc)
newKin=c()
for( i in  1:nrow(temp))
{
  
  freq=table(as.numeric(temp[i,2:131]))
  probs=freq/130
  newKin=c(newKin,sum(probs^2))
  
  
}

test[,"NEWKINETIC"]=newKin

write.csv(test,"d:/paris/expandedTest",row.names=F)
