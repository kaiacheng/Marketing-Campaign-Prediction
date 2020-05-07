#sample sizes(11162), variables(17)
dim(bank)

#variable names
names(bank)
summary(bank)

#the percentage of success (9.6%)
table(bank$poutcome)/dim(bank)[1]

#standarzie variables (duration,  campaign, pdays, previous)
S.duration <- scale(bank$duration)
S.campaign <- scale(bank$campaign)
S.pdays <- scale(bank$pdays)
S.previous <- scale(bank$previous)
cbind(S.duration, S.campaign, S.pdays, S.previous)
S.bank <-cbind(S.duration, S.campaign, S.pdays, S.previous)

#generate our data to training data and testing data. 80/20 principle (training data:testing data=8930:2232)
train.index<-1:8930
train.X<-S.bank[train.index,-16]
train.Y<-bank[train.index,+16]
test.X<-S.bank[-train.index,-16]
test.Y<-bank[-train.index,+16]

library(class)

#knn model
KNN_1<-knn(train=train.X, test=test.X, cl=train.Y, k=1)

#Confusion Matrix
Confusion.matrix<-table(Truth=test.Y,Prediction=KNN_1)
Confusion.matrix

#Correct rate 91.44% / incorrect rate 8.56%
sum(diag(Confusion.matrix))/sum(Confusion.matrix)
1-sum(diag(Confusion.matrix))/sum(Confusion.matrix)

#The actual success rate in our prediction is 13.98%.
Confusion.matrix[3,3]/sum(Confusion.matrix[,3])

k<-1:50
Rate<-numeric(length(k))
for(i in k)
{
  set.seed(1)
  KNN_k<-knn(train=train.X, test=test.X, cl=train.Y, k=i)
  Confusion.matrix<-table(Truth=test.Y,Prediction=KNN_k)
  ifelse(sum(Confusion.matrix[,2])!=0,
         Rate[i]<-Confusion.matrix[2,2]/sum(Confusion.matrix[,2]),
         Rate[i]<-0)
  print(i)
}
Rate

plot(k,Rate,type="l",lty=2,lwd=2,col=3,ylab="Hit rate",xlab="K value")
points(k,Rate,pch=19,lwd=2)

#K=37 or 39 has the highest success rate (47%) 
