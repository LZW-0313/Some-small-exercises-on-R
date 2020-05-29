data<-read.csv("data.csv")
data<-na.omit(data)
fix(data)
data<-na.omit(data)
ind<-sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
set.seed(100)#分测试集与训练集
train<-data[ind==1,]
test<-data[ind==2,]
library(randomForest)
n<-length(names(train))
set.seed(100)#选取最优结点数
for(i in 1:(n-1)){
  mtry_fit<-randomForest(sort~.,data=train,mtry=i)
  err<-mean(mtry_fit$err.rate)
  print(err)
}
set.seed(100)
ntree_fit<-randomForest(sort~.,data=train,mtry=3,ntree=1000)#拟合模型
plot(ntree_fit)
ntree_fit#查看模型拟合情况
importance<-importance(x=ntree_fit)#看重要性
importance
pred1<-predict(ntree_fit,data=train)
Freq1<-table(pred1,train$sort)
sum(diag(Freq1))/sum(Freq1)        #预测精度
plot(margin(ntree_fit,test$sort),main=观测值被判断正确的概率图)




