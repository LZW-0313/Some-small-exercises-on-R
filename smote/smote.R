library(DMwR)
data<-rnorm(1000)
hist(data)
a<-as.numeric(quantile(data,1/8))
b<-as.numeric(quantile(data,7/8))
length(data)
A<-c()
B<-c()
for(i in 1:1000){
 if(data[i]>a&&data[i]<b){
  A[i]<-data[i]
 }
 else{B[i]<-data[i]}
}
A<-na.omit(A)
B<-na.omit(B)
A1<-c(1:750)
for(i in 1:750){A1[i]<-1}
B1<-c(1:250)
for(i in 1:250){B1[i]<-0}
AA<-cbind(A,A1)
BB<-cbind(B,B1)
fdata<-rbind(AA,BB)
fdata<-data.frame(fdata)
fix(fdata)
fdata$Y<-factor(fdata$Y)
table(fdata$Y)
newData <- SMOTE(Y ~ ., fdata,perc.over = 600,perc.under=115,k=5)
table(newData$Y)
par(mfrow = c(1, 2))
plot(fdata[,1],pch = 19 + as.integer(fdata[,2]),main="Original Data")
plot(newData[,1],pch = 19 + as.integer(newData[,2]),main = "SMOTE'd Data")




#kmeans聚类
library(DMwR)
write.csv(iris,file="IRIS.csv")
iris<-read.csv("IRIS.csv")
real<-iris[5]
newIris<-iris[1:4]
#传统SMOTE算法
nData <- SMOTE(Species ~ ., iris,perc.over = 200,perc.under=225,k=5)
table(nData$Species)
par(mfrow = c(1, 2))
plot(iris$Sepal.Length,iris$Sepal.Width,pch = 19 + as.integer(iris$Species),main="Original Data")
plot(nData$Sepal.Length,nData$Sepal.Width,pch = 19 + as.integer(nData$Species),main = "SMOTE'd Data")
#KM-SMOTE算法
kc<-kmeans(newIris,3)
kc$cluster
fitted(kc)
sort<-cbind(real,kc$cluster)
plot(newIris[c("Sepal.Length","Sepal.Width")],col=kc$cluster,pch=as.integer(real$Species)) #聚类图
data<-cbind(newIris,kc$cluster)
fix(data)
data$Y<-factor(data$Y)
newData <- SMOTE(Y ~ ., data,perc.over = 200,perc.under=225,k=5)
table(newData$Y)
par(mfrow = c(1, 2))
plot(data$Sepal.Length,data$Sepal.Width,pch = 19 + as.integer(data$Y),main="Original Data")
plot(newData$Sepal.Length,newData$Sepal.Width,pch = 19 + as.integer(newData$Y),main = "KM-SMOTE'd Data")


###################################################
data1<-data[which(data$Y==1),]#将数据分组
data2<-data[which(data$Y==2),]
data3<-data[which(data$Y==3),]
data1$Y<-factor(data1$Y)
data2$Y<-factor(data2$Y)
data3$Y<-factor(data3$Y)
#分组后的smote算法
ddata<-rbind(data3,data2)
ddata$Y<-factor(ddata$Y)
dddata<-rbind(data1,data3)
dddata$Y<-factor(dddata$Y)
newData1 <- SMOTE(Y ~ .,ddata,perc.over = 100,perc.under=120,k=5)
table(newData1$Y)
newData2 <- SMOTE(Y ~ .,dddata,perc.over = 300,perc.under=120,k=5)
table(newData2$Y)
newdata<-rbind(newData1,newData2)
table(newdata$Y)
par(mfrow = c(1, 2))
plot(data$Sepal.Length,data$Sepal.Width,pch = 19 + as.integer(data$Y),main="Original Data")
plot(newdata$Sepal.Length,newdata$Sepal.Width,pch = 19 + as.integer(newdata$Y),main = "KM-SMOTE'd Data")



#############################4.30号完工###################################
#smote插值
library(DMwR)
set.seed(1)#生成重叠数据
a<-runif(100,min=-10,max=10)
b<-runif(100,min=-10,max=10)
data1<-cbind(a,b)
data1<-data.frame(data1)
sort<-c()
for(i in 1:100){
  sort[i]<-1
}
data1<-cbind(data1,sort)
set.seed(2)
a<-runif(20,min=5,max=15)
b<-runif(20,min=-5,max=5)
data2<-cbind(a,b)
data2<-data.frame(data2)
sort<-c()
for(i in 1:20){
  sort[i]<-2
}
data2<-cbind(data2,sort)
data<-rbind(data1,data2)
View(data)
plot(data$a,data$b,pch = 19 + as.integer(data$sort),main="Original Data")#原始数据可视化
data2_smote<-data2[-which((data2$a>7.5&data2$a<12.5)&(data2$b>-2.5&data2$b<2.5)),]
data_smote<-rbind(data2_smote,data1)
data_smote$sort<-factor(data_smote$sort)
newData<- SMOTE(sort~ .,data_smote,perc.over = 400,perc.under=240,k=5)
table(newData$sort)
newdataf<-rbind(newData,data2[which((data2$a>7.5&data2$a<12.5)&(data2$b>-2.5&data2$b<2.5)),])
par(mfrow = c(1, 2))
plot(data$a,data$b,pch = 19 + as.integer(data$sort),main="Original Data")
plot(newdataf$a,newdataf$b,pch = 19 + as.integer(newdataf$sort),main = "SMOTE'd Data")
table(data$sort)
table(newdataf$sort)

#随机森林
data1<-read.csv("data1.csv")
newDataf<-read.csv("newDataf.csv")
#对原始数据分类
ind<-sample(2,nrow(data1),replace=TRUE,prob=c(0.7,0.3))
set.seed(100)#分测试集与训练集
train<-data1[ind==1,]
test<-data1[ind==2,]
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
#对插值后数据分类
ind<-sample(2,nrow(newDataf),replace=TRUE,prob=c(0.7,0.3))
set.seed(100)#分测试集与训练集
train<-newDataf[ind==1,]
test<-newDataf[ind==2,]
n<-length(names(train))
set.seed(100)#选取最优结点数
for(i in 1:(n-1)){
  mtry_fit<-randomForest(sort~.,data=train,mtry=i)
  err<-mean(mtry_fit$err.rate)
  print(err)
}
set.seed(100)
ntree_fit_somte<-randomForest(sort~.,data=train,mtry=3,ntree=1000)#拟合模型
plot(ntree_fit_somte)
ntree_fit#查看模型拟合情况
importance<-importance(x=ntree_fit)#看重要性
importance
pred1<-predict(ntree_fit,data=train)
Freq1<-table(pred1,train$sort)
sum(diag(Freq1))/sum(Freq1)        #预测精度