getwd()                                     #查看读取路径
data<-read.csv("HW_data.csv")               #读取数据，命名为data
View(data)                                  #查看数据
plot(data$密度,data$含糖率,pch = 19 + as.integer(data$好瓜))
                                            #数据可视化
summary(data)                               #了解数据详细信息
set.seed(2)                                 #将数据集分为训练集与测试集
ind=sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train_data<-data[ind==1,]
test_data<-data[ind==2,]
##训练部分#################################################
glm.fit=glm(train_data$好瓜~train_data$密度+train_data$含糖率,family=binomial) #建立logistic模型
summary(glm.fit)                            #查看模型信息(包括模型系数)
glm.probs<-predict(glm.fit,type="response") #计算模型训练数据的相应概率
glm.pred<-rep("是",10)
glm.pred[glm.probs<.5]="否"                 #将输出小于0.5的样本归为“坏瓜”
dim(glm.pred)<-c(10,1)                      #将实际标签与模型预测值合并成一个数据框,命名为a
real<-train_data$好瓜
dim(real)<-c(10,1)
predict<-glm.pred
real<-data.frame(real)
predict<-data.frame(predict)
a<-cbind(real,predict)
table(a,dnn=c("real","predict"))            #利用table函数计算模型基于训练集的混淆矩阵
plot(table(a))                              #混淆矩阵可视化
accurate<-(5+3)/10                          #在训练集上的准确率
x<-train_data$密度                          #分类器在训练集上的表现可视化
y<-train_data$含糖率
z<-train_data$好瓜
plot(x,y,pch = 19 + as.integer(z))
model=function(x){
  (-8.009*x+6.723)/9.008
}
curve(model,xlim=c(0,1),add=TRUE,col="red",lwd=3) 
##测试部分#################################################
If_good<-function(x_1,x_2){                 #构建预测是否为好瓜的函数
  y=1/(1+exp(-(x_1*8.009+x_2*9.008-6.723)))
  if(y>0.5)
    y<-c("是")
  else
    y<-c("否")
  y
}
predict<-c()
for(i in 1:7){
predict[i]<-If_good(test_data[i,1],test_data[i,2])}
dim(predict)<-c(7,1)
real<-test_data$好瓜
dim(real)<-c(7,1)
predict<-data.frame(predict)
real<-data.frame(real)
b<-cbind(predict,real)
table(b)                                    #混淆矩阵
plot(b)
P<-1/1                                      #计算查准率与查全率
R<-1/4
accurate_text<-(1+3)/7                      #查看在测试集上的准确率
x<-test_data$密度                           #分类器在测试集上的表现可视化
y<-test_data$含糖率
z<-test_data$好瓜
plot(x,y,pch = 19 + as.integer(z))
model=function(x){
  (-8.009*x+6.723)/9.008
}
curve(model,xlim=c(0,1),add=TRUE,col="red",lwd=3) 
If_good(0.6,0.5)                            #可预测任意无标签的新样本是否为好瓜
If_good(0.2,0.1)
####基于LOOCV的logistic模型的参数估计######################################
data<-read.csv("HW_data.csv") 
train_list<-list()
test_list<-list()
for(i in 1:17){
  train_list[[i]]<-data[-i,]}
for(i in 1:17){
  test_list[[i]]<-data[i,]
}
train_accurate<-c()
test_accurate<-c()
coef<-list()
for(i in 1:17){
  glm.fit=glm(train_list[[i]]$好瓜~train_list[[i]]$密度+train_list[[i]]$含糖率,family=binomial)
  coef[[i]]<-coef(glm.fit)
  glm.probs<-predict(glm.fit,type="response") #计算模型训练数据的相应概率
  glm.pred<-rep("是",16)
  glm.pred[glm.probs<.5]="否"                 #将输出小于0.5的样本归为“坏瓜”
  dim(glm.pred)<-c(16,1)                      
  real<-train_list[[1]]$好瓜
  dim(real)<-c(16,1)
  predict<-glm.pred
  real<-data.frame(real)
  predict<-data.frame(predict)
  a<-cbind(real,predict)
  table(a,dnn=c("real","predict"))            #利用table函数计算模型基于训练集的混淆矩阵
  subset1<-subset(a,real=="是"&predict=="是")
  subset2<-subset(a,real=="否"&predict=="否")
  train_accurate[i]<-(dim(subset1)[1]+dim(subset2)[1])/16
  If_good<-function(x_1,x_2){                 #构建预测是否为好瓜的函数
    y=1/(1+exp(-(x_1*coef[[i]][2]+x_2*coef[[i]][3]+coef[[i]][1])))
    if(y>0.5)
      y<-c("是")
    else
      y<-c("否")
    y
  }
  if(If_good(test_list[[i]]$密度,test_list[[i]]$含糖率)==test_list[[i]]$好瓜)
  {test_accurate[i]<-1} else{test_accurate[i]<-0}
}
train_accurate<-data.frame(train_accurate)
test_accurate<-data.frame(test_accurate)
result<-cbind(train_accurate,test_accurate)
coef[[9]]
x<-data$密度                                  #分类器在原始数据集上的表现可视化
y<-data$含糖率
z<-data$好瓜
plot(x,y,pch = 19 + as.integer(z))
model=function(x){
  (-3.599018*x+0.5+4.312497)/11.288993
}
curve(model,xlim=c(0,1),add=TRUE,col="red",lwd=3) 