getwd()                                     #�鿴��ȡ·��
data<-read.csv("HW_data.csv")               #��ȡ���ݣ�����Ϊdata
View(data)                                  #�鿴����
plot(data$�ܶ�,data$������,pch = 19 + as.integer(data$�ù�))
                                            #���ݿ��ӻ�
summary(data)                               #�˽�������ϸ��Ϣ
set.seed(2)                                 #�����ݼ���Ϊѵ��������Լ�
ind=sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train_data<-data[ind==1,]
test_data<-data[ind==2,]
##ѵ������#################################################
glm.fit=glm(train_data$�ù�~train_data$�ܶ�+train_data$������,family=binomial) #����logisticģ��
summary(glm.fit)                            #�鿴ģ����Ϣ(����ģ��ϵ��)
glm.probs<-predict(glm.fit,type="response") #����ģ��ѵ�����ݵ���Ӧ����
glm.pred<-rep("��",10)
glm.pred[glm.probs<.5]="��"                 #�����С��0.5��������Ϊ�����ϡ�
dim(glm.pred)<-c(10,1)                      #��ʵ�ʱ�ǩ��ģ��Ԥ��ֵ�ϲ���һ�����ݿ�,����Ϊa
real<-train_data$�ù�
dim(real)<-c(10,1)
predict<-glm.pred
real<-data.frame(real)
predict<-data.frame(predict)
a<-cbind(real,predict)
table(a,dnn=c("real","predict"))            #����table��������ģ�ͻ���ѵ�����Ļ�������
plot(table(a))                              #����������ӻ�
accurate<-(5+3)/10                          #��ѵ�����ϵ�׼ȷ��
x<-train_data$�ܶ�                          #��������ѵ�����ϵı��ֿ��ӻ�
y<-train_data$������
z<-train_data$�ù�
plot(x,y,pch = 19 + as.integer(z))
model=function(x){
  (-8.009*x+6.723)/9.008
}
curve(model,xlim=c(0,1),add=TRUE,col="red",lwd=3) 
##���Բ���#################################################
If_good<-function(x_1,x_2){                 #����Ԥ���Ƿ�Ϊ�ùϵĺ���
  y=1/(1+exp(-(x_1*8.009+x_2*9.008-6.723)))
  if(y>0.5)
    y<-c("��")
  else
    y<-c("��")
  y
}
predict<-c()
for(i in 1:7){
predict[i]<-If_good(test_data[i,1],test_data[i,2])}
dim(predict)<-c(7,1)
real<-test_data$�ù�
dim(real)<-c(7,1)
predict<-data.frame(predict)
real<-data.frame(real)
b<-cbind(predict,real)
table(b)                                    #��������
plot(b)
P<-1/1                                      #�����׼�����ȫ��
R<-1/4
accurate_text<-(1+3)/7                      #�鿴�ڲ��Լ��ϵ�׼ȷ��
x<-test_data$�ܶ�                           #�������ڲ��Լ��ϵı��ֿ��ӻ�
y<-test_data$������
z<-test_data$�ù�
plot(x,y,pch = 19 + as.integer(z))
model=function(x){
  (-8.009*x+6.723)/9.008
}
curve(model,xlim=c(0,1),add=TRUE,col="red",lwd=3) 
If_good(0.6,0.5)                            #��Ԥ�������ޱ�ǩ���������Ƿ�Ϊ�ù�
If_good(0.2,0.1)
####����LOOCV��logisticģ�͵Ĳ�������######################################
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
  glm.fit=glm(train_list[[i]]$�ù�~train_list[[i]]$�ܶ�+train_list[[i]]$������,family=binomial)
  coef[[i]]<-coef(glm.fit)
  glm.probs<-predict(glm.fit,type="response") #����ģ��ѵ�����ݵ���Ӧ����
  glm.pred<-rep("��",16)
  glm.pred[glm.probs<.5]="��"                 #�����С��0.5��������Ϊ�����ϡ�
  dim(glm.pred)<-c(16,1)                      
  real<-train_list[[1]]$�ù�
  dim(real)<-c(16,1)
  predict<-glm.pred
  real<-data.frame(real)
  predict<-data.frame(predict)
  a<-cbind(real,predict)
  table(a,dnn=c("real","predict"))            #����table��������ģ�ͻ���ѵ�����Ļ�������
  subset1<-subset(a,real=="��"&predict=="��")
  subset2<-subset(a,real=="��"&predict=="��")
  train_accurate[i]<-(dim(subset1)[1]+dim(subset2)[1])/16
  If_good<-function(x_1,x_2){                 #����Ԥ���Ƿ�Ϊ�ùϵĺ���
    y=1/(1+exp(-(x_1*coef[[i]][2]+x_2*coef[[i]][3]+coef[[i]][1])))
    if(y>0.5)
      y<-c("��")
    else
      y<-c("��")
    y
  }
  if(If_good(test_list[[i]]$�ܶ�,test_list[[i]]$������)==test_list[[i]]$�ù�)
  {test_accurate[i]<-1} else{test_accurate[i]<-0}
}
train_accurate<-data.frame(train_accurate)
test_accurate<-data.frame(test_accurate)
result<-cbind(train_accurate,test_accurate)
coef[[9]]
x<-data$�ܶ�                                  #��������ԭʼ���ݼ��ϵı��ֿ��ӻ�
y<-data$������
z<-data$�ù�
plot(x,y,pch = 19 + as.integer(z))
model=function(x){
  (-3.599018*x+0.5+4.312497)/11.288993
}
curve(model,xlim=c(0,1),add=TRUE,col="red",lwd=3) 