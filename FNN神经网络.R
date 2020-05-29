library(grid)    
library(MASS)
library(neuralnet) #加载相应的程序包，这里用的是neuralnet，还可以用nnet、tensorflow实现
p <- matrix(c(1.78,1.96,1.86,1.72,2.00,2.00,1.96,1.74,1.64,1.82,1.90,1.70,1.82,1.82,1.14,1.18,1.20,1.24,1.26,1.28,1.30,1.36,1.38,1.38,1.38,1.40,1.48,1.54),14,2,byrow = T)
ttt <- c(1,1,1,0,1,1,1,0,0,0,0,0,0,0)
trainingdata <- cbind(p,ttt) #合并数据集
trainingdata
colnames(trainingdata) <- c("Input1","Input2","Output")
sgn<-function(x){
  if(x>=0){y<-x}else{y<-0}
  y
}
net <- neuralnet(Output~Input1+Input2,trainingdata, hidden=20, threshold=0.005, learningrate = 0.1, algorithm = "rprop+", err.fct = "ce", act.fct = sgn,linear.output="FALSE")  #建立神经网络模型解决回归预测，三个输入，一个输出，隐藏层20个神经元，阈值为0.005，学习率为0.1，选用rprop+方法进行参数优化，损失函数SSE，激活函数logistic  
print(net)   
plot(net)   #画网络图
testdata <- matrix(c(2.08,1.56),1,2,byrow = T) #建立测试集
net.results <- compute(net, testdata)   #利用训练好的模型进行预测
ls(net.results)   
print(net.results$net.result)  #输出预测值
if(print(net.results$net.result)<0.5){print("0")}else {print("1")}
