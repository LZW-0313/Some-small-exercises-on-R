#0+x+y
library(rgl)
x<-seq(-5,5,by=0.5)
y<-seq(-5,5,by=0.5)
f<-function(x,y){
  min(0,x,y)
}
z<-matrix(1:441)
dim(z)<-c(21,21)
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j]<-f(x[i],y[j])
  }
}
dim(x)<-c(21,1)
X<-cbind(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
dim(y)<-c(1:21)
Y<-rbind(y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y)
plot3d(X,Y,z)

#10+5.5x+0x^2+8.5y+6.5y^2+4.5xy
library(rgl)
x<-seq(-10,10,by=0.1)
y<-seq(-10,10,by=0.1)
f<-function(x,y){
  max(10,5.5+x,2*x,8.5+y,6.5+2*y,4.5+x+y)
}
z<-matrix(1:40401)
dim(z)<-c(201,201)
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j]<-f(x[i],y[j])
  }
}
dim(x)<-c(201,1)
mylist<-list(1:201)
mylist[[1]]<-x
for(i in 1:200){
  mylist[[i+1]]<-cbind(mylist[[i]],x)
}
X<-mylist[[201]]  
dim(y)<-c(1,201)
mylist2<-list(1:201)
mylist2[[1]]<-y
for(i in 1:200){
  mylist2[[i+1]]<-rbind(mylist2[[i]],y)
}
Y<-mylist2[[201]] 
plot3d(X,Y,z,col="blue")

#5+4x+2.25x^2+0x^3+4y+2.5xy+1x^2y+3y^2+1.5xy^2+1.5y^3
library(rgl)
x<-seq(-10,10,by=0.1)
y<-seq(-10,10,by=0.1)
f<-function(x,y){
  max(5,4+x,2.25+2*x,3*x,4+y,2.5+x+y,1+2*x+y,3+2*y,1.5+x+2*y,1.5+3*y)
}
z<-matrix(1:40401)
dim(z)<-c(201,201)
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j]<-f(x[i],y[j])
  }
}
dim(x)<-c(201,1)
mylist<-list()
mylist[[1]]<-x
for(i in 1:200){
  mylist[[i+1]]<-cbind(mylist[[i]],x)
}
X<-mylist[[201]]  
dim(y)<-c(1,201)
mylist2<-list()
mylist2[[1]]<-y
for(i in 1:200){
  mylist2[[i+1]]<-rbind(mylist2[[i]],y)
}
Y<-mylist2[[201]] 
plot3d(X,Y,z,col="blue")