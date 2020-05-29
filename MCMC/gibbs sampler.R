#Section 7.2, Beta-binomial
betabi=function(x,a,b,n){
  beta(x+a,n-x+b)/(beta(a,b)*beta(x+1,n-x+1)*(n+1))}
nsim=10^4
n=15;a=3;b=7
X=T=rep(0,nsim)
T[1]=rbeta(1,a,b)               #initialize the chain
X[1]=rbinom(1,n,T[1])           #initialize the chain
for (i in 2:nsim){
  X[i]=rbinom(1,n,T[i-1])
  T[i]=rbeta(1,a+X[i],n-X[i]+b)
}
par(mfrow=c(1,2),mar=c(4,4,2,1))
hist(X[2000:nsim],nclass=16,col="grey",freq=FALSE, xlim=c(0,15),main="",xlab="X")
curve(betabi(x,a,b,n),from=0, to=15,col="gold4",lwd=2,add=TRUE)
hist(T[2000:nsim],nclass=134,col="grey",freq=FALSE,xlim=c(0,.8),main="", xlab=expression(theta))
curve(dbeta(x,shape1=a,shape2=b),from=0, to=.8,col="sienna",lwd=2,add=TRUE)


#Section 7.4, slice sampling in Example 7.10

fff=function(y)exp(-sqrt(y))/2;
nsim=10^3;x=rep(pi,nsim);x[1]=-log(runif(1))
for (i in 2:nsim){
  w=runif(1,min=0,max=fff(x[i-1]));x[i]=runif(1,min=0,max=(-log(2*w))^2)
}
X11(h=3.5);par(mfrow=c(1,2),mar=c(4,4,2,1))
hist(x,xlab="Slice Sampler",main="",xlim=c(0,40),ylim=c(0,.25),freq=FALSE,col="grey",breaks=250)
curve(fff,add=T,lwd=3,col="sienna",xlim=c(0,40))
acf(x,xlab="Autocorrelation",ylab="",lwd=3)


#gibbs sampling for mixmodel
library('mcsm')
gibbsmix()