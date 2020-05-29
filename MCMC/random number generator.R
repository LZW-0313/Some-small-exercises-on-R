#generate beta random number
optimize(f=function(x){dbeta(x,2.7,6.3)},
         interval = c(0,1), maximum=TRUE)$objective
Nsim=2500
a = 2.7;b=6.3
M=2.67
u=runif(Nsim,max=M)
y=runif(Nsim)
x=y[u<dbeta(y,a,b)]
xu=u[u<dbeta(y,a,b)]

#generate truncated normal random numbers by inverse transform method
for(i in 2:nsim){
  temp=runif(n-m,min=pnorm(a,mean=that[i-1],sd=1),
             max=1)
  zbar[i]=mean(qnorm(temp,mean=that[i-1],sd=1))
  that[i]=rnorm(1,mean=(m/n)*xbar+(1-m/n)*zbar,
                sd=sqrt(1/n))}