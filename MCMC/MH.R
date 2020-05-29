#example 1

a = 2.7;b=6.3;c=2.669   #initial values
Nsim=5000
X =rep(runif(1),Nsim) #initialize the chains
for (i in 2:Nsim){
  Y=runif(1)
  rho=dbeta(Y,a,b)/dbeta(X[i-1],a,b)
  X[i]=X[i-1] + (Y - X[i-1])*(runif(1)<rho)
}
X11(h=3.5);plot(4500:4800,X[4500:4800],ty="l",lwd=2,xlab="Iterations",ylab="X")



par(mfrow=c(1,2),mar=c(2,2,1,1))
hist(X,nclass=150,col="grey",main="Metropolis-Hastings",fre=FALSE)
curve(dbeta(x,a,b),col="sienna",lwd=2,add=TRUE)
hist(rbeta(5000,a,b),nclass=150,col="grey",main="Direct Generation",fre=FALSE)
curve(dbeta(x,a,b),col="sienna",lwd=2,add=TRUE)


#example 2
library('mvtnorm')
#metropolis algorithm
t1 <- -2.5
t2 <- 2.5
#' Number of iterations.
M <- 5000

#'  Metropolis sampling
tt <- matrix(rep(0, 2*M), ncol = 2)
tt[1,] <- c(t1, t2)
for(i in 2:M){
  Y = mvrnorm(1,tt[i-1,],diag(2))
  rho = dmvnorm(Y,c(0,0),diag(2)*0.8)/dmvnorm(tt[i-1,],c(0,0),diag(2)*0.8)
  tt[i,] = tt[i-1,] + (Y - tt[i-1,])*(runif(1)<rho)
}

library(ggplot2)
theme_set(theme_minimal())
library(tidyr)
library(gganimate)
library(ggforce)
library(MASS)
library(rprojroot)
library(rstan)
setwd("C:/Users/lx/Desktop")



root<-has_dirname("BDA_R_demos-master")$make_fix_file()

#' Parameters of a normal distribution used as a toy target distribution
y1 <- 0
y2 <- 0
r <- 0.8
S <- diag(2)
S[1, 2] <- r
S[2, 1] <- r
#' Metropolis proposal distribution scale
sp <- 0.3

df100 <- data.frame(id=rep(1,100),
                    iter=1:100,
                    th1 = tt[1:100, 1],
                    th2 = tt[1:100, 2],
                    th1l = c(tt[1, 1], tt[1:(100-1), 1]),
                    th2l = c(tt[1, 2], tt[1:(100-1), 2]))
#' Sample from the toy distribution to visualize 90% HPD
#' interval with ggplot's stat_ellipse()
dft <- data.frame(mvrnorm(100000, c(0, 0), S))
# labels and frame indices for the plot
labs1 <- c('Draws', 'Steps of the sampler', '90% HPD')
p1 <- ggplot() +
  geom_jitter(data = df100, width=0.05, height=0.05,
              aes(th1, th2, color ='1'), alpha=0.3) +
  geom_segment(data = df100, aes(x = th1, xend = th1l, color = '2',
                                 y = th2, yend = th2l)) +
  stat_ellipse(data = dft, aes(x = X1, y = X2, color = '3'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('red', 'forestgreen','blue'), labels = labs1) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA, NA), linetype = c(0, 1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

#' The following generates a gif animation
#' of the steps of the sampler (might take 10 seconds).
#+ Metropolis (1)
animate(p1 +
          transition_reveal(id=id, along=iter) +
          shadow_trail(0.01))

#' Plot the final frame
p1

#' Take the first 5000 observations after warmup of 50
s <- 5000
warm <- 500
dfs <- data.frame(th1 = tt[(warm+1):s, 1], th2 = tt[(warm+1):s, 2])



#' show 1000 draws after the warm-up
labs2 <- c('Draws', '90% HPD')
ggplot() +
  geom_point(data = dfs[1:1000,],
             aes(th1, th2, color = '1'), alpha = 0.3) +
  stat_ellipse(data = dft, aes(x = X1, y = X2, color = '2'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('steelblue', 'blue'), labels = labs2) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA), linetype = c(0, 1), alpha = c(1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

#' show 4500 draws after the warm-up
labs2 <- c('Draws', '90% HPD')
ggplot() +
  geom_point(data = dfs,
             aes(th1, th2, color = '1'), alpha = 0.3) +
  stat_ellipse(data = dft, aes(x = X1, y = X2, color = '2'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('steelblue', 'blue'), labels = labs2) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA), linetype = c(0, 1), alpha = c(1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())



#example 3


Nsim = 10^4
# X = c(rt(1,1))  # initialize the chain from the stationary
X =12.788  # initialize the chain from the stationary
for (t in 2:Nsim){
  Y=rnorm(1) # candidate normal
  rho=dt(Y,1)*dnorm(X[t-1])/(dt(X[t-1],1)*dnorm(Y))
  X[t]=X[t-1] + (Y - X[t-1])*(runif(1)<rho)
}

Nsim = 10^4
Z =12.788  # initialize the chain from the stationary

for (t in 2:Nsim){
  Y=rt(1,.5) # candidate normal
  rho=dt(Y,1)*dt(Z[t-1],.5)/(dt(Z[t-1],1)*dt(Y,.5))
  Z[t]=Z[t-1] + (Y - Z[t-1])*(runif(1)<rho)
}
par(mfrow=c(3,2),mar=c(2,2,1,1))
plot(5000:5800,X[5000:5800],type="l",lwd=2,xlab="",ylab="")
plot(5000:5800,Z[5000:5800],type="l",lwd=2,xlab="",ylab="")
hist(X,nclass=100,col="grey",main="",xlab="",ylab="",fre=FALSE,xlim=c(-10,10))
curve(dt(x,1),col="sienna",lwd=2,add=TRUE)
hist(Z[abs(Z)<10],nclass=100,col="grey",main="",xlab="",ylab="",fre=FALSE,xlim=c(-10,10))
curve(dt(x,1),col="sienna",lwd=2,add=TRUE)
acf(X,lag.max=50,lwd=2,col="gold3");acf(Z,lag.max=50,lwd=2,col="gold3")

X11()
plot(cumsum(X<3)/(1:Nsim),lwd=2,ty="l",ylim=c(.85,1),xlab="iterations",ylab="")
lines(cumsum(Z<3)/(1:Nsim),lwd=2,col="sienna")

