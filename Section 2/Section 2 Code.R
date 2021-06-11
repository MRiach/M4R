# This gives all the code used in section 2. The code appears in the same order
# in which it appears in the report

#ICE CREAM SIMULATION

n=1000
X<-c()
set.seed(10) # set seed for reproducibility of results
X1 = rbinom(n,1,0.5) # 1 if rainy day
X2 = rnorm(n,1000,100)/(X1+1)+rnorm(n,200,50) # number of people out in the park  

X <- cbind(X1,X2) # design matrix of true model
betas<-c(-100,2) # fix relationships between response and covariates
Y<- 100+X%*%betas + rnorm(n,sd=100) # Number of ice creams sold


Data <- cbind(Y,X1,X2)

Y1 <- subset(Data,X1==1)[,1]
Y2 <- subset(Data,X1==0)[,1]

Z1 <- subset(Data,X1==1)[,3]
Z2 <- subset(Data,X1==0)[,3]

plot(Z1,Y1,type="p",col="orange",xlim=c(400,1600),ylim=c(1000,3000),xlab = "Number of people in the park",
     ylab = "Number of ice creams sold")
lines(Z2,Y2,type ="p",col="blue")






#VOTE SHARE EXAMPLE

library(ggplot2)
library(cowplot)
library(stargazer)

n=1000
X<-c()
set.seed(10) # set seed for reproducibility of results
X1 = rbinom(n,1,0.5) # Incumbency of candidate
X2 = rnorm(n,50,10) # percentage of voters with college degrees
X3 = rexp(n,1/20000) # median income
X4 = rnorm(n,100,10)-X2 # Brexit vote  

X <- cbind(X1,X2,X3,X4) # design matrix of true model
betas<-c(2,-0.2,7/100000,0.4) # fix relationships between response and covariates
truemodelbeta<-c()
omittedmodelbeta<-c()

#Simulation
for (i in 1:10000){
  Y<- 20+X%*%betas + rnorm(n,sd=0.5) # True relationship between Y and covariates
  model1<-lm(Y~X1+X2+X3)
  model2<-lm(Y~X1+X2+X3+X4)
  omittedmodelbeta<-cbind(omittedmodelbeta,summary(model1)$coefficients[1:4, 1]) # extract beta values according to simulated data
  truemodelbeta<-cbind(truemodelbeta,summary(model2)$coefficients[1:4, 1])
  
}

betavalues <- c(2,-0.2,7/100000)
omittedbetamean <- rowMeans(omittedmodelbeta)
truebetamean <- rowMeans(truemodelbeta)

#standard errors:

sd(omittedmodelbeta[1,])/100 #s.e. = 0.000824
sd(omittedmodelbeta[2,])/100 #s.e. = 0.000317
sd(omittedmodelbeta[3,])/100 #s.e. = 1.54e-05
sd(omittedmodelbeta[4,])/100 #s.e. = 7.71e-09

print(omittedbetamean)
print(truebetamean)

plot(X4,Y,xlab = 'Brexit Vote',ylab = 'Vote Share')
plot(X4,X2, xlab = 'Brexit Vote', ylab = 'Percentage with college degrees')

gamma1 <- lm(X4~X2)
gamma1 <- gamma1$coefficients[2] #gamma1 = -0.9839646

#Simulate outcome given vote share and fit the original data to a logistic regression
#using all variables and one variable omitted 

set.seed(10)
Y<- 20+X%*%betas + rnorm(n,sd=0.5)
A<- which(Y>=50) 
B <- which(45<=Y&Y<50)
C <- which(40<=Y&Y<45)
D <- which(35<=Y&Y<40)
Results<- rep(0,n)
Results[A]<-1 # set to 1 if candidate wins, 0 otherwise  
Results[B]<-rbinom(length(B),1,0.9) # candidate has 0.9 prob of winning if vote share is between 45% and 50%
Results[C]<-rbinom(length(C),1,0.75)
Results[D]<-rbinom(length(D),1,0.5)

#Fit logistic regression to data including and excluding Brexit vote 

model3<- glm(Results~X1+X2+X3+X4,family="binomial")

model4<- glm(Results~X1+X2+X3,family="binomial")





#PERFECT SEPARABILITY

library(latex2exp)
library(ggplot2)

set.seed(10)

X1 = runif(50,-1,-0.1)
X2 = runif(50,0.1,1)

X = c(X1,X2)

Y = rep(0,100)

Z <- which(X>0)

Y[Z] <- 1

plot(X,Y,xlim = c(-1,1), xlab = 'Covariate', ylab = 'Outcome')

X3 <- seq(-1,1,0.001)

Y1 <- 1/(1+exp(-10*X3))

lines(X3,Y1,type = 'l', col = "sienna")

X4 <- seq(-1,1,0.001)

Y2 <- 1/(1+exp(-50*X4))

lines(X4,Y2,type = 'l',col = "purple")

X5 <- seq(-1,1,0.001)

Y3 <- 1/(1+exp(-100*X5))

lines(X5,Y3,type = 'l',col = "blue")

legend(x=0.5, y=0.2, legend=c(TeX('$\\beta = 10$'),TeX('$\\beta = 50$'),TeX('$\\beta = 100$')),col=c("blue","red","orange"),cex=0.65,lty=1)






#POWER SIMULATION

Nvalues <- seq(10,500,10)
powers <- rep(0,length(Nvalues))
for (i in 1:length(Nvalues)){
  n = Nvalues[i]
  X1 = rbinom(n,1,0.5) # Incumbency of candidate
  X2 = rnorm(n,50,10) # percentage of voters with college degrees
  X3 = rexp(n,1/20000) # median income
  X4 = rnorm(n,100,10)-X2 # Brexit vote  
  X <- cbind(X1,X2,X3,X4) # design matrix of true model
  betas<-c(2,-0.2,7/100000,0.4)
  
  Y<- 20+X%*%betas + rnorm(n,sd=0.5)
  A<- which(Y>=50) 
  B <- which(45<=Y&Y<50)
  C <- which(40<=Y&Y<45)
  D <- which(35<=Y&Y<40)
  
  Results<- rep(0,n)
  Results[A]<-1 # set to 1 if candidate wins, 0 otherwise  
  Results[B]<-rbinom(length(B),1,0.9) # candidate has 0.9 prob of winning if vote share is between 45% and 50%
  Results[C]<-rbinom(length(C),1,0.75)
  Results[D]<-rbinom(length(D),1,0.5)
  
  model1<- glm(Results~X1+X2+X3+X4,family="binomial")
  
  model2<- glm(Results~X1+X2+X3,family="binomial")
  
  w = sqrt((model2$deviance-model1$deviance)/n)
  lambda = w^2*n
  critchisq <- qchisq(p=.05, df=1, lower.tail=FALSE)
  powers[i] <- 1-pchisq(critchisq,1,ncp = lambda)}

ggplot() + geom_line(aes(x=Nvalues,y=powers),color='blue')+xlab("N") + ylab("Power")
