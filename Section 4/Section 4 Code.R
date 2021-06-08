# This gives all the code used in section 4. The code appears in the same order
# in which it appears in the report

#SIMULATED EXAMPLE

#THIS IS AN IMPLEMENTATION OF Imbens and Kal

#Simulated data

#Margins
X1 <- (seq(20,49.9,0.1)-50)/100
X2 <- (seq(50.1,80,0.1)-50)/100
X <- append(X1,X2)

#Vote share
set.seed(10)
Y1 <- 0.42+0.5*X1 + rnorm(300,sd=0.02)
set.seed(11)
Y2 <- 0.48+0.6*X2 + rnorm(300,sd=0.02)
Y <- append(Y1,Y2)

#Categorical variable determining if won at time t-1
Z1 <- rep(0,300)
Z2 <- rep(1,300)
Z <- append(Z1,Z2)

#Plot to see that RDD is appropriate
plot(X,Y,xlab = 'Hypothetical Vote Margin at t-1', ylab = 'Vote Share at time t')


#Fit weighted linear model as in Eggers and Spirling 

#STEP 1

#sample variance
sx <- var(X)

h1 <- 1.84*sqrt(sx)*(600^(-1/5))

Nh1minus <- length(X[X<0 & X>-h1])
Ybarh1minus <- mean(Y[which(X<0 & X>-h1)])
Yvarh1minus <- var(Y[which(X<0 & X>-h1)])

Nh1plus <- length(X[X>0 & X<h1])
Ybarh1plus <- mean(Y[which(X>0 & X<h1)])
Yvarh1plus <- var(Y[which(X>0 & X<h1)])

f0 <- (Nh1minus+Nh1plus)/(2*600*h1)

#STEP 2

#fit global cubic with jump at threshold

polydeg3fit <- lm(Y~X+I(X^2)+I(X^3)+Z)
gamma4 <- summary(polydeg3fit)$coefficients[4, 1]
mhatthirdderivative <- 6*gamma4

h2plus = 3.56*(Yvarh1plus/(f0*mhatthirdderivative^2))^(1/7)*300^(-1/7)
h2minus = 3.56*(Yvarh1minus/(f0*mhatthirdderivative^2))^(1/7)*300^(-1/7)

#fit quadratics for mhatsecondderivative plus and minus and approximate second derivative
#which is 2 multiplied by gamma3 (in quadratic equation that is inspired from I & K paper)

Xplus <- X[X>0 & X<h2plus]
Xminus <- X[X<0 & X>-h2minus]
Yplus <- Y[which(X>0 & X<h2plus)]
Yminus <- Y[which(X<0 & X>-h2minus)]

polydeg2fitplus <- lm(Yplus~Xplus+I(Xplus^2))
polydeg2fitminus <- lm(Yminus~Xminus+I(Xminus^2))

gamma3plus <- summary(polydeg2fitplus)$coefficients[3,1]
gamma3minus <- summary(polydeg2fitminus)$coefficients[3,1] 

mhatsecondderivativeplus <- 2*gamma3plus
mhatsecondderivativeminus <- 2*gamma3minus

#STEP 3
Nh2minus <- length(X[X<0 & X>-h2minus])
Nh2plus <- length(X[X>0 & X<h2plus])

rplus <- (2160*Yvarh1plus )/(Nh2plus*h2plus^4)
rminus <- (2160*Yvarh1minus )/(Nh2minus*h2minus^4)

#input all variables into formula for optimal bandwidth 
hopt <- 3.4375*((Yvarh1plus+Yvarh1minus)/(f0*((mhatsecondderivativeplus-mhatsecondderivativeminus)^2+rplus+rminus)))^(1/5)*600^(-1/5)

#hopt here is 0.2546747

#with hopt we can now implement a weighted regression with the weights given by the triangular kernel applied to X with hopt as parameter

N <-length(X)
w <- rep(0,N)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

plot(X,w,xlab = 'Hypothetical Vote Margin at t-1', ylab = 'Relative weight')

plot(X,Y,xlab = 'Hypothetical Vote Margin at t-1', ylab = 'Vote Share at time t', main = 'Past Vote Margin vs Current Vote Share with Opt. Bandwidth')
abline(v = hopt, lty =2)
abline(v = -hopt, lty =2)

weightedlinearmodel<-lm(Y~X+Z+X*Z,weights = w)
#Z coefficient here is 0.053524 suggesting 5.4% advantage over running as incumbent vs not running as incumbent  





#CONSERVATIVE PARTY RESULTS FOR 2015, 2017, & 2019

library(parlitools)

#2015

data2015 <- bes_2015

#Only work with English constituencies

data2015 <- subset(data2015,country=='England')

#Drop constituencies which the speaker of the house filled in 2010-19 and
#replace NA with 0 to make calculations work

data2015<-data2015[-c(83,115),]
data2015$ld_10[is.na(data2015$ld_10)] <- 0
data2015$ukip_10[is.na(data2015$ukip_10)] <- 0
data2015$green_10[is.na(data2015$green_10)] <- 0

data2015$Con_Margin <- data2015$con_10-pmax(data2015$lab_10,data2015$ld_10,data2015$ukip_10,data2015$green_10)
plot(data2015$Con_Margin,data2015$con_15)

N <- 531
X <- data2015$Con_Margin
Y <- data2015$con_15
Z <- rep(0,531)
Z[which(X>0)] <-1





X <- X/100
Y <- Y/100



#Fit weighted linear model as in Eggers and Spirling (need to find h_opt first using I & K algorithm)

#STEP 1

#sample variance
sx <- var(X)

#Silverman bandwidth 
h1 <- 1.84*sqrt(sx)*(N^(-1/5))

Nh1minus <- length(X[X<0 & X>-h1])
Ybarh1minus <- mean(Y[which(X<0 & X>-h1)])
Yvarh1minus <- var(Y[which(X<0 & X>-h1)])

Nh1plus <- length(X[X>0 & X<h1])
Ybarh1plus <- mean(Y[which(X>0 & X<h1)])
Yvarh1plus <- var(Y[which(X>0 & X<h1)])

f0 <- (Nh1minus+Nh1plus)/(2*N*h1)

#STEP 2

#fit global cubic with jump at threshold

polydeg3fit <- lm(Y~X+I(X^2)+I(X^3)+Z)
gamma4 <- summary(polydeg3fit)$coefficients[4, 1]
mhatthirdderivative <- 6*gamma4

Nplus <- length(X[X>=0])
Nminus <- length(X[X<0])
h2plus = 3.56*(Yvarh1plus/(f0*mhatthirdderivative^2))^(1/7)*Nplus^(-1/7)
h2minus = 3.56*(Yvarh1minus/(f0*mhatthirdderivative^2))^(1/7)*Nminus^(-1/7)

#fit quadratics for mhatsecondderivative plus and minus and approximate second derivative
#which is 2 multiplied by gamma3 (in quadratic equation that is inspired from I & K paper)

Xplus <- X[X>0 & X<h2plus]
Xminus <- X[X<0 & X>-h2minus]
Yplus <- Y[which(X>0 & X<h2plus)]
Yminus <- Y[which(X<0 & X>-h2minus)]

polydeg2fitplus <- lm(Yplus~Xplus+I(Xplus^2))
polydeg2fitminus <- lm(Yminus~Xminus+I(Xminus^2))


gamma3plus <- summary(polydeg2fitplus)$coefficients[3,1]
gamma3minus <- summary(polydeg2fitminus)$coefficients[3,1] 

mhatsecondderivativeplus <- 2*gamma3plus
mhatsecondderivativeminus <- 2*gamma3minus

#STEP 3
Nh2minus <- length(X[X<0 & X>-h2minus])
Nh2plus <- length(X[X>0 & X<h2plus])

rplus <- (2160*Yvarh1plus )/(Nh2plus*h2plus^4)
rminus <- (2160*Yvarh1minus )/(Nh2minus*h2minus^4)

#input all variables into formula for optimal bandwidth 
hopt <- 3.4375*((Yvarh1plus+Yvarh1minus)/(f0*((mhatsecondderivativeplus-mhatsecondderivativeminus)^2+rplus+rminus)))^(1/5)*600^(-1/5)

#with hopt we can now implement a weighted regression with the weights given by the triangular kernel applied to X with hopt as parameter

w <- rep(0,N)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

weightedlinearmodel<-lm(Y~X+Z+X*Z,weights = w)

#Plot to see that RDD is appropriate
plot(X,Y,xlab = 'Conservative Vote Margin in 2010', ylab = 'Vote Share in 2015')
abline(v = hopt, lty =2)
abline(v = -hopt, lty =2)

#2017

data2017 <- bes_2017

#Only work with English constituencies

data2017 <- subset(data2017,country=='England')

#Drop constituencies which the speaker of the house filled in 2010-19 and
#replace NA with 0 to make calculations work

data2017<-data2017[-c(83,115),]
data2017$ld_15[is.na(data2017$ld_15)] <- 0
data2017$ukip_15[is.na(data2017$ukip_15)] <- 0
data2017$green_15[is.na(data2017$green_15)] <- 0

data2017$Con_Margin <- data2017$con_15-pmax(data2017$lab_15,data2017$ld_15,data2017$ukip_15,data2017$green_15)

N <- 531
X <- data2017$Con_Margin
Y <- data2017$con_17
Z <- rep(0,531)
Z[which(X>0)] <-1



X <- X/100
Y <- Y/100

#Fit weighted linear model as in Eggers and Spirling (need to find h_opt first using I & K algorithm)

#STEP 1

#sample variance
sx <- var(X)

#Silverman bandwidth 
h1 <- 1.84*sqrt(sx)*(N^(-1/5))

Nh1minus <- length(X[X<0 & X>-h1])
Ybarh1minus <- mean(Y[which(X<0 & X>-h1)])
Yvarh1minus <- var(Y[which(X<0 & X>-h1)])

Nh1plus <- length(X[X>0 & X<h1])
Ybarh1plus <- mean(Y[which(X>0 & X<h1)])
Yvarh1plus <- var(Y[which(X>0 & X<h1)])

f0 <- (Nh1minus+Nh1plus)/(2*N*h1)

#STEP 2

#fit global cubic with jump at threshold

polydeg3fit <- lm(Y~X+I(X^2)+I(X^3)+Z)
gamma4 <- summary(polydeg3fit)$coefficients[4, 1]
mhatthirdderivative <- 6*gamma4

Nplus <- length(X[X>=0])
Nminus <- length(X[X<0])
h2plus = 3.56*(Yvarh1plus/(f0*mhatthirdderivative^2))^(1/7)*Nplus^(-1/7)
h2minus = 3.56*(Yvarh1minus/(f0*mhatthirdderivative^2))^(1/7)*Nminus^(-1/7)

#fit quadratics for mhatsecondderivative plus and minus and approximate second derivative
#which is 2 multiplied by gamma3 (in quadratic equation that is inspired from I & K paper)

Xplus <- X[X>0 & X<h2plus]
Xminus <- X[X<0 & X>-h2minus]
Yplus <- Y[which(X>0 & X<h2plus)]
Yminus <- Y[which(X<0 & X>-h2minus)]

polydeg2fitplus <- lm(Yplus~Xplus+I(Xplus^2))
polydeg2fitminus <- lm(Yminus~Xminus+I(Xminus^2))


gamma3plus <- summary(polydeg2fitplus)$coefficients[3,1]
gamma3minus <- summary(polydeg2fitminus)$coefficients[3,1] 

mhatsecondderivativeplus <- 2*gamma3plus
mhatsecondderivativeminus <- 2*gamma3minus

#STEP 3
Nh2minus <- length(X[X<0 & X>-h2minus])
Nh2plus <- length(X[X>0 & X<h2plus])

rplus <- (2160*Yvarh1plus )/(Nh2plus*h2plus^4)
rminus <- (2160*Yvarh1minus )/(Nh2minus*h2minus^4)

#input all variables into formula for optimal bandwidth 
hopt <- 3.4375*((Yvarh1plus+Yvarh1minus)/(f0*((mhatsecondderivativeplus-mhatsecondderivativeminus)^2+rplus+rminus)))^(1/5)*600^(-1/5)

#with hopt we can now implement a weighted regression with the weights given by the triangular kernel applied to X with hopt as parameter

w <- rep(0,N)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

weightedlinearmodel<-lm(Y~X+Z+X*Z,weights = w)

#Plot to see that RDD is appropriate
plot(X,Y,xlab = 'Conservative Vote Margin in 2015', ylab = 'Vote Share in 2017')
abline(v = hopt, lty =2)
abline(v = -hopt, lty =2)

#2019

data2019 <- bes_2019

#Only work with English constituencies

data2019 <- subset(data2019,country=='England')

#Drop constituencies which the speaker of the house filled in 2010-19 and
#replace NA with 0 to make calculations work

data2019<-data2019[-c(79,108),]
data2019$ld_17[is.na(data2019$ld_17)] <- 0
data2019$ukip_17[is.na(data2019$ukip_17)] <- 0
data2019$green_17[is.na(data2019$green_17)] <- 0

data2019$Con_Margin <- data2019$con_17-pmax(data2019$lab_17,data2019$ld_17,data2019$ukip_17,data2019$green_17)

N <- 531
X <- data2019$Con_Margin
Y <- data2019$con_19
Z <- rep(0,531)
Z[which(X>0)] <-1


X <- X/100
Y <- Y/100

#Fit weighted linear model as in Eggers and Spirling (need to find h_opt first using I & K algorithm)

#STEP 1

#sample variance
sx <- var(X)

#Silverman bandwidth 
h1 <- 1.84*sqrt(sx)*(N^(-1/5))

Nh1minus <- length(X[X<0 & X>-h1])
Ybarh1minus <- mean(Y[which(X<0 & X>-h1)])
Yvarh1minus <- var(Y[which(X<0 & X>-h1)])

Nh1plus <- length(X[X>0 & X<h1])
Ybarh1plus <- mean(Y[which(X>0 & X<h1)])
Yvarh1plus <- var(Y[which(X>0 & X<h1)])

f0 <- (Nh1minus+Nh1plus)/(2*N*h1)

#STEP 2

#fit global cubic with jump at threshold

polydeg3fit <- lm(Y~X+I(X^2)+I(X^3)+Z)
gamma4 <- summary(polydeg3fit)$coefficients[4, 1]
mhatthirdderivative <- 6*gamma4

Nplus <- length(X[X>=0])
Nminus <- length(X[X<0])
h2plus = 3.56*(Yvarh1plus/(f0*mhatthirdderivative^2))^(1/7)*Nplus^(-1/7)
h2minus = 3.56*(Yvarh1minus/(f0*mhatthirdderivative^2))^(1/7)*Nminus^(-1/7)

#fit quadratics for mhatsecondderivative plus and minus and approximate second derivative
#which is 2 multiplied by gamma3 (in quadratic equation that is inspired from I & K paper)

Xplus <- X[X>0 & X<h2plus]
Xminus <- X[X<0 & X>-h2minus]
Yplus <- Y[which(X>0 & X<h2plus)]
Yminus <- Y[which(X<0 & X>-h2minus)]

polydeg2fitplus <- lm(Yplus~Xplus+I(Xplus^2))
polydeg2fitminus <- lm(Yminus~Xminus+I(Xminus^2))


gamma3plus <- summary(polydeg2fitplus)$coefficients[3,1]
gamma3minus <- summary(polydeg2fitminus)$coefficients[3,1] 

mhatsecondderivativeplus <- 2*gamma3plus
mhatsecondderivativeminus <- 2*gamma3minus

#STEP 3
Nh2minus <- length(X[X<0 & X>-h2minus])
Nh2plus <- length(X[X>0 & X<h2plus])

rplus <- (2160*Yvarh1plus )/(Nh2plus*h2plus^4)
rminus <- (2160*Yvarh1minus )/(Nh2minus*h2minus^4)

#input all variables into formula for optimal bandwidth 
hopt <- 3.4375*((Yvarh1plus+Yvarh1minus)/(f0*((mhatsecondderivativeplus-mhatsecondderivativeminus)^2+rplus+rminus)))^(1/5)*600^(-1/5)

#with hopt we can now implement a weighted regression with the weights given by the triangular kernel applied to X with hopt as parameter

w <- rep(0,N)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

weightedlinearmodel<-lm(Y~X+Z+X*Z,weights = w)

#Plot to see that RDD is appropriate
plot(X,Y,xlab = 'Conservative Vote Margin in 2017', ylab = 'Vote Share in 2019')
abline(v = hopt, lty =2)
abline(v = -hopt, lty =2)

#CONSERVATIVE CONFIDENCE INTERVALS

library(ggplot2)

# Plot of beta2 coefficients with 95% C.I

dates<-c(2015,2017,2019)
C <- rep(0,3)
U<- rep(0,3)
L <- rep(0,3)

C[1] <-0.025418
C[2] <-0.007850
C[3] <-0.018109


U[1] <-100*(C[1] + 1.96*0.010140)
U[2] <-100*(C[2] + 1.96*0.008940)
U[3] <-100*(C[3] + 1.96*0.008730)

L[1] <-100*(C[1] - 1.96*0.010140)
L[2] <-100*(C[2] - 1.96*0.008940)
L[3] <-100*(C[3] - 1.96*0.008730)


df <- data.frame(dates, C, L, U)

ggplot(df, aes(x = dates, y = 100*C)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) + scale_x_discrete(name ="Year of Election", 
                                                            limits=c(2015,2017,2019))+ylab("Estimated Conservative Incumbency Advantage (percentage)")







#LABOUR PARTY RESULTS FOR 2015, 2017, & 2019



data2015 <- bes_2015

#Only work with English constituencies

data2015 <- subset(data2015,country=='England')

#Drop constituencies which the speaker of the house filled in 2010-19 and
#replace NA with 0 to make calculations work

data2015<-data2015[-c(83,115),]
data2015$ld_10[is.na(data2015$ld_10)] <- 0
data2015$ukip_10[is.na(data2015$ukip_10)] <- 0
data2015$green_10[is.na(data2015$green_10)] <- 0

data2015$Lab_Margin <- data2015$lab_10-pmax(data2015$con_10,data2015$ld_10,data2015$ukip_10,data2015$green_10)
plot(data2015$Lab_Margin,data2015$lab_15)

N <- 531
X <- data2015$Lab_Margin
Y <- data2015$lab_15
Z <- rep(0,531)
Z[which(X>0)] <-1





X <- X/100
Y <- Y/100



#Fit weighted linear model as in Eggers and Spirling (need to find h_opt first using I & K algorithm)

#STEP 1

#sample variance
sx <- var(X)

#Silverman bandwidth 
h1 <- 1.84*sqrt(sx)*(N^(-1/5))

Nh1minus <- length(X[X<0 & X>-h1])
Ybarh1minus <- mean(Y[which(X<0 & X>-h1)])
Yvarh1minus <- var(Y[which(X<0 & X>-h1)])

Nh1plus <- length(X[X>0 & X<h1])
Ybarh1plus <- mean(Y[which(X>0 & X<h1)])
Yvarh1plus <- var(Y[which(X>0 & X<h1)])

f0 <- (Nh1minus+Nh1plus)/(2*N*h1)

#STEP 2

#fit global cubic with jump at threshold

polydeg3fit <- lm(Y~X+I(X^2)+I(X^3)+Z)
gamma4 <- summary(polydeg3fit)$coefficients[4, 1]
mhatthirdderivative <- 6*gamma4

Nplus <- length(X[X>=0])
Nminus <- length(X[X<0])
h2plus = 3.56*(Yvarh1plus/(f0*mhatthirdderivative^2))^(1/7)*Nplus^(-1/7)
h2minus = 3.56*(Yvarh1minus/(f0*mhatthirdderivative^2))^(1/7)*Nminus^(-1/7)

#fit quadratics for mhatsecondderivative plus and minus and approximate second derivative
#which is 2 multiplied by gamma3 (in quadratic equation that is inspired from I & K paper)

Xplus <- X[X>0 & X<h2plus]
Xminus <- X[X<0 & X>-h2minus]
Yplus <- Y[which(X>0 & X<h2plus)]
Yminus <- Y[which(X<0 & X>-h2minus)]

polydeg2fitplus <- lm(Yplus~Xplus+I(Xplus^2))
polydeg2fitminus <- lm(Yminus~Xminus+I(Xminus^2))


gamma3plus <- summary(polydeg2fitplus)$coefficients[3,1]
gamma3minus <- summary(polydeg2fitminus)$coefficients[3,1] 

mhatsecondderivativeplus <- 2*gamma3plus
mhatsecondderivativeminus <- 2*gamma3minus

#STEP 3
Nh2minus <- length(X[X<0 & X>-h2minus])
Nh2plus <- length(X[X>0 & X<h2plus])

rplus <- (2160*Yvarh1plus )/(Nh2plus*h2plus^4)
rminus <- (2160*Yvarh1minus )/(Nh2minus*h2minus^4)

#input all variables into formula for optimal bandwidth 
hopt <- 3.4375*((Yvarh1plus+Yvarh1minus)/(f0*((mhatsecondderivativeplus-mhatsecondderivativeminus)^2+rplus+rminus)))^(1/5)*600^(-1/5)

#with hopt we can now implement a weighted regression with the weights given by the triangular kernel applied to X with hopt as parameter

w <- rep(0,N)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

weightedlinearmodel<-lm(Y~X+Z+X*Z,weights = w)

#Plot to see that RDD is appropriate
plot(X,Y,xlab = 'Labour Vote Margin in 2010', ylab = 'Vote Share in 2015')
abline(v = hopt, lty =2)
abline(v = -hopt, lty =2)

#2017

data2017 <- bes_2017

#Only work with English constituencies

data2017 <- subset(data2017,country=='England')

#Drop constituencies which the speaker of the house filled in 2010-19 and
#replace NA with 0 to make calculations work

data2017<-data2017[-c(83,115),]
data2017$ld_15[is.na(data2017$ld_15)] <- 0
data2017$ukip_15[is.na(data2017$ukip_15)] <- 0
data2017$green_15[is.na(data2017$green_15)] <- 0

data2017$Lab_Margin <- data2017$lab_15-pmax(data2017$con_15,data2017$ld_15,data2017$ukip_15,data2017$green_15)
plot(data2017$Lab_Margin,data2017$lab_17)

N <- 531
X <- data2017$Lab_Margin
Y <- data2017$lab_17
Z <- rep(0,531)
Z[which(X>0)] <-1





X <- X/100
Y <- Y/100



#Fit weighted linear model as in Eggers and Spirling (need to find h_opt first using I & K algorithm)

#STEP 1

#sample variance
sx <- var(X)

#Silverman bandwidth 
h1 <- 1.84*sqrt(sx)*(N^(-1/5))

Nh1minus <- length(X[X<0 & X>-h1])
Ybarh1minus <- mean(Y[which(X<0 & X>-h1)])
Yvarh1minus <- var(Y[which(X<0 & X>-h1)])

Nh1plus <- length(X[X>0 & X<h1])
Ybarh1plus <- mean(Y[which(X>0 & X<h1)])
Yvarh1plus <- var(Y[which(X>0 & X<h1)])

f0 <- (Nh1minus+Nh1plus)/(2*N*h1)

#STEP 2

#fit global cubic with jump at threshold

polydeg3fit <- lm(Y~X+I(X^2)+I(X^3)+Z)
gamma4 <- summary(polydeg3fit)$coefficients[4, 1]
mhatthirdderivative <- 6*gamma4

Nplus <- length(X[X>=0])
Nminus <- length(X[X<0])
h2plus = 3.56*(Yvarh1plus/(f0*mhatthirdderivative^2))^(1/7)*Nplus^(-1/7)
h2minus = 3.56*(Yvarh1minus/(f0*mhatthirdderivative^2))^(1/7)*Nminus^(-1/7)

#fit quadratics for mhatsecondderivative plus and minus and approximate second derivative
#which is 2 multiplied by gamma3 (in quadratic equation that is inspired from I & K paper)

Xplus <- X[X>0 & X<h2plus]
Xminus <- X[X<0 & X>-h2minus]
Yplus <- Y[which(X>0 & X<h2plus)]
Yminus <- Y[which(X<0 & X>-h2minus)]

polydeg2fitplus <- lm(Yplus~Xplus+I(Xplus^2))
polydeg2fitminus <- lm(Yminus~Xminus+I(Xminus^2))


gamma3plus <- summary(polydeg2fitplus)$coefficients[3,1]
gamma3minus <- summary(polydeg2fitminus)$coefficients[3,1] 

mhatsecondderivativeplus <- 2*gamma3plus
mhatsecondderivativeminus <- 2*gamma3minus

#STEP 3
Nh2minus <- length(X[X<0 & X>-h2minus])
Nh2plus <- length(X[X>0 & X<h2plus])

rplus <- (2160*Yvarh1plus )/(Nh2plus*h2plus^4)
rminus <- (2160*Yvarh1minus )/(Nh2minus*h2minus^4)

#input all variables into formula for optimal bandwidth 
hopt <- 3.4375*((Yvarh1plus+Yvarh1minus)/(f0*((mhatsecondderivativeplus-mhatsecondderivativeminus)^2+rplus+rminus)))^(1/5)*600^(-1/5)

#with hopt we can now implement a weighted regression with the weights given by the triangular kernel applied to X with hopt as parameter

w <- rep(0,N)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

weightedlinearmodel<-lm(Y~X+Z+X*Z,weights = w)

#Plot to see that RDD is appropriate
plot(X,Y,xlab = 'Labour Vote Margin in 2015', ylab = 'Vote Share in 2017')
abline(v = hopt, lty =2)
abline(v = -hopt, lty =2)

#2019

data2019 <- bes_2019

#Only work with English constituencies

data2019 <- subset(data2019,country=='England')

#Drop constituencies which the speaker of the house filled in 2010-19 and
#replace NA with 0 to make calculations work

data2019<-data2019[-c(79,108),]
data2019$ld_17[is.na(data2019$ld_17)] <- 0
data2019$ukip_17[is.na(data2019$ukip_17)] <- 0
data2019$green_17[is.na(data2019$green_17)] <- 0

data2019$Lab_Margin <- data2019$lab_17-pmax(data2019$con_17,data2019$ld_17,data2019$ukip_17,data2019$green_17)

N <- 531
X <- data2019$Lab_Margin
Y <- data2019$lab_19
Z <- rep(0,531)
Z[which(X>0)] <-1


X <- X/100
Y <- Y/100

#Fit weighted linear model as in Eggers and Spirling (need to find h_opt first using I & K algorithm)

#STEP 1

#sample variance
sx <- var(X)

#Silverman bandwidth 
h1 <- 1.84*sqrt(sx)*(N^(-1/5))

Nh1minus <- length(X[X<0 & X>-h1])
Ybarh1minus <- mean(Y[which(X<0 & X>-h1)])
Yvarh1minus <- var(Y[which(X<0 & X>-h1)])

Nh1plus <- length(X[X>0 & X<h1])
Ybarh1plus <- mean(Y[which(X>0 & X<h1)])
Yvarh1plus <- var(Y[which(X>0 & X<h1)])

f0 <- (Nh1minus+Nh1plus)/(2*N*h1)

#STEP 2

#fit global cubic with jump at threshold

polydeg3fit <- lm(Y~X+I(X^2)+I(X^3)+Z)
gamma4 <- summary(polydeg3fit)$coefficients[4, 1]
mhatthirdderivative <- 6*gamma4

Nplus <- length(X[X>=0])
Nminus <- length(X[X<0])
h2plus = 3.56*(Yvarh1plus/(f0*mhatthirdderivative^2))^(1/7)*Nplus^(-1/7)
h2minus = 3.56*(Yvarh1minus/(f0*mhatthirdderivative^2))^(1/7)*Nminus^(-1/7)

#fit quadratics for mhatsecondderivative plus and minus and approximate second derivative
#which is 2 multiplied by gamma3 (in quadratic equation that is inspired from I & K paper)

Xplus <- X[X>0 & X<h2plus]
Xminus <- X[X<0 & X>-h2minus]
Yplus <- Y[which(X>0 & X<h2plus)]
Yminus <- Y[which(X<0 & X>-h2minus)]

polydeg2fitplus <- lm(Yplus~Xplus+I(Xplus^2))
polydeg2fitminus <- lm(Yminus~Xminus+I(Xminus^2))


gamma3plus <- summary(polydeg2fitplus)$coefficients[3,1]
gamma3minus <- summary(polydeg2fitminus)$coefficients[3,1] 

mhatsecondderivativeplus <- 2*gamma3plus
mhatsecondderivativeminus <- 2*gamma3minus

#STEP 3
Nh2minus <- length(X[X<0 & X>-h2minus])
Nh2plus <- length(X[X>0 & X<h2plus])

rplus <- (2160*Yvarh1plus )/(Nh2plus*h2plus^4)
rminus <- (2160*Yvarh1minus )/(Nh2minus*h2minus^4)

#input all variables into formula for optimal bandwidth 
hopt <- 3.4375*((Yvarh1plus+Yvarh1minus)/(f0*((mhatsecondderivativeplus-mhatsecondderivativeminus)^2+rplus+rminus)))^(1/5)*600^(-1/5)

#with hopt we can now implement a weighted regression with the weights given by the triangular kernel applied to X with hopt as parameter

w <- rep(0,N)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

weightedlinearmodel<-lm(Y~X+Z+X*Z,weights = w)

#Plot to see that RDD is appropriate
plot(X,Y,xlab = 'Labour Vote Margin in 2017', ylab = 'Vote Share in 2019')
abline(v = hopt, lty =2)
abline(v = -hopt, lty =2)


#LABOUR CONFIDENCE INTERVALS

# Plot of beta2 coefficients with 95% C.I

dates<-c(2015,2017,2019)
C <- rep(0,3)
U<- rep(0,3)
L <- rep(0,3)

C[1] <-0.003644
C[2] <-0.020370
C[3] <--0.013281


U[1] <-100*(C[1] + 1.96*0.007807)
U[2] <-100*(C[2] + 1.96*0.009720)
U[3] <-100*(C[3] + 1.96*0.008136)

L[1] <-100*(C[1] - 1.96*0.007807)
L[2] <-100*(C[2] - 1.96*0.009720)
L[3] <-100*(C[3] - 1.96*0.008136)


df <- data.frame(dates, C, L, U)

ggplot(df, aes(x = dates, y = 100*C)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) + scale_x_discrete(name ="Year of Election", 
                                                            limits=c(2015,2017,2019))+ylab("Estimated Labour Incumbency Advantage (percentage)")
