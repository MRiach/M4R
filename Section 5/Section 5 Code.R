# This gives all the code used in section 5. The code appears in the same order
# in which it appears in the report

#Fit model which uses both types of incumbency to eliminate any possible OVB  
#Do this model for both Labour and Conservative
library(corrplot)
library(ggplot2)

#Conservatives


data2015con<-read.csv(file = '2015con.csv')
data2017con<-read.csv(file = '2017con.csv')
data2019con<-read.csv(file = '2019con.csv')

#2015

hopt <- 0.249698

X <- data2015con$Con_Margin
Y <- data2015con$con_15
Z <- rep(0,531)
Z[which(X>0)] <-1
I <- data2015con$Incumbency_Status
X <- X/100
Y <- Y/100

w <- rep(0,531)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

con2015model<-lm(Y~X+Z+X*Z+I,weights = w)

#Show correlation between variables in this instance

corrdata <- cbind(Y,X,Z,I)
colnames(corrdata)<-c('2015 Vote Share','2010 Vote Margin', 'Party Incumbency', 'Individual Incumbency')

#2017

hopt <- 0.4210012


X <- data2017con$Con_Margin
Y <- data2017con$con_17
Z <- rep(0,531)
Z[which(X>0)] <-1
I <- data2017con$Incumbency_Status
X <- X/100
Y <- Y/100

w <- rep(0,531)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

con2017model<-lm(Y~X+Z+X*Z+I,weights = w)

#2019

hopt <- 0.4355345


X <- data2019con$Con_Margin
Y <- data2019con$con_19
Z <- rep(0,531)
Z[which(X>0)] <-1
I <- data2019con$Incumbency_Status
X <- X/100
Y <- Y/100

w <- rep(0,531)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

con2019model<-lm(Y~X+Z+X*Z+I,weights = w)

#Labour


data2015lab<-read.csv(file = '2015lab.csv')
data2017lab<-read.csv(file = '2017lab.csv')
data2019lab<-read.csv(file = '2019lab.csv')

#2015

hopt <- 0.4918017

X <- data2015lab$Lab_Margin
Y <- data2015lab$lab_15
Z <- rep(0,531)
Z[which(X>0)] <-1
I <- data2015lab$Incumbency_Status
X <- X/100
Y <- Y/100

w <- rep(0,531)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

lab2015model<-lm(Y~X+Z+X*Z+I,weights = w)

#2017

hopt <- 0.3875571

X <- data2017lab$Lab_Margin
Y <- data2017lab$lab_17
Z <- rep(0,531)
Z[which(X>0)] <-1
I <- data2017lab$Incumbency_Status
X <- X/100
Y <- Y/100

w <- rep(0,531)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

lab2017model<-lm(Y~X+Z+X*Z+I,weights = w)

#2019

hopt <- 0.3264774

X <- data2019lab$Lab_Margin
Y <- data2019lab$lab_19
Z <- rep(0,531)
Z[which(X>0)] <-1
I <- data2019lab$Incumbency_Status
X <- X/100
Y <- Y/100

w <- rep(0,531)
NonZeroPoints <- which(abs(X)<=hopt)
w[NonZeroPoints] <- 1-abs(X[NonZeroPoints])/hopt

lab2019model<-lm(Y~X+Z+X*Z+I,weights = w)

#Plots of confidence intervals for both personal and party based incumbency

#Conservative party based incumbency advantage

dates<-c(2015,2017,2019)
C <- rep(0,3)
U<- rep(0,3)
L <- rep(0,3)

C[1] <--0.024397
C[2] <--0.02926
C[3] <--0.038103 


U[1] <-100*(C[1] + 1.96*0.019125)
U[2] <-100*(C[2] + 1.96*0.02201)
U[3] <-100*(C[3] + 1.96*0.013931)

L[1] <-100*(C[1] - 1.96*0.019125)
L[2] <-100*(C[2] - 1.96*0.02201)
L[3] <-100*(C[3] - 1.96*0.013931)


df <- data.frame(dates, C, L, U)

ggplot(df, aes(x = dates, y = 100*C)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) + scale_x_discrete(name ="Year of Election", 
                                                            limits=c(2015,2017,2019))+ylab("Conservative party based incumbency advantage (percentage)")


#Labour party based incumbency advantage

dates<-c(2015,2017,2019)
C <- rep(0,3)
U<- rep(0,3)
L <- rep(0,3)

C[1] <--0.035134
C[2] <-0.022864
C[3] <--0.064459


U[1] <-100*(C[1] + 1.96*0.014830)
U[2] <-100*(C[2] + 1.96*0.024301)
U[3] <-100*(C[3] + 1.96*0.012751)

L[1] <-100*(C[1] - 1.96*0.014830)
L[2] <-100*(C[2] - 1.96*0.024301)
L[3] <-100*(C[3] - 1.96*0.012751)


df <- data.frame(dates, C, L, U)

ggplot(df, aes(x = dates, y = 100*C)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) + scale_x_discrete(name ="Year of Election", 
                                                            limits=c(2015,2017,2019))+ylab("Labour party based incumbency advantage (percentage)")

#Individual incumbency advantage 

dates<-c(2015,2017,2019)
C <- rep(0,3)
U<- rep(0,3)
L <- rep(0,3)

C[1] <-0.028485
C[2] <-0.01980
C[3] <-0.034883


U[1] <-100*(C[1] + 1.96*0.009315)
U[2] <-100*(C[2] + 1.96*0.01074)
U[3] <-100*(C[3] + 1.96*0.006847)

L[1] <-100*(C[1] - 1.96*0.009315)
L[2] <-100*(C[2] - 1.96*0.01074)
L[3] <-100*(C[3] - 1.96*0.006847)


df <- data.frame(dates, C, L, U)

ggplot(df, aes(x = dates, y = 100*C)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) + scale_x_discrete(name ="Year of Election", 
                                                            limits=c(2015,2017,2019))+ylab("Individual incumbency advantage (percentage)")