# This gives all the code used in section 3. The code appears in the same order
# in which it appears in the report

#SOPHOREMORE SURGE SIMULATION

library(rgl)
library(latex2exp)

set.seed(102)

X <- runif(1000000,0.25,0.75)

set.seed(103)

Y <- runif(1000000,0.25,0.75)

Z <- which(X>0.5)

Incumb <- rep(0,1000000)

#Identify votes that exceed 50% so that we can add in incumbency effect
Incumb[Z] <- 1

Y[Z]<- Y[Z] + 0.05

Vote_Data <- cbind(X,Y,Incumb)

Vote_Data <- subset(Vote_Data,Incumb==1)

mean(Vote_Data[,2]-Vote_Data[,1]) 

hist(Vote_Data[,2]-Vote_Data[,1]-0.05, xlab = 'Difference between vote share increase and incumbency advantage', main = 'Histogram to show bias of sophomore surge')

#mean is -0.125

#variance is 0.0261


# SIMULATION ACROSS DIFFERENT PARAMETERS

alpha <- seq(0.2,0.45,0.01)
beta <- seq(0.55,0.8,0.01)

Biases <- matrix(0L,length(alpha),length(beta))

for (i in 1:length(alpha)) {
  for (j in 1:length(beta)){
    
    X <- runif(10000,alpha[i],beta[j])
    
    Y <- runif(10000,alpha[i],beta[j])
    
    Z <- which(X>0.5)
    
    Incumb <- rep(0,10000)
    
    #Identify votes that exceed 50% so that we can add in incumbency effect
    Incumb[Z] <- 1
    
    Y[Z]<- Y[Z] + 0.05
    
    Vote_Data <- cbind(X,Y,Incumb)
    
    Vote_Data <- subset(Vote_Data,Incumb==1)
    
    Biases[i,j] <- mean(Vote_Data[,2]-Vote_Data[,1]) -0.05
    
  }
}

zlim <- range(Biases)
zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[ Biases - zlim[1] + 1 ] # assign colors to heights for each point

open3d()
surface3d(alpha,beta,Biases,color=col)
axes3d()
#title3d(xlab= TeX('$\\alpha$'), ylab=TeX('$\\beta$'), zlab='Bias')

Z <- matrix(1L,length(alpha),length(beta))*0.05

surface3d(alpha,beta,Z,color='red')





#UNBIASED REGRESSION BASED METHOD

library(ggplot2)
library(stargazer)
#stargazer(model2001,title="Results", align=TRUE)

data2001<-read.csv(file = '2001.csv')
data2005<-read.csv(file = '2005.csv')
data2010<-read.csv(file = '2010.csv')
data2015<-read.csv(file = '2015.csv')
data2017<-read.csv(file = '2017.csv')
data2019<-read.csv(file = '2019.csv')

model2001<-lm(data2001$ï..v_.t.~data2001$v_.t.1.+data2001$P.t.+data2001$I.t)
model2005<-lm(data2005$ï..v_.t.~data2005$v_.t.1.+data2005$P.t.+data2005$I.t)
model2010<-lm(data2010$ï..v_.t.~data2010$v_.t.1.+data2010$P.t.+data2010$I.t)
model2015<-lm(data2015$ï..v_.t.~data2015$v_.t.1.+data2015$P.t.+data2015$I.t)
model2017<-lm(data2017$ï..v_.t.~data2017$v_.t.1.+data2017$P.t.+data2017$I.t)
model2019<-lm(data2019$ï..v_.t.~data2019$v_.t.1.+data2019$P.t.+data2019$I.t)

summary2001<-summary(model2001)
summary2005<-summary(model2005)
summary2010<-summary(model2010)
summary2015<-summary(model2015)
summary2017<-summary(model2017)
summary2019<-summary(model2019)

dates<-c(2001,2005,2010,2015,2017,2019)
C <- rep(0,6)
U<- rep(0,6)
L <- rep(0,6)

C[1] <-summary2001$coefficients[4,1]
C[2] <-summary2005$coefficients[4,1]
C[3] <-summary2010$coefficients[4,1]
C[4] <-summary2015$coefficients[4,1]
C[5] <-summary2017$coefficients[4,1]
C[6] <-summary2019$coefficients[4,1]

U[1] <-summary2001$coefficients[4,1] + 1.96*summary2001$coefficients[4,2]
U[2] <-summary2005$coefficients[4,1] + 1.96*summary2005$coefficients[4,2]
U[3] <-summary2010$coefficients[4,1] + 1.96*summary2010$coefficients[4,2]
U[4] <-summary2015$coefficients[4,1] + 1.96*summary2015$coefficients[4,2]
U[5] <-summary2017$coefficients[4,1] + 1.96*summary2017$coefficients[4,2]
U[6] <-summary2019$coefficients[4,1] + 1.96*summary2019$coefficients[4,2]

L[1] <-summary2001$coefficients[4,1] - 1.96*summary2001$coefficients[4,2]
L[2] <-summary2005$coefficients[4,1] - 1.96*summary2005$coefficients[4,2]
L[3] <-summary2010$coefficients[4,1] - 1.96*summary2010$coefficients[4,2]
L[4] <-summary2015$coefficients[4,1] - 1.96*summary2015$coefficients[4,2]
L[5] <-summary2017$coefficients[4,1] - 1.96*summary2017$coefficients[4,2]
L[6] <-summary2019$coefficients[4,1] - 1.96*summary2019$coefficients[4,2]

df <- data.frame(dates, C, L, U)

ggplot(df, aes(x = dates, y = C)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) + scale_x_discrete(name ="Year of Election", 
                                                            limits=c(2001,2005,2010,2015,2017,2019))+ylab("Estimated Incumbency Advantage (percentage)")


#Average Conservative vote share in 179 constituencies
X<- c(32.17,32.92,33.33,37.84,39.13,44.23,48.03)

#Average Conservative vote share in England
Y<- c(33.7,35.2,35.7,39.5,40.9,45.4,47.2)

#plot to see if there is huge selection bias
plot(X,Y,xlab = 'Average Con vote share in chosen seats', ylab = 'Con vote share in England',ylim=c(30,50),xlim=c(30,50))
abline(coef = c(0,1))

ggplot(data2001, aes(I.t.)) + geom_bar() + labs(x = "Incumbency Status") + labs(y = "Quantity")
ggplot(data2005, aes(I.t.)) + geom_bar() + labs(x = "Incumbency Status") + labs(y = "Quantity")
ggplot(data2010, aes(I.t.)) + geom_bar() + labs(x = "Incumbency Status") + labs(y = "Quantity")
ggplot(data2015, aes(I.t.)) + geom_bar() + labs(x = "Incumbency Status") + labs(y = "Quantity")
ggplot(data2017, aes(I.t.)) + geom_bar() + labs(x = "Incumbency Status") + labs(y = "Quantity")
ggplot(data2019, aes(I.t.)) + geom_bar() + labs(x = "Incumbency Status") + labs(y = "Quantity")