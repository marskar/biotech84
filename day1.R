ls("package:datasets")

head(Theoph)
summary(Theoph)
names(Theoph)
str(Theoph)
install.packages("lattice")
library(lattice)
str(Theoph)

#### Output the first few rows of a dataframe-----
head(Theoph)

#### Output the last few rows of a dataframe------
tail(Theoph)

#### Check the number of rows-------
nrow(Theoph)

#### Check the number of columns------
ncol(Theoph)

##### Check column names for a dataframe-------
names(Theoph)

#### To look at same place in the middle of dataframe, use [row,column]-------
Theoph[51,]
Theoph[,4]

#### To look at a particular element in dataset------
Theoph[51,2]

#### attach.dataframe, but be careful to detach before using another dataset-----
attach(Theoph)
detach(Theoph)

#### to look for a specific column in a dataframe without attaching dataframe----
Theoph$Time

#### How many Time measurements are less than 3 hrs post dose?-----
Theoph[Theoph$Time<3,]

### How many rows?--
nrow(Theoph[Theoph$Time<3,])

#### How many Time measurements are at pre-dose (Time zero)?-----
Theoph[Theoph$Time==0,]
nrow(Theoph[Theoph$Time==0,])

### Instead of all data, ask how many rows of data have conc data above 0 at time zero----
Theoph[Theoph$Time==0 & Theoph$conc >0,]
nrow(Theoph[Theoph$Time==0 & Theoph$conc >0,])

#### Try to get weights of individuals between 40 and 60-----
names(Theoph)
Theoph[Theoph$Wt > 40 & Theoph$Wt < 60,]

#### Calculating some summary statistics-----
mean(Theoph$Wt)
median(Theoph$Wt)
sd(Theoph$Wt)
var(Theoph$Wt)

##### Mean of all column variables (only for continuous variables)-------
lapply(Theoph,mean)


##### How to get mean of a variable (like conc) by time (grouping variable)-------
aggregate(Theoph$conc ~ Theoph$Time, data = Theoph, FUN = mean)
aggregate(Theoph$conc, by= list(Theoph$Time,Theoph$Subject), FUN=mean)

##### Basic plotting function------
plot(Theoph$Time, Theoph$conc, type="p")
plot(Theoph$Time, Theoph$conc, type="l")
plot(Theoph$Time, Theoph$conc, type="b")

Theoph[Theoph$Subject==1,]
#### Plot mean conc by time-----
plot(aggregate(Theoph$conc ~ Theoph$Time, data = Theoph, FUN = mean))

### Need to make a new dataframe ###
meanTheoph<-aggregate(Theoph$conc, by= list(Theoph$Time,Theoph$Subject), FUN=mean)
names(meanTheoph)<-c("Time","Subject","MeanTheophConc")

### Plot mean Theoph by Time again...###
plot(MeanTheophConc ~ Time, data=meanTheoph)

### Change size of data points and axes (cex default size =1)###
plot(Theoph$Time, Theoph$conc, type="p", cex=1.5,
     cex.lab=1.5, cex.axis=1.5)

### Add color to data points ###
plot(Theoph$Time, Theoph$conc, type="p", cex=1.5,
     cex.lab=1.5, cex.axis=1.5, col="blue")

### Add axis labels ###
plot(Theoph$Time, Theoph$conc, type="p", cex=1.5,
     cex.lab=1.5, cex.axis=1.5, col="blue",
     xlab="Time (hrs)", ylab="Theophylline Conc (ng/mL)", main="Theophylline Conc by Time")

###  1 row, 2 columns of plots ###
par(mfrow=c(1,2))
hist(Theoph$Wt, cex=1.5, xlab="Subject Body Weight (kg)", cex.lab=1.5, cex.axis=1.5, col="blue")
hist(Theoph$Dose, cex=1.5, xlab="Theophylline Dose (mg)", cex.lab=1.5, cex.axis=1.5, col="red")



#################################################################################
################# Create a unique, simulated dataframe #########################

##### Simulate gender---------
n<-50
is.female<-rbinom(n, 1, 0.5)
table(is.female)
n_fem<-sum(is.female)
n_mal<-n-n_fem

##### Given gender simulate age from a uniform distribution-----
#### Age and gender not dependent------
Age_m<-runif(n_mal,20,60)
Age_f<-runif(n_fem,20,60)
Age<-ifelse(is.female==1,Age_f,Age_m)

###### Simulate Weight given age and gender----
###### Conditional simulation of weight -----
wt_fem<-65+0.02*(Age_f-40)+rnorm(n_fem,0,sd=2.5)
wt_m<-75+0.02*(Age_m-40)+rnorm(n_mal,0,sd=2.5)
WT<-ifelse(is.female==1,wt_fem, wt_m)

#### Create data.frame-------
simdata<-data.frame(is.female=is.female, WT=WT, Age=Age)
head(simdata)
