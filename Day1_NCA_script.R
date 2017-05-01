### read in data ###
data1<-read.csv("finalPKdata.csv", header=TRUE)

#### activate required libraries ###
install.packages("lattice")
library(lattice)
install.packages("PKNCA")
library(PKNCA)
install.packages("ggplot2")
library(ggplot2)
install.packages("knitr")
library(knitr)

#################  Plot trial demographic data ##################################

#### histogram of age --------
hist(data1$Age)
hist(data1$Age,cex=2.0, cex.lab=1.5, cex.axis=1.5,xlab="Age (yrs)", 
     xlim=c(30,65),main="Age of Patients")

#### histogram of body weight --------
hist(data1$Weight,cex=2.0, cex.lab=1.5, cex.axis=1.5,xlab="Weight (kgs)", 
     xlim=c(50,100),main="Body Weight of Patients")

#### histogram of Serum Creatinine --------
hist(data1$SCreatinine,cex=2.0, cex.lab=1.5, cex.axis=1.5,xlab="SerCreatinine (units))", 
     xlim=c(0,2),main="Serum Creatinine of Patients")

#### histogram of Serum ALT --------
hist(data1$SerumALT,cex=2.0, cex.lab=1.5, cex.axis=1.5,xlab="Serum ALT (units))", 
     xlim=c(10,25),main="Serum ALT of Patients")


############################## PK Analysis #####################################33
### basic plot ###
plot(data1$Conc~data1$Time, ylab="Concentration (ng/mL)", 
     xlab="Time (hr)")

#### group data and plot ####
install.packages("nlme")
library(nlme)
data1.2<-groupedData(Conc~Time|ID, data=data1, labels = list(x="Time(hr)", 
                y="Conc(ng/mL)"))
plot(data1.2, aspect=1/1, ylab="Concentration (ng/mL)", xlab="Time (hr)")
data1.2$logConc<-log(data1.2$Conc)

data1.2log<-groupedData(logConc~Time|ID, data=data1.2, labels = list(x="Time(hr)", 
      y="Log Conc(ng/mL)"))
plot(data1.2log, aspect=1/1, ylab="Log Concentration (ng/mL)", xlab="Time (hr)")


#### PK Profiles by Sex ###
data1.3<-groupedData(Conc~Time|Gender, data=data1, labels=list(x="Time", 
            y="Concentration"))
plot(data1.3, aspect=1/1, ylab="Concentration (ng/mL)", xlab="Time (hr)")

### log transform dependent variable ##
data1$logConc<-log(data1$Conc)
data1.3log<-groupedData(logConc~Time|Gender, data=data1, labels=list(x="Time", 
        y="Log Concentration"))
plot(data1.3log, aspect=1/1, ylab="Log Concentration (ng/mL)", 
     xlab="Time (hr)")

### Perform NCA ####
library(PKNCA)
library(knitr)

## By default it is groupedData; convert it to a data frame for use--
### maps conc to any variable or grouping factor ##
my.conc <- PKNCAconc(as.data.frame(data1), Conc~Time|ID)

## Dosing data needs to only have one row per dose, so subset for that first##
d.dose <- unique(data1[data1$Time == 0,
   c("ID","Time","Dose", "Conc", "Age","Weight","SCreatinine","SerumALT",
     "Gender","Race")])

knitr::kable(d.dose,
             caption="Dosing data extracted from data set")

### by dose
my.dose <- PKNCAdose(d.dose, Dose~Time|ID)
my.dose

#### combine dose and conc data ##
my.data.automatic <- PKNCAdata(my.conc, my.dose)
summary(my.data.automatic)
knitr::kable(PKNCA.options("single.dose.aucs"))
knitr::kable(my.data.automatic$intervals)

### compute parameters ###
my.results.automatic <- pk.nca(my.data.automatic)
summary(my.results.automatic)

### specify start/stop times ####
my.intervals <- data.frame(start=0,
                           end=Inf,
                           cmax=TRUE,
                           tmax=TRUE,
                           aucinf.obs=TRUE,
                           auclast=FALSE,
                           cl.obs=TRUE,
                           vz.obs=TRUE,
                           half.life=TRUE)

my.data.manual <- PKNCAdata(my.conc, my.dose,intervals=my.intervals)

knitr::kable(my.data.manual$interval)
summary(my.data.manual)

my.results.manual <- pk.nca(my.data.manual)
summary(my.results.manual)
my.results.manual$result

write.csv(my.results.manual$result, file="PKNCA data.csv")

knitr::kable(summary(my.results.manual))


### make a data.frame for PK manual results###
PKresults<-my.results.manual$result
  #### drop first two columns in PK results ###
PKresults1<-PKresults[c(-1, -2)]

#### subset out only t=0hr rows in main data.frame ###
data0<-data1[data1$Time==0,]
### drop the log conc column ###
data0.1<-data0[c(-11)]

install.packages("reshape2")
library(reshape2)
library(nlme)
PKresults2<-t(PKresults1)
### melt PK data ###
mPKresults<-melt(PKresults1, id=c("ID", "PPTESTCD"))
?cast
### cast the melted data ##
   ### cast(data, formula, function) ###
PKresults3<-dcast(mPKresults, ID~variable+PPTESTCD)
write.csv(PKresults3, file="PKNCA results.csv")

#### Cbind pk results to subsetted data.frame ###
alldata<-cbind2(PKresults3, data0.1)
head(alldata)
write.csv(alldata, file="FinalPKNCA1.csv")

########## Subset dataset for PK grouping ####################
#############################  PK by SEX #####################

my.conc.sex <- PKNCAconc(as.data.frame(data1), 
        Conc~Time|Gender+ID)
my.dose.sex <- PKNCAdose(d.dose, Dose~Time|Gender+ID)
my.data.auto.sex <- PKNCAdata(my.conc.sex, my.dose.sex)
my.results.auto.sex <- pk.nca(my.data.auto.sex)
summary(my.results.auto.sex)

my.data.manual.sex <- PKNCAdata(my.conc.sex, my.dose.sex,
                            intervals=my.intervals)
my.results.manual.sex <- pk.nca(my.data.manual.sex)
summary(my.results.manual.sex)


#############################  PK by Race ###################################3

my.conc.race <- PKNCAconc(as.data.frame(data1), Conc~Time|Race+ID)
my.dose.race <- PKNCAdose(d.dose, Dose~Time|Race+ID)
my.data.auto.race <- PKNCAdata(my.conc.race, my.dose.race)
my.results.auto.race <- pk.nca(my.data.auto.race)
summary(my.results.auto.race)

my.data.manual.race <- PKNCAdata(my.conc.race, my.dose.race,
                                intervals=my.intervals)
my.results.manual.race <- pk.nca(my.data.manual.race)
summary(my.results.manual.race)


#################################################################################
#################################################################################
############  PK Correlations with Patient Demographics #########################

### scatter plot matrix for broad overview ###
library(lattice)
# Basic Scatterplot Matrix for exposure vs continuous variables only---
pairs(~PPORRES_cmax+PPORRES_aucinf.obs +Age + Weight +
        SCreatinine+SerumALT,data=alldata1, 
      main="Scatterplot Matrix for exposure vs continuous variables only")
write.csv(alldata, file="alldata1")
alldata1<-read.csv("alldata1", header=TRUE)

pairs(~PPORRES_cmax+PPORRES_aucinf.obs+Race+Gender,data=alldata1, 
      main="Scatterplot Matrix for exposure vs ordinal variables only")

pairs(~PPORRES_cl.obs+Age+Weight+SCreatinine+SerumALT,data=alldata1, 
      main="Scatterplot Matrix for clearance vs continuous variables only")


### CL by WT ####
plot(alldata$Weight, alldata$PPORRES_cl.obs)
plot.clwt<- ggplot() + geom_point(data = alldata1, 
  aes(x=Weight, y = PPORRES_cl.obs), size=2.5, colour='blue') +
  xlab('Weight (kg)') +
  ylab('Clearance(L/hr)')
plot.clwt+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Drug Clearance by Body Weight")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

### CL by Age ###
plot(alldata$Age, alldata$PPORRES_cl.obs)
plot.clage<- ggplot() + geom_point(data = alldata, aes(x=Age, y = PPORRES_cl), size=2.5, colour='blue') +
  xlab('Age(yr)') +
  ylab('Clearance(L/hr)')
plot.clage+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Drug Clearance by Age")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))


par(mfrow=c(1,2))
plot(alldata$Weight, alldata$PPORRES_cl.obs)
plot(alldata$Age, alldata$PPORRES_cl.obs)


##### CL by Gender ####
plot(alldata1$Gender, alldata1$PPORRES_cl.obs)
plot.clsex<- ggplot() + geom_boxplot(data = alldata, aes(x=Gender, y = PPORRES_cl), size=1.5, colour='blue') +
  xlab('Sex)') +
  ylab('Clearance(L/hr)')
plot.clsex+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Drug Clearance by Sex")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

##### CL by Race ####
par(mfrow=c(1,1))
plot(alldata1$Race, alldata1$PPORRES_cl.obs)
plot.clrace<- ggplot() + geom_boxplot(data = alldata, aes(x=Race, y = PPORRES_cl), size=1.5, colour='blue') +
  xlab('Race') +
  ylab('Clearance(L/hr)')
plot.clrace+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Drug Clearance by Race")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

par(mfrow=c(1,2))
plot(alldata$Gender, alldata$PPORRES_cl.obs)
plot(alldata$Race, alldata$PPORRES_cl.obs)


par(mfrow=c(1,1))
### Cmax by WT ####
plot(alldata$Weight, alldata$PPORRES_cmax)
plot.cmaxwt<- ggplot() + geom_point(data = alldata, aes(x=Weight, y = PPORRES_cmax), size=2.5, colour='blue') +
  xlab('Weight (kg)') +
  ylab('Cmax (ng/mL))')
plot.cmaxwt+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Cmax by Body Weight")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

### Cmax by Age ###
plot(alldata$Age, alldata$PPORRES_cmax)
plot.cmaxage<- ggplot() + geom_point(data = alldata, aes(x=Age, y = PPORRES_cmax), size=2.5, colour='blue') +
  xlab('Age(yr)') +
  ylab('Cmax (ng/mL)')
plot.cmaxage+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Cmax by Age")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))


par(mfrow=c(1,2))
plot(alldata$Weight, alldata$PPORRES_cmax)
plot(alldata$Age, alldata$PPORRES_cmax)


##### Cmax by Gender ####
plot(alldata$Gender, alldata$PPORRES_cmax)
plot.clsex<- ggplot() + geom_boxplot(data = alldata, aes(x=Gender, y = PPORRES_cmax), size=1.5, colour='blue') +
  xlab('Sex)') +
  ylab('Cmax (ng/mL)')
plot.clsex+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Cmax by Sex")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

##### Cmax by Race ####
par(mfrow=c(1,1))
plot(alldata$Race, alldata$PPORRES_cmax)
plot.clrace<- ggplot() + geom_boxplot(data = alldata, aes(x=Race, y = PPORRES_cmax), size=1.5, colour='blue') +
  xlab('Race') +
  ylab('Cmax (ng/mL))')
plot.clrace+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("Cmax by Race")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

par(mfrow=c(1,2))
plot(alldata$Gender, alldata$PPORRES_cmax)
plot(alldata$Race, alldata$PPORRES_cmax)


par(mfrow=c(1,1))
### AUCinf by WT ####
plot(alldata$Weight, alldata$PPORRES_aucinf)
plot.aucinfwt<- ggplot() + geom_point(data = alldata, aes(x=Weight, y = PPORRES_aucinf), size=2.5, colour='blue') +
  xlab('Weight (kg)') +
  ylab('AUCinf (hr*ng/mL))')
plot.aucinfwt+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("AUCinf by Body Weight")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

### AUCinf by Age ###
plot(alldata$Age, alldata$PPORRES_aucinf)
plot.aucinfage<- ggplot() + geom_point(data = alldata, aes(x=Age, y = PPORRES_aucinf), size=2.5, colour='blue') +
  xlab('Age(yr)') +
  ylab('AUCinf (hr*ng/mL)')
plot.aucinfage+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("AUCinf by Age")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))


par(mfrow=c(1,2))
plot(alldata$Weight, alldata$PPORRES_aucinf)
plot(alldata$Age, alldata$PPORRES_aucinf)


##### AUCinf by Gender ####
plot(alldata$Gender, alldata$PPORRES_aucinf)
plot.aucinfsex<- ggplot() + geom_boxplot(data = alldata, aes(x=Gender, y = PPORRES_aucinf), size=1.5, colour='blue') +
  xlab('Sex)') +
  ylab('AUCinf (hr*ng/mL)')
plot.aucinfsex+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("AUCinf by Sex")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

##### AUCinf by Race ####
par(mfrow=c(1,1))
plot(alldata$Race, alldata$PPORRES_aucinf)
plot.aucinfrace<- ggplot() + geom_boxplot(data = alldata, aes(x=Race, y = PPORRES_aucinf), size=1.5, colour='blue') +
  xlab('Race') +
  ylab('AUCinf (hr*ng/mL)')
plot.aucinfrace+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 15))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 15))+
  ggtitle("AUCinf by Race")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))

par(mfrow=c(1,2))
plot(alldata$Gender, alldata$PPORRES_aucinf)
plot(alldata$Race, alldata$PPORRES_aucinf)

