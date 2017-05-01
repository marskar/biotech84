### read in library data--
library(survival)
data(ovarian)
head(ovarian)
#### summarize data--
ovarian<-within(ovarian,{
  rx<-factor(rx)
  ecog.ps<-factor(ecog.ps)
  resid.ds<-factor(resid.ds)
  fustat<-factor(fustat)})
summary(ovarian)

###### Fit Kaplan-Meier ------ 
library(survival) 
km.fit<-survfit( Surv(futime, fustat)~1, data=ovarian) 
km.fit
plot(km.fit, cex=1.8, cex.lab=1.5, cex.axis=1.5, xlab="Time, days", 
     ylab="Probability of survival", lwd=2.0, col="red")

### summarize to show data points that are plotted on K-M plot--
summary(km.fit)

### compare survival between treatments---
km.fit1<-survfit(Surv(futime, fustat)~rx, data=ovarian)
km.fit1 
summary(km.fit1)
### plot the survival curves from each treatment--
plot(km.fit1, lty=c(1,3),cex=1.8,cex.lab=1.5,cex.axis=1.5, 
     xlab="Time, days",ylab="Probability of survival", lwd=3.0, 
     col=c("red","blue")) 
legend("bottomleft",c("Rx=1","Rx=2"),lty=c(1,3),col=c("red","blue"), 
       cex=1.5,lwd=2.0, bty="n") 

#### Cumulative hazard function ----------- 
plot(km.fit1,fun="cumhaz",lty=c(1,3),cex=1.8,cex.lab=1.5,cex.axis=1.5, 
     xlab="Time, days",ylab="Probability of survival", lwd=3.0, col=c("red","blue"))

### Are survival distribs different between treatment groups?---
  ### use hypothesis testing using log rank test--
test<-survdiff(Surv(futime, fustat)~rx,data = ovarian) 
test 
### p-value not sig, thus no diff, Ho not rejected--

### test whether age groups affect survival---
  ### mathangi  needs to provide code she used to make age groups--
test2<-survdiff(Surv(futime, fustat)~agecut,data = ovarian) 
test2


#### Cox models---
library(survminer)
cph.fit <- coxph(Surv(futime, fustat) ~ 
                   rx+resid.ds+ecog.ps+age,data = ovarian,ties="breslow") 
summary(cph.fit) 
##########Test the significance of age------------ 
cph.fit.noage <- coxph(Surv(futime, fustat) ~ rx+resid.ds+ecog.ps,data = ovarian, ties="breslow") 
cph.fit.noage$loglik
cph.fit$loglik
## Partial loglikelihood ratio test 
lrt.age = 2*(cph.fit$loglik[2] - cph.fit.noage$loglik[2]) 
lrt.age

######## Final model ----------------------- 
cph.fit.age<-coxph(Surv(futime, fustat) ~ age, data = ovarian,method="breslow")
summary(cph.fit.age)

##### Estimated Survival function--------
newdf=data.frame(age=c(55,60,70))
plot(survfit(cph.fit.age,newdf),cex=1.8,cex.lab=1.5, cex.axis=1.5, xlab="Time,days",lty=c(1,2,3), ylab="Estimated Probability of survival", lwd=3.0)
legend("bottomleft", legend=c("Age=55yrs","Age=60yrs", "Age=70yrs"),lty=c(1,2,3)) 

##### check proportional odds assumption for age as predictor----------- 
agecut<-cut(ovarian$age,c(quantile(ovarian$age,0:2/2))) 
kmfit.age<-survfit(Surv(futime, fustat)~agecut, data=ovarian) 
plot(kmfit.age, lty=c(1,2),cex=1.8,cex.lab=1.5,cex.axis=1.5, xlab="Time, days",ylab="Probability of survival", lwd=3.0, col=c("red","blue"))
legend("bottomleft",c("Age<=57","Age>57" ),lty=c(1,2,3),col=c("red","blue"), cex=1.5, lwd=2.0, bty="n")

#### Types of Residuals-------------------- 
mtg.1 = resid(cph.fit.age,type="martingale") 
coxsn.1 = ovarian$fustat - mtg.1 
dev.1 = resid(cph.fit.age,type="deviance") 
score.1 = resid(cph.fit.age,type="score") 
schoen.1 = resid(cph.fit.age,type="schoenfeld") 
dfb.1 = resid(cph.fit.age,type="dfbeta") 
dfbs.1 = resid(cph.fit.age,type="dfbetas") 

##### Cox proportional hazards assumption------------ 
temp <- cox.zph(cph.fit.age) 
temp 
plot(temp) 

#### Overall fit - Cox Snell residual 
cumhaz_coxsnell<-survfit(Surv(coxsn.1,ovarian$fustat)~1,type="fleming-harrington") 
plot(cumhaz_coxsnell$time,-log(cumhaz_coxsnell$surv),xlim=c(0,3), ylim=c(0,3),xlab="Cox-snell residual", ylab="Cumulative hazard of CS-residuals", main="Overall fit of the model",cex=1.5,cex.axis=1.5,cex.lab=1.5)
lines(c(0,3),c(0,3),lwd=2.0) 

#### Deviance residual-Outlier detection----------------- 
plot(dev.1,ylab="Deviance residual",, 
     type="b",cex=1.5,cex.axis=1.5,cex.lab=1.5, 
     xlab="Subject number", ylim=c(-3,3)) 
abline(h=0, lty=2, lwd = 2.0, col="blue") 
plot(ovarian$age,dev.1,ylab="",cex=1.5,cex.axis=1.5,cex.la
     b=1.5, xlab="Age, years", ylim=c(-3,3)) 
lines(lowess(ovarian$age,dev.1)) 
abline(h=0, lty=2, lwd = 2.0, col="blue")
### Influential points------------------------ 
par(mfrow=c(1,1))
fb.1 = resid(cph.fit.age,type="dfbeta") 
plot(dfb.1,ylim=c(--.04,0.04),cex.lab=1.4,cex.axis=1.5, 
     cex=2.0,type="b") 
abline(h=0, lty=2, lwd = 2.0, col="blue") 
