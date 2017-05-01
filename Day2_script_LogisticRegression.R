#### read in data---
aedata<-read.csv("Nausea.csv", header=TRUE)

###subset Trt data---
aedata_drug<-aedata[aedata$TRT == "Drug",]

### Fit logistic regression---
fit.nausea<-glm(aedata_drug$Nausea ~ 1, family=binomial)
summary(fit.nausea)

#### consider full model with all covariates--
fit.nausea.full<-glm(Nausea ~ AUC+ WT+ relevel(Gender,"Male")+ RACE,
    family=binomial, data=aedata_drug)

summary(fit.nausea.full)

pchisq(deviance(fit.nausea.full),df.residual(fit.nausea.full),lower=FALSE)
### p-value non-significant for WT and all Races, meaning they not influence nausea--
   ### can be removed from model, since don't improve predictive aspect of model
  ### p-values significant for AUC and Gender, suggesting they do infuence nausea--
    ### keep in model, as they help improve predictive nature of model

### Reduced model that removed non-signif WT and Race--
fit.nausea.reduced<-glm(formula = Nausea ~ AUC + relevel(Gender, "Male"), 
    family = binomial, data = aedata_drug)
summary(fit.nausea.reduced)
pchisq(deviance(fit.nausea.reduced),df.residual(fit.nausea.reduced),lower=FALSE)
exp(2.19)
exp(0.00028*1000)
exp(0.00028)
exp(-3.29)

### 95% CIs for odds ratio (OR)--
exp(confint(fit.nausea.reduced))

### Plot of final model---
install.packages("faraway")
library(faraway)
summary(aedata_drug)
par(mfrow=c(1,1))
x1_AUC<-seq(0,30000, by = 100)
x2_gender<-rbinom(length(x1_AUC), 1,0.5)
fit.logit<-ilogit(fit.nausea.reduced$coef[1]+fit.nausea.reduced$coef[2]*x1_AUC+ 
                    fit.nausea.reduced$coef[3]*x2_gender)
plot(x1_AUC, fit.logit,xlab="AUC, ng.h/ml", 
     ylab="Predicted Probability of Nausea",cex=1.8, cex.lab=1.5,cex.axis = 1.8)
text(2000,0.6, "Females", cex=2.0)
text(10000,0.15,"Males" , cex=2.0)

### Another model diagnostic (besides deviance, G^2) is Pearson's chi sq statistic--
pearson_Chisq<-sum(residuals(fit.nausea.reduced, type="pearson")^2)
pearson_Chisq

### A third diagnostic is Pearson's residuals--
plot(aedata_drug$ID, residuals(fit.nausea.reduced, type="pearson"), 
     xlab=" Subject ID", ylab="Pearson's residuals", cex=2.0, ylim=c(-3,3), 
     cex.lab=1.5, cex.axis=1.8)

### Two other link functions (besides logit or logistic reg)--
 ### Probit link function--
fit.nausea.reduced_probit<-glm(Nausea ~ AUC+ relevel(Gender,"Male"),
family=binomial(link=probit), data=aedata_drug)
summary(fit.nausea.reduced_probit)

 ### complementary log-log model -------
fit.nausea.reduced_cloglog<-glm(Nausea ~ AUC+ relevel(Gender,"Male"), 
        family=binomial(link=cloglog),data=aedata_drug)
summary(fit.nausea.reduced_cloglog)

