resp<-read.csv("resp.csv", sep=",")
head(resp)

###### cross tabulations-----
 ###just look at binary resp (nstat) by month--
ftable(addmargins(xtabs(~nstat+month,data=resp)))
 ### now add treatment (drug or placebo) into the mix---
ftable((xtabs(~treatment+nstat+month,data=resp)))
 ### add baseline resp as covariate---
ftable((xtabs(~baseline+nstat+month,data=resp)))

### To identify potential predictor variables---
####### Re arrange data for stacked bar chart---------------
install.packages("reshape2")
library(reshape2)
count1<-ftable(xtabs(~treatment+nstat+month,data=resp))
### melt fnc converts data from a wide format to a 
mydf = melt(count1)
names(mydf) = c("TRT","Outcome","Visit","Count")

###plot it--
####### Stacked bar chart---------------
plot1<-ggplot(mydf, aes(x=Visit, y=Count, fill=Outcome )) +
  geom_bar(stat="identity")+
  facet_grid(.~TRT)
plot1+ theme(panel.background = element_rect(fill='white', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 18, face = "bold"))+
  theme(strip.text.x = element_text(size = 12, face = "bold",colour = "black"))

######### Fit a marginal model-----------------------
install.packages("gee")
install.packages("geepack")
library(gee)
library(geepack)

### base ###
resp_gee0<-gee(nstat ~ 1, data = resp, family = "binomial", id = subject,
               corstr = "independence", scale.fix = TRUE, scale.value = 1)
summary(resp_gee0)

### full ###
resp_gee1<-gee(nstat ~ centre + treatment + sex + baseline + age, 
      data = resp, family = "binomial", id = subject,
      corstr = "independence", scale.fix = TRUE, scale.value = 1)
summary(resp_gee1)
round(summary(resp_gee1)$coefficients,4)
round(summary(resp_gee1)$working.correlation,3)

### independenet matrix not correct, try exchangeable--
###### Exchangeable correlation ----------
resp_gee2<-gee(nstat ~ centre + treatment + sex + baseline + age, 
      data = resp, family = "binomial", id = subject, 
      corstr = "exchangeable", scale.fix = TRUE, scale.value = 1)
summary(resp_gee2)
round(summary(resp_gee2)$coefficients,4)
round(summary(resp_gee2)$working.correlation,3)

###### Hypothesis testing--------
teststat.gee2<-round(summary(resp_gee2)$coefficients,4)
#### calculate p-value ------
pval<-round(pnorm(abs(teststat.gee2[,5]),lower.tail=FALSE)*2,4)
## final table-------
cbind(teststat.gee2,pval)

### remove gender, age from model b/c not significant---
  ### final model---
resp_gee3<-gee(nstat ~ treatment + baseline, 
      data = resp, family = "binomial", id = subject, 
      corstr = "exchangeable", scale.fix = TRUE, scale.value = 1)
summary(resp_gee3)
round(summary(resp_gee3)$coefficients,4)
round(summary(resp_gee3)$working.correlation,3)

###### Hypothesis testing--------
teststat.gee3<-round(summary(resp_gee3)$coefficients,4)
#### calculate p-value ------
pval3<-round(pnorm(abs(teststat.gee3[,5]),lower.tail=FALSE)*2,4)
## final table-------
cbind(teststat.gee3,pval3)
 #### robust SE very close to naive SE, thus correl matrix strucutre is adequate--

### interpretation of population averages through odds ratios---
  ####### Oddsratio and confidence inetrval--------------- 

se<-summary(resp_gee3)$coefficients["treatmenttreatment","Robust S.E."]
ci<-exp(coef(resp_gee3)["treatmenttreatment"] + c(-1,1) * se * qnorm(0.975))
cbind(OR=exp(coef(resp_gee3)["treatmenttreatment"]),ci)

 ##### odds ratio and CI for baseline as predictor---
####### Oddsratio and confidence interval---------------

se1<-summary(resp_gee3)$coefficients["baselinepoor","Robust S.E."]
ci1<-exp(coef(resp_gee3)["baselinepoor"] + c(-1,1) * se1 * qnorm(0.975))
cbind(OR=exp(coef(resp_gee3)["baselinepoor"]),ci1)



#### Generalized linear mixed effects model (GLMM)---
install.packages("lme4")
library(lme4)
resp.glmm0<-glmer(nstat ~ 1 + (1|subject),
                data = resp, family = binomial, REML=F )
summary(resp.glmm0)

resp.glmm<-glmer(nstat ~ centre + treatment + baseline + sex + age +
      (1|subject), data = resp, family = binomial, REML=F )
summary(resp.glmm)

### removed age, gender from model to get final model--
resp.glmm2<-glmer(nstat ~ centre + treatment + baseline+(1|subject),
                  data = resp, family = binomial, REML=F )
summary(resp.glmm2)

 ### removed age, gender, center from model to get final model--
resp.glmm1<-glmer(nstat ~ treatment + baseline+ (1|subject),
    data = resp, family = binomial, REML=F )
summary(resp.glmm1)
### output full fixed effect data--
exp(fixef(resp.glmm1))

#####Compare gee, glmm and glm fits------
out.compare<-round(cbind(exp(coef(resp_gee3)), exp(fixef(resp.glmm2))),3)
rownames(out.compare)<-names(coef(resp_gee3))
colnames(out.compare)<-c('GEE','GLMM')
out.compare
