##### read in adverse event (nausea) dataset ###
aedata<-read.csv("Nausea.csv", header=TRUE)
head(aedata)
summary(aedata)

### Bar graphs ###
library(ggplot2)
plot1<-ggplot(data=aedata, aes(x=factor(Nausea), fill=TRT)) +
  geom_bar(stat="count") +
  facet_grid(.~ TRT) +
  xlab("Occurrence of Nausea") +
  ylab("Count")
plot1

#### plot attributes - axis labels, axis titles, background, legend options ###
plot1+ theme(panel.background = element_rect(fill='white', colour='red'))+ 
  theme(axis.title.y = element_text(colour = 'black', size = 20))+ 
  theme(axis.text.y = element_text(size = 18))+ 
  theme(axis.title.x = element_text(colour = 'black', size = 20))+ 
  theme(axis.text.x = element_text(size = 18))+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+ 
  theme(legend.text = element_text(colour="blue", size = 18, face = "bold"))

### Cross-Tabulation (2x2) table for Nausea by Treatment Option---
table(Nausea=aedata$Nausea,Treatment=aedata$TRT)

### Cross-Tabulation (2x2) table for Nausea by Gender---
table(Nausea=aedata$Nausea, Gender=aedata$Gender, Treatment=aedata$TRT)


### Chi-squared test--Nausea by TRT--
ae.test<-chisq.test(aedata$Nausea, aedata$TRT)
### Display test results---
ae.test
### Display observed/expected results--
ae.test$observed
ae.test$expected
### Display chi squared test p-value--
ae.test$p.value
### Determine chi squared critical value for df=1--
qchisq(0.99,1)
#### any chi squared test statistic greater than 3.84 (with df=1) will reject Null--
qnorm(0.025)
qnorm(0.975)


### Second method (besides chi sqd) to determine differences in variables--
### Confidence intervals for the difference in the two proportions (for two scenarios)--
prop.test(c(34,7), c(100,50))
### 95% CI does NOT contain zero, hence reject Ho (Null hypoth)---

### Subset Treatment by Drug--
aedata_drug<-aedata[aedata$TRT=="Drug",]

### Create 2x2 contingency table by nausea and gender
### for 100 sub who got drug--
table(aedata_drug$Nausea, aedata_drug$Gender)
## determine relative risk (RR) of sample (SRR) as surrogate for population RR--
## find asymptotic 95% CIs for SRR---
gentable<-table(aedata_drug$Nausea, aedata_drug$Gender)
col.total_F<-gentable[1,1]+gentable[2,1]
col.total_M<-gentable[1,2]+gentable[2,2]
p_female<-gentable[2,1]/col.total_F
p_male<-gentable[2,2]/col.total_M
## Calculate SRR--
RR<-p_female/p_male
RR
#### C.I for log(RR)---
SE_logRR<-sqrt((1-p_female)/(p_female*col.total_F)+
                 (1-p_male)/(p_male*col.total_M))
logRR<-log(p_female/p_male)
ci.lb<-logRR-1.96*SE_logRR
ci.ub<-logRR+1.96*SE_logRR
#### Exponentiate to get CI on normal scale-----------
ci_RR<-c(RR,exp(ci.lb),exp(ci.ub))
ci_RR

### Odds ratio (OR) is another way to determine associations b/w 2 nominal variables--
OR = (p_female/(1-p_female))/(p_male/(1-p_male))
OR
log_OR = log(OR)
log_OR
####### C.I for OR ---
SE_logOR<-sqrt((1/gentable[1,1])+(1/gentable[1,2])+(1/gentable[2,1]) + 
                 (1/gentable[2,2])) 
ci.lb<-log_OR - 1.96 * SE_logOR
ci.ub<-log_OR + 1.96 * SE_logOR
#### Exponentiate to get CI on normal scale---
ci_OR<-c(OR,exp(ci.lb),exp(ci.ub))
ci_OR

### Fisher's Exact Test (for small sample sizes) ###
fisher.test(aedata_drug$Nausea, aedata_drug$Gender, alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95,
            simulate.p.value = TRUE, B = 100)

#### Different method for RR and OR--
install.packages("epitools")
library(epitools)
?riskratio
### risk ratio template--
riskratio.wald(aedata_drug$Nausea, aedata_drug$Gender,
               conf.level = 0.95,
               rev = c("both"),
               correction = TRUE,
               verbose = FALSE)
?oddsratio
oddsratio.fisher(aedata_drug$Nausea, aedata_drug$Gender,
                 conf.level = 0.95,
                 rev = c("both"),
                 correction = FALSE,
                 verbose = FALSE)

