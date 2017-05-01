pain<-read.csv("Painrelief.csv", sep=",")
summary(pain)

##########################################################################
##### Exploratory plots ########

 ##### Conc vs time curve, all together---
library(ggplot2)
plot1<-ggplot(data = pain, aes(x = TIME, y = CONC, group=DOSE))+geom_point()+
  xlab("Time (hr)")+
  ylab("Concentration (ng/mL)")
plot1+theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 18, face = "bold"))

#### group data by DOSE and plot ####
install.packages("nlme")
library(nlme)
pain.2<-groupedData(CONC~TIME|DOSE, data=pain, 
     labels = list(x="Time(hr)", y="Conc(ng/mL)"))
plot2<-plot(pain.2, aspect=1/1, ylab="Concentration (ng/mL)", 
      xlab="Time (hr)")
plot2
plot22<-ggplot(pain, aes(displ, hwy)) + 
  geom_point() + 
  geom_line(data = grid, colour = "blue", size = 1.5) + 
  geom_text(data = outlier, aes(label = model))

plot23<-ggplot(data=pain, aes(x=TIME, y=CONC)+
                 geom_point())


#### log-transform the drug concentration ###
pain.2$logConc<-log(pain.2$CONC)
pain.2log<-groupedData(logConc~TIME|DOSE, data=pain.2, 
        labels = list(x="Time(hr)", y="Log Conc(ng/mL)"))
plot2.log<-plot(pain.2log, aspect=1/1, ylab="Log Concentration (ng/mL)", 
            xlab="Time (hr)")

#######
plot3<-ggplot(pain, aes(x = DOSE, y = PAINRELIEF)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)
plot3

#######  Exploratory Response Plots ####
## Create 2x2 contingency table by pain relief and Dose for all 160 sub-
table(pain$PAINRELIEF, pain$DOSE)

###### cross tabulations-----
###just look at binary resp (nstat) by month--
ftable(addmargins(xtabs(~PAINRELIEF+TIME,data=pain)))
### now add treatment (drug or placebo) into the mix---
ftable((xtabs(~DOSE+PAINRELIEF+TIME,data=pain)))

### To identify potential predictor variables---
####### Re arrange data for stacked bar chart---------------
library(reshape2)
count1<-ftable(xtabs(~PAINRELIEF+DOSE,data=pain))
count1

### melt fnc converts data from a wide format to a 
mydf = melt(count1)
names(mydf) = c("PainRelief", "Dose", "Count")
head(mydf)

###plot it--
####### Stacked bar chart---------------
plot4<-ggplot(mydf, aes(x= Dose, y= Count, fill=PainRelief)) +
  geom_bar(stat="identity")
plot41<-ggplot(mydf4, aes(x= Dose, y= Count, fill=PainRelief)) +
  geom_bar(stat="identity")
plot4+ theme(panel.background = element_rect(fill='gray', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  theme(legend.position="right")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 18, face = "bold"))+
  theme(strip.text.x = element_text(size = 12, face = "bold",colour = "black"))

##### count of pain relief by time ####
count2<-ftable(xtabs(~PAINRELIEF+TIME,data=pain))
count2
mydf2 = melt(count2)
names(mydf2) = c("PainRelief", "Time", "Count")
mydf2

plot5<-ggplot(mydf2, aes(x= Time, y= Count, fill=PainRelief)) +
  geom_bar(stat="identity")
#### plot attributes - axis labels, axis titles, background, #####legend options
plot5+ theme(panel.background = element_rect(fill='gray', colour='red'))+
  theme(axis.title.y = element_text(colour = 'black', size = 20))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(colour = 'black', size = 20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  theme(legend.position="right")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="blue", size = 18, face = "bold"))+
  theme(strip.text.x = element_text(size = 12, face = "bold",colour = "black"))

####### Using only successfully responses ######
### only PainRelief=1 responses###
nopain<-pain[pain$PAINRELIEF==1,]
count3<-ftable(xtabs(~PAINRELIEF+DOSE,data=nopain))
### melt fnc converts data from a wide format to a 
mydf3 = melt(count3)
names(mydf3) = c("PainRelief", "Dose", "Count")
head(mydf3)

####### Stacked bar chart---------------
ggplot(mydf3, aes(x= Dose, y= Count, fill=PainRelief)) +
  geom_bar(stat="identity")

##### count of pain relief by time ####
count4<-ftable(xtabs(~PAINRELIEF+TIME,data=nopain))
mydf4 = melt(count4)
names(mydf4) = c("PainRelief", "Time", "Count")
ggplot(mydf4, aes(x= Time, y= Count, fill=PainRelief)) +
  geom_bar(stat="identity")
write.csv(mydf4, file="mydf4.csv")

#### Martin's overlaid plots ###
ggplot() + 
  geom_bar(data=mydf4, aes(x = Time, weight = Count))

ggplot() + 
  geom_bar(data=pain, aes(x = TIME, weight = PAINRELIEF)) +
  geom_point(data=pain, aes(x = TIME, y = DOSE, color = as.factor(PAINRELIEF))) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Dose"))

ggplot() + 
  geom_bar(data=pain, aes(x = TIME, weight = PAINRELIEF)) +
  geom_point(data=pain, aes(x = TIME, y = CONC*10, color = as.factor(PAINRELIEF))) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Concentration"))

ggplot() + 
  geom_bar(data=pain, aes(x = TIME, weight = PAINRELIEF)) +
  geom_point(data=pain, aes(x = TIME, y = CONC*12,color = factor(PAINRELIEF), 
        shape = factor(DOSE))) +
  scale_y_continuous(sec.axis = sec_axis(~./12, name = "Concentration"))


plot45<-ggplot(mydf4, aes(x= Time, y= Count)) +
  geom_bar(stat="identity") + 
  geom_point(data=pain, aes(x=TIME, y=CONC), color='green')
plot45

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)
### mar - A numeric vector of length 4, which sets the margin sizes in the ###
  ###following order: bottom, left, top, and right. #####
    ###The default is c(5.1, 4.1, 4.1, 2.1) ####
## Plot first set of data and draw its axis
plot(pain$TIME, pain$CONC, pch=16, axes=FALSE, ylim=c(0,10), xlab="", ylab="", 
     type="p",col="black")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Drug Concentration (ng/mL)",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(mydf4$Time, mydf4$Count, pch=15,  xlab="", ylab="", ylim=c(0,120), 
     axes=FALSE, type="l", col="red")
## a little farther out (line=4) to make room for labels
mtext("Count of Pain Relief",side=4,col="red",line=4) 
axis(4, ylim=c(0,110), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,xlim=c(0,8))
axis(1,pretty(range(pain$TIME),1))
mtext("Time (Hours)",side=1,col="black",line=2.5)  

## Add Legend
legend("topleft",legend=c("Drug Conc","Pain Relief Count"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))
install.packages("plotrix")
library(plotrix)
example(twoord.plot)
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(pain$TIME, pain$CONC) # first plot
par(new = TRUE)
plot(mydf4$Time, mydf4$PainRelief, type = "l", axes = FALSE, bty = "n",
     ylim=c(0,120), xlab = "", ylab = "")
axis(side=4, at = pretty(range(z)))
mtext("z", side=4, line=3)

##############################################################################
###### NCA analysis on pain data to get PK values ###
install.packages("PKNCA")
### Perform NCA ####
library(PKNCA)
library(knitr)

## By default it is groupedData; convert it to a data frame for use--
### maps conc to any variable or grouping factor ##
my.conc<-PKNCAconc(as.data.frame(pain), CONC~TIME|ID)

## Dosing data needs to only have one row per dose, so subset for that first##
d.dose<-unique(pain[pain$TIME == 0.0,
                       c("ARM","ID","TIME","CONC", "PAINRELIEF","DOSE")])

knitr::kable(d.dose,
             caption="Dosing data extracted from data set")

### by dose
my.dose <- PKNCAdose(d.dose, DOSE~TIME|ID)
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
my.intervals <- data.frame(start=0.0,
                           end=Inf,
                           cmax=TRUE,
                           tmax=TRUE,
                           aucinf=TRUE,
                           auclast=TRUE,
                           cl=TRUE,
                           vz=TRUE,
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
pain.0<-pain[pain$TIME==0,]

PKresults2<-t(PKresults1)
### melt PK data ###
mPKresults<-melt(PKresults1, id=c("ID", "PPTESTCD"))
### cast the melted data ##
### cast(data, formula, function) ###
PKresults3<-dcast(mPKresults, ID~variable+PPTESTCD)
write.csv(PKresults3, file="PKNCA results.csv")
#### Cbind pk results to subsetted data.frame ###
pain.nca<-cbind2(PKresults3, pain.0)
head(pain.nca)
write.csv(pain.nca, file="FinalPKNCA.csv")
### drop columns except cmax, auclast, HL ###
pain.nca1<-pain.nca[c(-2, -4,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,
                      -19,-20,-21,-22,-23,-24,-25,-27)]
write.csv(pain.nca1, file="FinalPKNCA1.csv")
head(pain.nca1)
round((pain.nca1)$PPORRES_auclast,3)

nca<-read.csv("FinalPKNCA1.csv", header=TRUE)

### plot overlay of dose vs AUC, with dose vs PainRelief ###
   ## to show that increasing exposure does NOT increase response ###
boxplot(PPORRES_auclast ~ DOSE, data = nca, main="AUClast by Dose", xlab="Dose (mg)",
        ylab="AUClast (hr*ng/mL)")
boxplot(PPORRES_cmax ~ DOSE, data = nca, main="Cmax by Dose", xlab="Dose (mg)",
        ylab="Cmax (ng/mL)")


############################################################################
############################################################################
#### generalized linear mixed models for longitudinal binary responses----

######### Fit a marginal model-----------------------
library(gee)
library(geepack)
pain_gee1 <- gee(PAINRELIEF ~ as.factor(DOSE)+CONC+TIME, 
    data = pain, family = "binomial", id = ID, 
    corstr = "independence", scale.fix = TRUE, scale.value = 1)
summary(pain_gee1)

###### Hypothesis testing--------
teststat.gee1<-round(summary(pain_gee1)$coefficients,4)
#### calculate p-value ------
pval1<-round(pnorm(abs(teststat.gee1[,5]),lower.tail=FALSE)*2,4)
## final table-------
cbind(teststat.gee1,pval1)

### independenet matrix not correct, try exchangeable--

###### Exchangeable correlation ----------
pain_gee2<- gee(PAINRELIEF ~ as.factor(DOSE)+CONC+TIME, data = pain, 
    family = "binomial", id = ID, corstr = "exchangeable", 
    scale.fix = TRUE, scale.value = 1)
summary(pain_gee2)
round(summary(pain_gee2)$coefficients,4)
round(summary(pain_gee2)$working.correlation,3)

#### Auto-regressive correl matix---
pain_gee3<-gee(PAINRELIEF~ as.factor(DOSE)+CONC+TIME, data = pain, 
    family="binomial",  id=ID, corstr="AR-M", 
    scale.fix=TRUE, scale.value=1)
summary(pain_gee3)
round(summary(pain_gee3)$coefficients,4)
round(summary(pain_gee3)$working.correlation,3)

#### Unstructured matrix---
pain_gee4<-gee(PAINRELIEF~as.factor(DOSE)+CONC+TIME, data=pain, 
    family="binomial", id=ID, corstr="unstructured", 
    scale.fix=TRUE, scale.value=1)
summary(pain_gee4)   
round(summary(pain_gee4)$coefficients,4)
round(summary(pain_gee4)$working.correlation,3)
####Looks the best----

###### Full Model Hypothesis Testing--------
teststat.gee4<-round(summary(pain_gee4)$coefficients,4)
#### calculate p-value ------
pval4<-round(pnorm(abs(teststat.gee4[,5]),lower.tail=FALSE)*2,4)
## final table-------
cbind(teststat.gee4,pval4)


#### Baseline model; intercept-only model---
pain_geeBL<-gee(PAINRELIEF~1, data=pain, family="binomial", 
    id=ID, corstr="unstructured", scale.fix=TRUE, scale.value=1)
summary(pain_geeBL)
round(summary(pain_geeBL)$coefficients,4)
round(summary(pain_geeBL)$working.correlation,3)

##### odds ratio and CI for baseline as predictor---
se.BL <- summary(pain_geeBL)$coefficients["(Intercept)","Robust S.E."]
ci.BL<-exp(coef(pain_geeBL)["(Intercept)"] + c(-1,1) * se.BL * qnorm(0.975))
cbind(OR=exp(coef(pain_geeBL)["(Intercept)"]),ci.BL)

###### Base Model Hypothesis Testing--------
teststat.geeBL<-round(summary(pain_geeBL)$coefficients,5)
#### calculate p-value ------
pval.BL<-pnorm(abs(teststat.geeBL[,5]),lower.tail=FALSE)*2
## final table-------
cbind(teststat.geeBL,pval.BL)

baselineprob=exp(-0.5402513)/(1+exp(-0.5402513))
baselineprob

### interpretation of population averages through odds ratios---
####### Oddsratio and confidence inetrval--------------- 
se.dose5 <- summary(pain_gee4)$coefficients["as.factor(DOSE)5","Robust S.E."]
ci.dose5<-exp(coef(pain_gee4)["as.factor(DOSE)5"] + c(-1,1) * se.dose5 * qnorm(0.975))
cbind(OR=exp(coef(pain_gee4)["as.factor(DOSE)5"]),ci.dose5)

se.dose20<- summary(pain_gee1)$coefficients["as.factor(DOSE)20","Robust S.E."]
ci.dose20<-exp(coef(pain_gee1)["as.factor(DOSE)20"] + c(-1,1) * se.dose20 * qnorm(0.975))
cbind(OR=exp(coef(pain_gee1)["as.factor(DOSE)20"]),ci.dose20)

se.dose80<- summary(pain_gee1)$coefficients["as.factor(DOSE)80","Robust S.E."]
ci.dose80<-exp(coef(pain_gee1)["as.factor(DOSE)80"] + c(-1,1) * se.dose80 * qnorm(0.975))
cbind(OR=exp(coef(pain_gee1)["as.factor(DOSE)80"]),ci.dose80)

#### Generalized linear mixed effects model (GLMM)---
library(lme4)
DOSE0520<-pain$DOSE[pain$DOSE<80]
DOSE0520

#### Baseline model (intercept-only)---
pain.glmmBL<-glmer(PAINRELIEF ~ 1+(1|ID),data = pain, family = binomial, REML=F)
pain.glmmBL
summary(pain.glmmBL)
#### FULL model---
pain.glmm<-glmer(PAINRELIEF ~ as.factor(DOSE)+TIME+CONC+(1|ID),data = pain, 
        family = binomial, REML=F)
summary(pain.glmm)
########Test the significance of adding ALL covariates = Full model vs base------------ 
pain.glmmBL$logLik
pain.glmm$logLik
##loglikelihood ratio test (2*(LLfull-LLbase/red)) ---
lrt.BLFull = 2*((-1020.7) - (-1089.131))
lrt.BLFull
### Determine chi squared critical value for df1--
qchisq(0.95,5)
install.packages("lmtest")
library(lmtest)
lrtest(pain.glmm, pain.glmmBL)

### Added random effect to CONC and ID--
pain.glmm1<-glmer(PAINRELIEF ~ as.factor(DOSE)+TIME+CONC+(1|ID)+(1|CONC),
        data = pain, family = binomial, REML=TRUE)
summary(pain.glmm1)
lrtest(pain.glmm1, pain.glmm)
### output full fixed effect data--
exp(fixef(pain.glmm1))

### Added random effect to CONC and ID, REMOVED 'TIME'--
pain.glmm2<-glmer(PAINRELIEF ~ as.factor(DOSE)+CONC+(1|ID)+(1|CONC),
          data = pain, family = binomial, REML=F)
summary(pain.glmm2)
exp(fixef(pain.glmm2))
lrtest(pain.glmm1, pain.glmm2)


### Removed CONC and RE on CONC--
pain.glmm3<-glmer(PAINRELIEF ~ as.factor(DOSE)+(1|ID),data = pain, family = binomial, REML=F)
summary(pain.glmm3)

#####Compare gee, glmm and glm fits------
out.compare <- round(cbind(exp(coef(pain_gee4)), exp(fixef(pain.glmm1))),3)
rownames(out.compare) <- names(coef(pain_gee4))
colnames(out.compare) <- c('GEE','GLMM')
out.compare
