orddata<-read.csv("Painscore.csv", sep=",")
head(orddata)
summary(orddata)
### List data in a table for painscore and TRT status--
lapply(orddata[, c("painscore", "TRT")], table)

### Cross tabulations---
ftable(xtabs(~painscore+TRT, data=orddata))

#### Re-arrange data for stacked bar chart--
count<-ftable(xtabs(~painscore+TRT, data=orddata))
install.packages("reshape2")
library(reshape2)
df1 = melt(count)
library(ggplot2)
names(df1) = c("value", "TRT", "Count")
df1
 ### Stacked bar chart---
plot<-ggplot(df1, aes(x=TRT, y=Count, fill=value))+
  geom_bar(stat="identity")
plot+ theme(panel.background = element_rect(fill='white', colour='red'))+
  theme(axis.title.y = element_text(colour = 'blue', size = 25, face='bold'))+
  theme(axis.text.y = element_text(size = 20, colour='black'))+
  theme(axis.title.x = element_text(colour = 'blue', size = 25, face='bold'))+
  theme(axis.text.x = element_text(size = 20, colour='black'))+
  ggtitle("Stacked Bar Chart")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="right")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size = 10))

### side-by-side box plot (grid plot)--
plot2<-ggplot(orddata,aes(x=painscore,y=Cmax))+
  geom_boxplot(size=1)+
  geom_jitter(alpha=0.5)+
  facet_grid(.~TRT)
plot2+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'blue', size = 25, face='bold'))+
  theme(axis.text.y = element_text(size = 20, colour='black'))+
  theme(axis.title.x = element_text(colour = 'blue', size = 25, face='bold'))+
  theme(axis.text.x = element_text(size = 10, colour='black', angle=45))+
  ggtitle("Grid Plot")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))


#### Cumulative logit model (BASE MODEL)---
install.packages("MASS")
library(MASS)
fit.pain0<-polr(painscore~1, data=orddata, Hess=TRUE)
summary(fit.pain0)

##### Store table and calculate -pvalues and add to the original summary-----
ctable0<-coef(summary(fit.pain0))
### calculate p-value ------
p0<-pnorm(abs(ctable0[,"t value"]),lower.tail=FALSE)*2
## Combined table ----------
(ctable0<-cbind(ctable0,'p value' =p0))


#### Cumulative logit model (FULL MODEL)---
fit.pain1<-polr(painscore~Cmax+WT+GENDER+factor(isTRT), 
      data=orddata, Hess=TRUE)
summary(fit.pain1)

ctable1<-coef(summary(fit.pain1))
p1<-pnorm(abs(ctable1[,"t value"]),lower.tail=FALSE)*2
(ctable1<-cbind(ctable1,'p value' =p1))
ctable1

### Reduced model---
fit.pain_red<-polr(painscore~Cmax,data=orddata, Hess=TRUE) 
summary(fit.pain_red)
summary(fit.pain1)

ctable_red<-coef(summary(fit.pain_red))
p_red<-pnorm(abs(ctable_red[,"t value"]),lower.tail=FALSE)*2
(ctable_red<-cbind(ctable_red,'p value' =p_red))


### Different representation of categories--
fit.pain_red2<-polr(factor(ps)~Cmax, data=orddata, Hess=TRUE)
summary(fit.pain_red2)

ctable_red2<-coef(summary(fit.pain_red2))
p_red2<-pnorm(abs(ctable_red2[,"t value"]),lower.tail=FALSE)*2
(ctable_red2<-cbind(ctable_red2,'p value' =p_red2))

  ### get 95% CIs---
exp(0.02276659)
ci<-exp(confint(fit.pain_red2))
cbind(OR=exp(coef(fit.pain_red2)),ci)
exp(confint(fit.pain_red2))


#### Predictions for the responses-------------------
newdat<- data.frame(Cmax = seq(0,200,length.out = 200 ))
newdat<- cbind(newdat, predict(fit.pain_red2, newdat, type = "probs"))
## show first few rows
head(newdat)

###### Rearrange new dataset-------------
library(reshape2)
lnewdat<-melt(newdat, id.vars = c("Cmax"), variable.name = "Level", value.name = "Probability")
 
  #### Plot of probabilities-------------------
plot3<-ggplot(lnewdat, aes(x = Cmax, y = Probability, colour = Level)) +
  geom_line(size=2)+
  xlab("Cmax (ng/mL)")+
  ylab("Probability of Pain relief in each category")
plot3+ theme(panel.background = element_rect(fill='grey', colour='red'))+
  theme(axis.title.y = element_text(colour = 'blue', size = 20, face='bold'))+
  theme(axis.text.y = element_text(size = 20, colour='black'))+
  theme(axis.title.x = element_text(colour = 'blue', size = 20, face='bold'))+
  theme(axis.text.x = element_text(size = 14, colour='black', angle=45))+
  ggtitle("Predicted Probabilites")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="right")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size = 10))

##### Check proportional odds assumption-----------
install.packages("Hmisc")
library(Hmisc)
sf <- function(y) {
  c(`Y>=1` = qlogis(mean(y >= 1)),
    `Y>=2` = qlogis(mean(y >= 2)),
    `Y>=3` = qlogis(mean(y >= 3)), 
    `Y>=4` = qlogis(mean(y >= 4)))}
(s <- with(orddata, summary(as.numeric(painscore) ~ Cmax, fun = sf)))

#### Normalize the first set of coefficients to be zero
s[, 5]<- s[, 5] - s[, 3]
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
# print
s

##### Plot to check proportional odds assumption-------------
plot(s, which = 1:4, pch = 1:4, xlab = "logit", main = " ",
     xlim = range(s[,3:5]), cex=1.2)

