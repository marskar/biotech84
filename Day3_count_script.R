seiz.data<- read.csv("countdata.csv", sep=",")
head(seiz.data)
summary(seiz.data)
##### Plot of Seizure data by ####treatment------------
library(ggplot2)
plot<-ggplot(seiz.data, aes(NSeizure, fill = TRT)) +
  geom_histogram(binwidth = 1) +
  facet_grid(TRT ~ .,scales = "free")
plot

### use tapply to get table of mean, sd for #seizures over 3 months---
with(seiz.data, tapply(NSeizure, TRT, function(x) {sprintf("M (SD) = %1.2f (%1.2f)", 
  mean(x), sd(x)) }))

### 2.99 seizures with Drug, 4.20 seizures with placebo, is that a significant diff?---
  ### could use a t-test for a quick and dirty---
  ## We'd like to model a relationship between drug and seizures with regression model-

###### Analyze data using poisson regression---------
fit.pois.base<- glm(NSeizure ~ 1, family = "poisson", data = seiz.data)
summary(fit.pois.base)

fit.pois.full<-glm(NSeizure ~ relevel(TRT,"Placebo") + relevel(Gender,"Female") + Age,
        family = "poisson", data = seiz.data)
summary(fit.pois.full)

### Assess goodness of fit--
with(fit.pois.full, cbind(res.deviance = deviance, df = df.residual,
    pvalue = pchisq(deviance, df.residual, lower.tail = FALSE)))

######## Negative binomial regression ---------------
library(MASS)
fit.nb <- glm.nb(NSeizure ~ relevel(TRT,"Placebo")+Gender+Age, data = seiz.data)
summary(fit.nb)

### Remove gender, since nonsignificant, thus final model--
fit.nb1<-glm.nb(NSeizure ~ relevel(TRT,"Placebo")+Age, data = seiz.data)
summary(fit.nb1)
####### Goodness of fit of the final model -------------
with(fit.nb1, cbind(res.deviance = deviance, df = df.residual,
    p = pchisq(deviance, df.residual, lower.tail = FALSE)))
  ### p-value for chi-sq test, testing deviance between final and saturated, is nonsignificant---
    ## meaning we accept null hypothesis that current model adequately fits data--

##### Confidence interval of the parameter estimates ------------------
(est <- cbind(Estimate = coef(fit.nb1), confint(fit.nb1)))
##### Exponentiate the estimates----------
exp(est)

###### Predictions------------------
newdata2 <- data.frame(Age = rep(seq(from = 20, to = 80,
                                     length.out = 100), 2),
                       TRT = factor(rep(1:2, each = 100), levels = 1:2, labels =
                                      levels(seiz.data$TRT)))
newdata2 <- cbind(newdata2, predict(fit.nb1, newdata2,
                                    type = "link",se.fit=TRUE))
newdata2 <- within(newdata2, {
  NumberofSeizures <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
plot1<-ggplot(newdata2, aes(Age, NumberofSeizures)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = TRT), alpha = .25) +
  geom_line(aes(colour = TRT), size = 2) +
  labs(x = "Age, years", y = "Predicted Number of seizures")
plot1+ theme(panel.background = element_rect(fill='white', colour='red'))+
  theme(axis.title.y = element_text(colour = 'blue', size = 25, face='bold'))+
  theme(axis.text.y = element_text(size = 20, colour='black'))+
  theme(axis.title.x = element_text(colour = 'blue', size = 25, face='bold'))+
  theme(axis.text.x = element_text(size = 20, colour='black'))+
  ggtitle("Model-Predicted Seizure Count by Covariates")+
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 18))+
  theme(legend.position="right")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size = 10))


