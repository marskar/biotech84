

ls(package:datasets)
head(Theoph)
tail(Theoph)
nrow(Theoph)
ncol(Theoph)
names(Theoph)
Theoph[51,]
Theoph[,4]
Theoph[51,2]
attach(Theoph)
detach(Theoph)
Theoph$Time
Theoph[Theoph$Time<3,]
nrow(Theoph[Theoph$Time<3,])
Theoph[Theoph$Time==0,]
nrow(Theoph[Theoph$Time==0,])
Theoph[Theoph$Wt > 40 & Theoph$Wt < 60,]
mean(Theoph$Wt)
median(Theoph$Wt)
var(Theoph$Wt)
sd(Theoph$Wt)

par(mfrow=c(1,2))
hist(Theoph$Wt, cex=1.5, xlab="Subject Body Weight (kg)", cex.lab=1.5, cex.axis=1.5, col="blue")
hist(Theoph$Dose, cex=1.5, xlab="Theophylline Dose (mg)", cex.lab=1.5, cex.axis=1.5, col="red")

par(mfrow=c(3,3))
plot(Theoph$Time, Theoph$conc, type = "p")
plot(Theoph$Time, Theoph$conc, type = "l")
plot(Theoph$Time, Theoph$conc, type = "b")
plot(Theoph$Time, Theoph$conc, type = "c")
plot(Theoph$Time, Theoph$conc, type = "o")
plot(Theoph$Time, Theoph$conc, type = "h")
plot(Theoph$Time, Theoph$conc, type = "s")
plot(Theoph$Time, Theoph$conc, type = "S")
plot(Theoph$Time, Theoph$conc, type = "n")

plot(Theoph$Time, Theoph$conc, type = "p", cex=1.5, cex.lab=1.5, cex.axis=1.5, col="blue", xlab = "Time (hrs)", ylab="Theophylline Conc (ng/ml)", main = "Theophylline Conc by Time", cex.main=1.5)
?plot
