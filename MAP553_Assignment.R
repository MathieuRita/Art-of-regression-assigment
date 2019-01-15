library(datasets)
data(pressure)
pp=pressure$pressure
pt=pressure$temperature
logpp=log(pressure$pressure)
logpt=log(pressure$temperature)

# Question 2
plot(logpp, logpt,col="red")
#Question 3
logpp2<-logpp[c(2:19)]
logpt2<- logpt[c(2:19)]

linreg=lm(logpp2~logpt2)
summary(linreg)

B0=linreg$coefficients[1]
B1=linreg$coefficients[2]

appro=B0+B1*logpt

plot(pt, logpp,col="red")
par(new=TRUE)
plot(pt, appro,axes=FALSE, col="green" )

confint(linreg)

#Question 4

x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

new = data.frame(logpt2=c(log(90),log(230)))
pred=predict.lm(linreg,new,interval="confidence")
pred=exp(pred)
pred

# Question 5
logpp=log(pressure$pressure)
logpt=log(pressure$temperature)
t=pressure$temperature
t2=1/t[c(2:19)]
linreg2=lm(logpp2 ~ logpt2+t2)

appro2=linreg2$coefficients[1]+logpp*linreg2$coefficients[2]+(linreg2$coefficients[3]/t)

plot(pt, logpp,col="red")
par(new=TRUE)
plot( pt, appro2,axes=FALSE, col="green" )


summary(linreg)
summary(linreg2)

print("linreg")
summary(linreg)$r.squared
summary(linreg)$adj.r.squared
summary(linreg)$sigma

print("linreg2")
summary(linreg2)$r.squared
summary(linreg2)$adj.r.squared
summary(linreg2)$sigma

print("AIC,BIC")
AIC(linreg,linreg2)
BIC(linreg,linreg2)

te mieux la variable que 1/t

# Question 6
logpp=log(pressure$pressure)
logpt=log(pressure$temperature)
t=pressure$temperature
t2=1/t[c(2:19)]
t22=t2**2
linreg3=lm(logpp[c(2:19)] ~ logpt[c(2:19)]+t2+t22+t[c(2:19)])
summary(linreg3)
confint(linreg3,parm="t[c(2:19)]",level = 0.95)
linreg4=lm(logpp[c(2:19)] ~ logpt[c(2:19)]+t2+t22)
summary(linreg4)
AIC(linreg3,linreg4,linreg2)
BIC(linreg3,linreg4,linreg2)

e=-3.889e+01
A1=7.628e+00
A2=2.680e+02
A3=-1.638e+03
appro3=e+A1*logpt+(A2/t)+(A3/t**2)

plot(pt, appro2,col="red")
par(new=TRUE)
plot( pt, appro3,axes=FALSE, col="green" )
coef(linreg4)
