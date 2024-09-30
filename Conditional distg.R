
set.seed(110)
n= 200
K = 20
e = c(runif(50), 1+runif(50))/8 ## Some PIC has lessor divergence in GC, others does not
Y = (rbinom(n, 300, prob=e) + rep(sample(41:50), each=K))/2 ## GC bias
X1 = as.factor(rep(1:10, each=K)) ## PIC
X2 = rexp(n, rate=1/(e))  + rep(sample(21:30), each=K)  ## CAIS
c(rep(F,30), rep(T, 70))
plot(X2~X1)
plot(Y~X1)
plot(Y~X2)
cor(Y, X2)

fit1 <- lm(Y~X1)
fit2 <- lm(Y~X2)
fit3 <- lm(X2~X1)
summary(fit1)
summary(fit2)
summary(fit3)

par(mfrow=c(2,2))
plot(Y~X2)
plot(Y~fit3$residuals, xlab="X2|X1")
plot(fit1$residuals~X2, ylab="Y|X1")
plot(fit1$residuals~fit3$residuals, ylab="Y|X1",xlab="X2|X1")

summary(lm(Y~X2))
summary(lm(Y~fit3$residuals))
summary(lm(fit1$residuals~X2))
summary(lm(fit1$residuals~fit3$residuals)) ## The relationship between GC|PIC and CAIS|PIC becomes conspicuous.

