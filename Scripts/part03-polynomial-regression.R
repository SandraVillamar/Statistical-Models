load("datasets/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] 
tmp = tmp[complete.cases(tmp),]
tmp = as.data.frame(tmp)
names(tmp) = c("hp", "mpg", "wt", "len", "wd")
dat = tmp
attach(dat)

# let's focus on mpg ~ hp
# we fit a line
fit = lm(mpg ~ hp)
summary(fit)
plot(hp, mpg, pch = 16, cex = 1)
abline(fit, col = "blue", lwd = 3)

# fitting a line in log-log scale
logfit = lm(log(mpg) ~ log(hp))
summary(logfit)
plot(log(hp), log(mpg), pch = 16)
abline(logfit, col = "red", lwd = 3)

# comparing the two fits visually
logfit = lm(mpg ~ hp, data=log(dat))
pts = seq(0, 600, len=100) # covers the range of hp
val = predict(logfit, data.frame(hp = log(pts)))
plot(hp, mpg, pch = 16)
abline(fit, col = "blue", lwd = 3)
lines(pts, exp(val), col="red", lwd = 3)

# fitting polynomial of degree 2
fit = lm(mpg ~ hp + I(hp^2))
summary(fit)
# same as
fit = lm(mpg ~ poly(hp, 2, raw = TRUE))
summary(fit)
val = predict(fit, data.frame(hp = pts))
plot(hp, mpg, pch = 16)
lines(pts, val, col="red", lwd = 3)

# R-squared for polynomial models of increasing degree
dmax = 30
R0 = rep(NA, dmax)
for (d in 1:dmax){
	fit = lm(mpg ~ poly(hp, d, raw = TRUE))
	S = summary(fit)
	# print(S) 
	R0[d] = S$r.squared
	}
plot(1:dmax, R0, type="o", xlab = "Degree", ylab = "R-squared", main = "R-squared for polynomial models", lwd = 3, col = "blue", pch = 5)

# comparing polynomial models visually
plot(hp, mpg, pch = 16)
for (d in 1:10){
	fit = lm(mpg ~ poly(hp, d, raw = TRUE))
	val = predict(fit, data.frame(hp = pts))
	lines(pts, val, col=rainbow(10)[d], lwd = 3)
	}

# piecewise constant fit
plot(hp, mpg, pch = 16, main="Piecewise constant fit", cex = 1)
K = quantile(hp, seq(0, 1, len = 6), type=1)
pts = rep(0,10)
val = rep(0,10)
for (j in 1:5){
	I = (K[j] < hp)&(hp <= K[j+1])
	fit = lm(mpg[I] ~ 1)
	pts[2*j-1] = K[j]
	pts[2*j] = K[j+1]
	val[2*j-1] = coef(fit)
	val[2*j] = coef(fit)
	}
lines(pts, val, col="red", lwd = 3)

# piecewise linear fit
plot(hp, mpg, pch = 16, main="Piecewise linear fit", cex = 1)
K = quantile(hp, seq(0, 1, len = 6), type=1)
pts = rep(0,10)
val = rep(0,10)
for (j in 1:5){
	I = (K[j] < hp)&(hp <= K[j+1])
	fit = lm(mpg[I] ~ hp[I])
	pts[2*j-1] = K[j]
	pts[2*j] = K[j+1]
	val[2*j-1] = coef(fit)%*%c(1,K[j])
	val[2*j] = coef(fit)%*%c(1,K[j+1])
	}
lines(pts, val, col="green", lwd = 3)

# splines fit of degree 1 (linear)
require(splines)
K = quantile(hp, c(.2,.4,.6,.8), type=1)
plot(hp, mpg, pch = 16, main="Linear splines fit")
fit = lm(mpg ~ bs(hp, degree=1, knots=K))
pts = seq(0, 600, len=100)
val = predict(fit, data.frame(hp = pts))
lines(pts, val, col="red", lwd = 3)

# splines fit of degree 3 (cubic)
K = quantile(hp, c(.2,.4,.6,.8), type=1)
plot(hp, mpg, pch = 16, main="Cubic splines fit", cex = 1)
fit = lm(mpg ~ bs(hp,degree=3,knots=K))
val = predict(fit, data.frame(hp = pts))
lines(pts, val, col="blue", lwd = 3)

# B-spline basis for the same knots
K = quantile(hp, c(.2,.4,.6,.8), type=1)
B1 = bs(pts, degree=1, knots=K, intercept=TRUE)
B3 = bs(pts, degree=3, knots=K, intercept=TRUE)
par(mfrow=c(1,3))
matplot(pts, B1, type="l", lwd = 3, ylab="", xlab="", main="Linear B-Splines")
matplot(pts, B3, type="l", lwd = 3, ylab="", xlab="", main="Cubic B-Splines")

# natural splines for the same knots
N = ns(pts, knots = K[2:3], intercept = TRUE, Boundary.knots = K[c(1,4)])
matplot(pts, N, type="l", lwd = 3, ylab="", xlab="", main="Natural Splines")

# smoothing splines fit
par(mfrow=c(1,1))
plot(hp, mpg, pch = 16, main="Smoothing splines fit", cex = 1)
fit = smooth.spline(hp, mpg)
lines(fit, col="red", lwd = 3)
# smoother look if the model is evaluated at more locations
fitted = predict(fit, pts) 
lines(fitted, col="green", lwd = 3)
# controlling the degrees of freedom by hand
fit = smooth.spline(hp, mpg, df=10)
fitted = predict(fit, pts) 
lines(fitted, col="blue", lwd = 3)

# polynomial model of degree 2 in hp and wt
fit = lm(mpg ~ hp*wt + I(hp^2) + I(wt^2))
summary(fit)

# spline model of degree 1 + interactions in hp and wt
fit = lm(mpg ~ bs(hp, degree=1, df=6) * bs(wt, degree=1, df=6))

# MARS algorithm for fitting multi-dimensional splines
require(mda)
mars.fit = mars(cbind(hp, wt), mpg)
