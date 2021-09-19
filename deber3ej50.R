x = c(135,110,130,145,175,160,120)
y = c(145,100,120,120,130,130,110)

reg = lm(y~x)

rs = rstandard(reg)

b0= reg$coefficients[1]
b1= reg$coefficients[2]
yest = b0 +b1*x
plot(yest,rs)

plot(x,y)
abline(reg)

