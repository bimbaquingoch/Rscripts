x = c(4,5,7,8,10,12,12,22)
y = c(12,14,16,15,18,20,24,19)

n = length(x)

reg = lm(y~x)

rs = rstandard(reg)

umbral = 6/n

hi = influence(reg)$hat

plot(x,y)
abline(reg)
