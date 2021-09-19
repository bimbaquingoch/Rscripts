x1 = c(2,6,8,3,2,7,9,8,4,6)

x2 = c(1,0,0,0,0,1,0,0,1,1)
x3 = c(0,1,0,1,0,0,0,0,0,0)
x4 = c(0,0,1,0,0,0,1,0,0,0)

y = c( 2.9,3.0,4.8,1.8,2.9,4.9,4.2,4.8,4.4,4.5)
reg = lm(y~x1+x2+x3+x4)
rs = rstandard(reg)
plot(x1,rs)

#verificar la normalidad
plot(reg)
