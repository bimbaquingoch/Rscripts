x=c(100,50,100,100,50,80,75,65,90,90)
y=c(9.3,4.8,8.9,6.5,4.2,6.2,7.4,6.0,7.6,6.1)
x2=c(4,3,4,2,2,2,3,4,3,2)
i=c(1:length(x))
reg = lm(y~x)
reg2 = lm(y~x+x2)
plot(x,y)
abline(reg)
anova(reg2)
##valor prueba t
qt(0.25, 9, lower.tail = F)

###ejemplo

y11 = c(4.0,3.5,3.5,3.5,3.5,3.5,3.0,3.0,2.5,2.0)
x11 = c(3.7,3.4,2.5,4.8,4.0,3.0,2.7,1.7,2.2,1.4)
x22 = c(4.5,3.0,4.0,3.7,3.5,3.0,2.5,3.5,2.7,3.6)
x33 = c(4.8,4.2,4.0,3.4,3.2,4.6,3.3,3.1,3.0,2.5)
reg2 = lm(y11~x11+x22+x33)

suma#a)ecuacion de regresion
#b)evaluacion cuando la ejecucion de 6, uso 3, gama 2.8
#c)Bondad de ajuste
#d)multi-colinealidad ?
#e)variable mas significativa

m = cbind(x11,x22,x33)
core = cor(m)
rstudent(reg2)
cooks.distance(reg2)
qf(0.99, df1=2, df2=7)
influence(reg2)
