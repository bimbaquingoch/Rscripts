#x->Temperatura(F)
#y->Precio
#Ingreso Vectores
x=c(12,24,3,13,38,4,5,15,25,45,25)
y=c(319,289,389,239,149,289,359,259,229,129,199)
reg = lm(y~x)
anova(reg)
