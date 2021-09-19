x=c(2,6,8,8,12,16,20,20,22,26)
y = c(58,105,88,118,117,137,157,169,149,202)
i = c(1:length(x))
plot(x,y)
xmed = mean(x)
ymed = mean(y)
x_xmed = x - xmed
y_ymed = y - ymed
prod = x_xmed * y_ymed
s1 = sum(prod)
cuad = x_xmed^2
s2 = sum(cuad)
b1 = s1/s2
b0 = ymed - b1*xmed
yest = b0 + b1*x
lines(x,yest)
error= y - yest
sce = sum((error)^2)                                           
stc = sum(y_ymed^2)
scr = stc-sce
r2 = scr/stc
rxy = sign(b1)*sqrt(r2)
n = length(x)
s22 = sce/(n-2)
ecm = s22
s = sqrt(ecm)
sb1 = s/sqrt(s2)
t = b1 /sb1
########
reg = lm(y~x)
boo= reg$coefficients[1]
b11= reg$coefficients[2]
summary(reg)

cmr = scr/(1)
ecm = sce/(n-2)
F = cmr/ecm
anova(reg)
##########
plot(x,error)
plot(yest,error)
###########
hi = 1/n + (cuad/s2)
s_error= s*sqrt(1-hi)
res_est = error / s_error
plot(x,res_est)
rstandard(reg)
######
norm = qqnorm(i,plot.it = FLASE)$x
res_est_ord=sort(res_est)

plot(norm,res_est_ord)
eje = c(-1.5,1.5)
lines(eje,eje)
##########
x1 = c(1,2,3,3,4,4,5,6)
y1 = c(55,50,40,45,30,35,25,15)
plot(x1,y1)
abline(reg2)
reg2 = lm(y1~x1)
rs1 = rstandard(reg2)
plot(x1,rs1)
#########
x2 = c(10,10,15,20,20,25,70)
y2 = c(125,130,120,115,120,110,100)
plot(x2,y2)
abline(reg3)
reg3 = lm(y2~x2)
rs2 = rstandard(reg3)
plot(x2,rs2)
hi1 = influence(reg3)
hi11 = hi1$hat

#######
x3 = c(21.9,6,22.8,18.1,12.7,14.5,20,19.2,16,6.6,15.9,9.2,19.7,20,8.3,17.1,10.8,11.1) 
y3 = c(18.54,33.7,19.67,21.01,35.09,19.41,25.58,17.02,24.04,31.42,18.74,26.76,27.72,18.2,25,29.78,37.03,28.64)
plot(x3,y3)
reg4 =lm(y3~x3)
rs3 = rstandard(reg4)
plot(x3,rs3)
hi2 = influence(reg4)$hat
########
x4 = c(32977.4,162365.1,31363.8,56849.0,68848.0,507216.8,44180.1,194455.9,143131.0,35377.5,31062.1,92923.7,54421.2,144152.9,116840.8,62259.4,120966.5,30040.7,36450.8,61288.1)
y4 = c(1130,1400,800,1350,1000,3325,978,2000,1365,950,700,1275,1625,1318.3,773,1200,116,950,897,750)