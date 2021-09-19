x=c(4,3,5,5,1,3,2,1,4,5,3,4,1,3,3)
y = c(33150,40570,35105,35174,42230,
      38225,37605,37695,34390,33845,
      36910,34695,37995,36955,33890)
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
equ = paste("y = ",round(b0),"",round(b1),"x")
lines(x,yest,col="blue")
legend("topleft",
       c(equ),
       fill=c("blue")
)
y4 = b0 + b1*4 #Se sustituye la X por el valor de confiablidad propuesto
