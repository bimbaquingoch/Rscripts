x=c(1,1,2,1,3,4,4,3,5,4)
y = c(600,649,799,899,950,
      1100,1149,1300,1550,1625)
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
equ = paste("y = ",round(b0),"+",round(b1),"x")
lines(x,yest,col="blue")
legend("topleft",
       c(equ),
       fill=c("blue")
)
y4 = b0 + b1*4 #Se sustituye la X por el valor de agarre propuesto
