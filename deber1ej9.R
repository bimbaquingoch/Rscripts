x=c(1,3,4,4,6,8,10,10,11,13)
y = c(80,97,92,102,103,111,
      119,123,117,136)
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
y4 = b0 + b1*9 #Se sustituye la X por el valor de años propuesto
