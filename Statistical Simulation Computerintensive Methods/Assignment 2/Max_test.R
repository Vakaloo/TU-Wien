f = function(y,a,b) {
  gamma(a+b)/(gamma(a)*gamma(b))*y^(a-1)*(1-y)^(b-1)
}

# Initial values
a <- 2
b <- 5

# find the maximum of f(x) within the interval [0, 1]
ans = optimize(f, c(0,1), maximum = TRUE, a = a,b = b)

x_max = ans$maximum
y_max = ans$objective

cat("The max value of the beta distribution is: ", y_max)

# plot f(x)
x = seq(0, 1, length.out = 100)
plot(x, f(x,a,b), type = 'l')

points(x_max, y_max, pch = 15)
text(x = x_max, y = y_max, labels = 'Maximum',
     pos = 4, col = 'blue')

abline(h = y_max, col = "red")
