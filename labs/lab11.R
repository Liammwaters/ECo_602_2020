require(here)

birds = read.csv(here("data", "bird.sub.csv"))
hab = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(birds, hab)




plot(BRCR ~ ls, data = birdhab)
fit_1 = lm(BRCR ~ ls, data = birdhab)
abline(fit_1)

#assign the summary table to an object
sum_fit_1 = summary(fit_1)

#this allows you to isolate the parts of the summary table such as:
sum_fit_1$coefficients


# create a function that returns the value of a line
linear = function(x, y_int, slope)
{
  return(y = y_int + slope * x)
}

linear(x = 1, y_int = 1, slope = 1)





# this is wrong
linear_simulator = function(x, y_int, slope, st_dev)
{
  y = y_int + slope * x
  return(y * st_dev)
}


n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}



n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 3.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}
