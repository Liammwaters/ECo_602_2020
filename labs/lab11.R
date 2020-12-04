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



linear = function(x, y_int, slope)
{
  return(y = y_int + slope * x)
}

linear(x = 1, y_int = 1, slope = 1)
