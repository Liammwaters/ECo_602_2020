require(here)

birds = read.csv(here("data", "bird.sub.csv"))
hab = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(birds, hab)
dim(birdhab)
summary(birdhab)
head(birdhab)


plot(BRCR ~ ls, data = birdhab)
fit_1 = lm(BRCR ~ ls, data = birdhab)
abline(fit_1)

coef(fit_1)



linear = function(x, y_int, slope)
{
  return(numeric())
}

linear(x = 1, y_int = 1, slope = 1)
