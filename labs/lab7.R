


require(palmerpenguins)
dat_gen = droplevels(subset(penguins, species == "Gentoo"))
gen_bill = dat_gen$bill_length_mm
head(gen_bill)


gen_mean = apply(gen_bill, 2, mean)
gen_quant = apply(dat_gen, 2, quantile, probs=c(0.025, 0.975))
gen = t(rbind(gen_mean, gen_quant))



qt(c(.025, .975))




require(boot)
library(boot())