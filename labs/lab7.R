

#---- Q1 ----
#
require(palmerpenguins)
dat_gen = droplevels(subset(penguins, species == "Gentoo"))
gen_bill_na = dat_gen$bill_length_mm
gen_bill = na.omit(gen_bill_na)




#
alpha = 0.05
n = sum(!is.na(gen_bill))
t_crit = abs(qt(alpha / 2, df = n - 1))

#
sse = sd(gen_bill) / sqrt(n)


sample_mean = mean(gen_bill)
ci_para = sse * t_crit

#
conf_int = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_para,
    upper = sample_mean + ci_para
  )






#---- Q2 ----
install.packages("boot")
require(boot)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = gen_bill,
    statistic = boot_mean,
    R = 10000)
print(myboot)


quantile(
  myboot$t,
  c(0.025, 0.975))



# ---- Q3 ----

# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))

#vvv
rarefaction_sampler = function(input_dat, n_iterations)
{
  moth_dat = moths[,-1]
  n_input_rows = nrow(input_dat)
  n = nrow(moth_dat) 
  m = 100 
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  
  for(i in 1:n_iterations)
  {
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)





#---- Q4 ----
#Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))


png(
  filename = here("figures", "SE_MA_moth_rarefaction_curve.png"),
  width = 800
)

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))
 dev.off()









