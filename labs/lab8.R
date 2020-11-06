require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))



t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species, alternative = "less")


install.packages("simpleboot")
require("simpleboot")


adelie = droplevels(subset(penguins, species == "Adelie"))
chinstrap = droplevels(subset(penguins, species == "Chinstrap"))

samp_a = adelie$flipper_length_mm

boot_test = two.boot(adelie$flipper_length_mm, chinstrap$flipper_length_mm, FUN = mean, R = 1000)

summary(boot_test)
head(penguin_dat) 
require(here)


# ---- Q1 ----

# subsets could also be created and run inside the two.boot function

adelie = droplevels(subset(penguins, species == "Adelie"))
chinstrap = droplevels(subset(penguins, species == "Chinstrap"))

pen_boot = two.boot(adelie$flipper_length_mm, chinstrap$flipper_length_mm, FUN = mean, R = 10000, na.rm = T)
summary(pen_boot)
head(pen_boot)


png(
  filename = here("figures", "bootstrapped_flipper_diff_R10000.png"),
  width = 650
)
hist(pen_boot, 
     main = "bootstrapped differences in mean flipper length", 
     sub = "R = 10000; Adelie and Chinstrap Penguins",
     xlim = c(-10, -2))
dev.off()



# ---- Q2 ----
str(pen_boot)


quantile(pen_boot$t, c(0.025, 0.975))






require(boot)
boot.ci(pen_boot)


# ---- Q3 ----

pen_ecdf = ecdf(pen_boot$t)



#---- Q4 ----

# the null hypothesis would be that the flipper lengths of Adelie and Chinstrap Penguins are the same.


# ---- Q5 ----
 
1 - pen_ecdf(-4.5)


1 - pen_ecdf(0)



# ---- Q6 ----

require(here)
veg = read.csv(here( "data", "vegdata.csv"))

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

wilcox.test(pine ~ treatment, data = dat_tree)


# ---- Q7 ----
require(simpleboot)

tree_boot = two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )


quantile(tree_boot$t, c(0.025, 0.975))

#7.2
mean(subset(dat_tree, treatment == "clipped")$pine) -
mean(subset(dat_tree, treatment == "control")$pine)



#---- Q8----

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

head(dat_bird)
head(dat_tree)

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all)

# s.sidi = slope coefficient
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]
slope_observed


plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)


dat_1 = subset(dat_all, select = c(b.sidi, s.sidi))




index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)



plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)





m = 10000

result = numeric(m)

#acutal loop code

m = 10000
for (i in 1:m) 
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
}

# ---- Q9 ----


#building off of the loop in Q8
png(
  filename = here("figures", "regression_slope_with_lines.png")
)
hist(result,
     main = "null distribution of regression slope",
     xlab = "slope parameter",)
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
crit_v = quantile(result, c(0.05))
abline(v = crit_v, lty = 3, col = "red", lwd = 2)
dev.off()


# ---- Q10 ----

quantile(result, c(0.05))

slope_observed = coef(fit_1)[2]
slope_observed

#critical value was -0.01308949 and my observed slope was higher than the critical value.

#that this is in fact an actual representation from these data and that there are likely other factors that play a role in this finding. This result could be limited to this study area and the vegetation and bird species that inhabit it. Or this could be representative of a larger area, but we can't say that from these data. It could be that in this area, there are a number of bird species that are adapted to just a few dominant plants, and  if these plants aren't in a great enough number then some of these birds can't make use of the habitat.






