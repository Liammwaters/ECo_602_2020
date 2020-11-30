
require(here)

rope = read.csv(here("data", "rope.csv"))
factor(rope$rope.type)
levels(rope$rope.type)


n_obs = nrow(rope)

n_groups = length(levels(rope$rope.type))

#total variance
grand_mean = mean(rope$p.cut, na.rm = T)

ss_tot = sum((rope$p.cut - grand_mean)^2)


#within-group variance
agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x - mean(x)
)

str(agg_resids)

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) (sum((x - mean(x))^2))
)

str(agg_sq_resids)

#final within group sum of squares
ss_within = sum(agg_sq_resids$x)




#final among group sum of squares
ss_among = ss_tot - ss_within





#in an anova table ...
#species = among group resids
#resids = between group resids



df_within = n_obs - n_groups
df_tot = n_obs - 1


ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)

f_ratio = ms_among/ms_within




fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)









# ---- template ----

rm(list = ls())

rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)

n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))

ss_tot = sum((rope$p.cut - mean(rope$p.cut, na.rm = T))^2)
df_tot = n_obs - 1

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) (sum((x - mean(x))^2))
)

ss_within = sum(agg_sq_resids$x)
df_within = n_obs - n_groups

ss_among = ss_tot - ss_within
df_among = df_tot - df_within

ms_within = ss_within / df_within
ms_among  =  ss_among / df_among

f_ratio = ms_among/ms_within
f_pval = pf(f_ratio, df_among, df_within, lower.tail = F)






# ---- self test ----


# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)









