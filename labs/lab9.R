require(here)

catrate = read.csv(here("data", "catrate.csv"))

head(catrate)

success = sum(catrate$success)
years = sum(catrate$years)
binom.test(success, years)

veg = read.csv(here("data", "vegdata.csv"), header=TRUE)
head(veg)

var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control', 'clipped')
)

shapiro.test(veg$pine[veg$treatment == "control"])
shapiro.test(veg$pine[veg$treatment == "clipped"])

fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control', 'clipped')
)



bartlett.test(pine ~ treatment, data = veg)



# Q1

bartlett.test(body_mass_g ~ species, data = penguins)
#0.0501



# Q2
bartlett.test(body_mass_g ~ sex, data = penguins)
#0.0319



# Q3
dat_groups = aggregate(
  body_mass_g ~ sex * species,
  data = penguins,
  FUN = c
)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g)

#p= 0.1741


# Q4

require(here)
birds = read.csv(here("data", "bird.sta.csv"), header = T)
hab = read.csv(here("data", "hab.sta.csv"), header = T)
birdhab = merge(birds, hab, by = c("basin", "sub", "sta"))

#this creates a contingency table for edge/interior and presence/absence with presence as the first column

br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0) [, 2:1]
br_creeper_table

chisq.test(br_creeper_table)


# null hypothesis is:
# that edge vs interior habitat has no impact on the presence rate of BRCRs


# do brcr have significant habitat preference?
# yes, the low p-value of much less than .05 provides evidence to reject the null hypothesis and suggest that BRCRs do have a significant habitat preference.
