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

# check about streamlining the process and if the str() is what he was looking for

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














