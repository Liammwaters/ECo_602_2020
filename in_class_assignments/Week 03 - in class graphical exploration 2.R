install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)



class(penguins)

penguins = data.frame(penguins)

mean(penguins$body_mass_g, na.rm = TRUE)

head(penguins)

summary(penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)


pairs(penguins [,c("sex", "species", "bill_depth_mm")])


summary(penguins$year)




coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
