
require(here)
catrate = read.csv(here("data", "catrate.csv"))





#Q1 a hist

summary(catrate)

png(
  filename = here("figures", "sal_catrate.png")
)
hist(catrate$cat.rate,
     main = "reproductive catastrophe of marbled salamanders",
     xlab = "catastrophe rate", col = "light blue")
dev.off()




#q2 

shapiro.test(catrate$cat.rate)


#Q3 
# the null hypothesis is that the data is normal.
# this sample is most likely from a nonnormal population since the p-value is less than 0.05 although it isn't much lower than .05.



#Q4 the null hypothesis is that the mean is not different from 2/7ths

#Q4 

t.test(x = catrate$cat.rate, mu = (2/7))



# Q5
# 0.01193
# the confidence interval is 0.3526 -- 0.7261 and does not include 0.
# yes, the results of the t.test provide evidence to reject the null hypothesis. The p-value is 0.01193 which is less than the 0.05 threshold that needed to be cleared to reject the null. 


# Q6: 
wilcox.test(x = catrate$cat.rate, mu = 2/7)



#Q7
# p-value is 0.006275
# since the p-value of 0.006275 is lower than 0.05, there is strong evidence for rejecting the null hypothesis


#Q8 
# Q8 The Wilcox test is more appropriate for these data because it is for non-normally distributed data and the shapiro test revealed that this data is not normally distributed.
# both of these tests provide evidence for rejecting the null hypothesis, though the wilcox test is better suited to this sample and also provided a stronger rejection of the null.


#9
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)


shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)
t.test(flipper_length_mm ~ species, data = penguin_dat)


# the shapiro.test doesn't provide evidence to reject the null hypothesis that the flipper lengths are normally distributed for either species.





#Q10
png(
  filename = here("figures", "flipper_hist.png"), width = 500, height = 800
)
par(mfrow = c(2,1))
hist(dat_adelie$flipper_length_mm, main = "adelie", xlab = "flipper length")
hist(dat_chinstrap$flipper_length_mm, main = "chinstrap", xlab = "flipper length")
dev.off()



#Q11

# the alternative hypothesis is that the mean value for the flipper length of the chinstrap penguins is not the same as the mean value for the flipper length of the adelie penguins. Since we are testing for the the difference in means to be greater than 0, this is a one-tailed hypothesis. 

t.test(flipper_length_mm ~ species, data = penguin_dat)






