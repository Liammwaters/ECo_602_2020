#code needed to run the questions:

require(palmerpenguins)

two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = mean(dat_1, na.rm = T) - mean(dat_2, na.rm = T)
  
  return(difference_in_means)
}



dat_pen = droplevels(subset(penguins, species != "Gentoo"))




# ---- Q1 ----
sse_mean()

require(palmerpenguins)

sse_mean = function(x)
{
  return(sd(x, na.rm = T) / sqrt(length(na.omit(x))))
}

sse_mean(penguins$bill_depth_mm)



# ---- Q2 ---- 
# submit only lines 20-28

two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = mean(dat_1, na.rm = T) - mean(dat_2, na.rm = T)
  
  return(difference_in_means)
}



two_group_resample(dat_pen$bill_length_mm, 50, 99)

#---- random code from workflow ----

#dat_pen removes a species from the datset
dat_pen = droplevels(subset(penguins, species != "Gentoo"))


# for reproduceability
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")


# for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)




#I think Q2 code should be something like this?
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)



agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed




#---- Q3 ----
#this is just the code from the walkthrough, why does it work and do I have to change anything?
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

length(mean_differences)
require(here)

png(
  filename = here("figures", "resampled_penguin_flipper_hist.png")
)
hist(mean_differences)
dev.off()

# ---- Q4 ----
two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  difference_in_means = mean(dat_1, na.rm = T) - mean(dat_2, na.rm = T)
  
  return(difference_in_means)
}
dat_pen = droplevels(subset(penguins, species != "Gentoo"))

#just submit this:
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}

sum(abs(mean_differences) >= 5.8)






# ---- Q6 ----

png(
  filename = here("figures", "bill_length_boxplot.png")
)

boxplot(dat_pen$bill_length_mm ~ dat_pen$species, 
        main = "bill length of Adelie and Chinstrap Penguins",
        xlab = "species", ylab = "bill length in mm")
dev.off()


agg_means = aggregate(
  dat_pen$bill_length_mm ~ dat_pen$species,
  FUN = mean, na.rm = T
)
diff_critt = diff(agg_means[, 2])




agg_means
diff_critt


# ---- Q7 ----
#needs work before submitting

t_test = t.test(dat_pen$bill_length_mm ~ dat_pen$species)
t_test$p.value
# the p-value of 2.900144e-41 means that there is effectively no chance of the null hypothesis (in this case that there is no difference in the bill length of adelie and chinstrap penguins) is supported.




# ---- Q8 ---- 

n = 1000
mean_differences_2 = c()
for (i in 1:n)
{
  mean_differences_2 = c(
    mean_differences_2,
    two_group_resample(dat_pen$bill_length_mm, 68, 152)
  )
}

sum(abs(mean_differences_2) >= diff_critt)

png(
  filename = here("figures", "resampled_penguin_bill_hist.png")
)
hist(mean_differences_2, main = "resampled bill lengths")
dev.off()


head(dat_pen)

t.test(agg_means)




dev.off()
