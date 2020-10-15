dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)



summary(dat_all$WIWR)



wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4)

#Q1
sum(log(dpois(x = wiwa_counts, lambda = 4)))


#Q2
hist(dat_all$WIWR, breaks = 6)

hist(dpois(x = dat_all$WIWR, lambda = 1.6))

median(dat_all$WIWR)


#Q3

dbinom(dat_all$WIWR, 6, .75)