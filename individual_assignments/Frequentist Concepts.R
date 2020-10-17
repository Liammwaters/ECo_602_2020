

dbinom(3, 4, p = .75, log = FALSE)
# for number of 3's out of 4 trials, and the probability of success for a single trial at .75


#2
pbinom(3, 4, p = .75, log = FALSE)

#3
(1 - (pbinom(3, 5, p = .75, log = FALSE)))


#4
pnorm(1.2, mean = 2, sd = 2)


#5
1 - pnorm(1.2, mean = 2, sd = 2)


#6
pnorm(3.2, 2, 2) - pnorm(1.2, 2, 2)


#7
# due to sampling error, the histogram does not match the draw distribution for the first few samples. As your number of samples (not sample size) increases, the histogram becomes to take the shape of the draw distribution.


#8
# Due to sample error, the histogram starts out fairly uniform before becoming skewed and finally showing a slightly more normal distribution than the draw.


#9
#the histogram takes on a normal distribution almost immediately and reamins that way throughout the many samples.
