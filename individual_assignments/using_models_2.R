
require(palmerpenguins)



# Q1
# I would say that just by the boxplots the male penguins of all three species are significantly heavier. THe difference is greatest in the gentoos, and weakest with the chinstrap. For all species though, the box (IQR) does not overlap between male and female.





# Q2
# I think that adding sex would improve the fit of the model since it gives more discrete bins for a value to fall into.




# Q3


fit_both = lm(body_mass_g ~ sex * species, data = penguins)
fit_both


# Q4
summary(fit_both)

# the base case is female adelie 



# Q5 
# you need the estimate value for the intercept and the speciesChinstrap rows.



# Q6
# the average mass of female chinstrap penguins is 3527.21






