
#all the code from the walkthrough

#Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)

apply(dat, MARGIN = 1, FUN = max)


apply(dat, MARGIN = 2, FUN = mean)


require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)

moth_dat = moths[,-1]
head(moth_dat)


n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)


n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)


#function draft 1
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#function draft 2
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


##### 
#debugging

# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

  


rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)




# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)







#---- debugged ---- 
# This clears the current R session's environment
rm(list = ls())

require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)
# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]


# ---- lab 7 Q3 ----
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n = nrow(moth_dat) 
  m = 100 
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
 
  for(i in 1:n_iterations)
  {
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)



alpha = 0.05
anst = moths$anst
n = sum(!is.na(anst))
t_crit = abs(qt(alpha / 2, df = n - 1))

sse = sd(anst) / sqrt(n)

sample_mean = mean(anst)
ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )
