
#---- line_point_slope function ----

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}


#---- workflow ----

set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)



# example

n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)


#---- Q1 ----
n = 17
mean = 10.4
sd = 2.4
norm_17 = rnorm(n, mean, sd)
norm_17


n = 30
mean = 10.4
sd = 2.4
norm_30 = rnorm(n, mean, sd)
norm_30


n = 300
mean = 10.4
sd = 2.4
norm_300 = rnorm(n, mean, sd)
norm_300


#---- Q2 ----

n = 17
mean = 10.4
sd = 2.4
norm_17 = rnorm(n, mean, sd)

n = 30
mean = 10.4
sd = 2.4
norm_30 = rnorm(n, mean, sd)

n = 300
mean = 10.4
sd = 2.4
norm_300 = rnorm(n, mean, sd)

{
  require(here)
  png(
    filename = here("figures", "lab_04_hist_01.png"),
    width = 700, height = 1400, 
    res = 180, units = "px")
  
  par(mfrow = c(3,1))
  hist(norm_17, main = "Histogram of random points n = 17", sub = "mean = 10.4, sd = 2.4")
  hist(norm_30, main = "Histogram of random points n = 30", sub = "mean = 10.4, sd = 2.4")
  hist(norm_300, main = "Histogram of random points n = 300", sub = "mean = 10.4, sd = 2.4")
  dev.off()
}



#---- Q3 ----
#Q1:
  
 # Q1:

#The norm_17 and norm_30 histograms are coarser with both a narrower spread and with more gaps due to sampling error from a small sample size. norm_300 has a large enough sample size to hide, or smooth out the sampling error.

#Q2:
  
  #(you have a typo in the question)

#Since the first two histograms have relatively small sample sizes, the distribution is not as obviously normal since there are gaps in the spread. These gaps are from sampling error due to random chance of getting an "outlier" or less frequent value. They are all mostly normal distributions, but the more points in the norm_300 histogram allows for a "smoother" look since the random gaps in the distribution of the points is pronounced at low sample sizes and fades at higher sample sizes as sampling error becomes less of an issue due to the increase in data showing a more representative picture.


#---- Q4 ----


{
  require(here)
  png(
    filename = here("figures", "norm_1.png")
    )
  
  x = seq(0, 20, length.out = 1000)
  y = dnorm(x, mean = 10.4, sd = 2.4)
  
  plot(x, y, main = "Normal PDF with mean of 10.4 and sd = 2.4", type = "l", bg = "transparent")
  
  abline(h = 0)
  dev.off()
}



#---- Q5 ----


{
  require(here)
  png(
    filename = here("figures", "sim_data_scatterplots.png"))
par(mfrow = c(2,2))

scatter_1 = 
  {
    set.seed(2000)
    
    n_pts = 50
    x_min = 1
    x_max = 10
    
    x = runif(n = n_pts, min = x_min, max = x_max)
    
  
    dat = data.frame(x = x, y_observed = rnorm(n_pts))
    
    plot(y_observed ~ x, data = dat, pch = 9, col = "violet", main = "scatter_1: \n random points, n = 50", sub = "set.seed = 2000")
    }

scatter_2 = 
  {set.seed(321)
    
    n_pts = 82
    x_min = 1
    x_max = 10
    
    x = runif(n = n_pts, min = x_min, max = x_max)
    
    dat = data.frame(x = x, y_observed = rnorm(n_pts))
    
    plot(y_observed ~ x, data = dat, pch = 5, col = "green", main = "scatter_2: \n random points, n = 82", sub = "set.seed = 321")
  }

scatter_3 = 
  {
    set.seed(50)
    
    n_pts = 90
    x_min = 1
    x_max = 10
    
    x = runif(n = n_pts, min = x_min, max = x_max)
    
    dat = data.frame(x = x, y_observed = rnorm(n_pts))
    
    plot(y_observed ~ x, data = dat, pch = 2, col = "orange", main = "scatter_3: \n random points, n = 90", sub = "set.seed = 50")
  }


scatter_4 = 
  {
    set.seed(7)
    
    n_pts = 100
    x_min = 1
    x_max = 10
    
    x = runif(n = n_pts, min = x_min, max = x_max)
    
    dat = data.frame(x = x, y_observed = rnorm(n_pts))
    
    plot(y_observed ~ x, data = dat, pch = 6, col = "purple", main = "scatter_4: \n random points, n = 100", sub = "set.seed = 7")
  }
dev.off()
}




#---- Q6 ----


line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

{
  require(here)
  png(
    filename = here("figures", "lab4_q6_linear_funct.png"))

  scatter_2 = 
    {set.seed(321)
      
      n_pts = 82
      x_min = 1
      x_max = 10
      
      x = runif(n = n_pts, min = x_min, max = x_max)
      
      dat = data.frame(x = x, y_observed = rnorm(n_pts))
      
      plot(y_observed ~ x, data = dat, pch = 5, col = "green", main = "linear fit of scatter_2: \n random points, n = 82", sub = "set.seed = 321")
    
    guess_x = 5
    guess_y = 0
    guess_slope = 0.08
    curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
    
  }
dev.off()
}

#---- Q7 ----


scatter_2 = 
  {
    set.seed(321)
    
    n_pts = 82
    x_min = 1
    x_max = 10
    
    x = runif(n = n_pts, min = x_min, max = x_max)
    
    dat = data.frame(x = x, y_observed = rnorm(n_pts))
    
    
    guess_x = 5
    guess_y = 0
    guess_slope = 0.08
  }



line_point_slope(dat$x, guess_x, guess_y, guess_slope)

dat = data.frame(x = x, y_observed = rnorm(n_pts), 
                 y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope),
                 resids = y_observed - y_predicted)


head(dat)





