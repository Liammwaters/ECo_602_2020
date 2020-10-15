

#ricker function code
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}


#example plot of ricker funct
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")


# ---- Q1 ----

#1.1
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}


#1.2
require(here)
png(
  filename = here("figures", "lab_5_q1_2.png")
)
{
  curve(exp_fun(x, 1.9, 0.1), add = FALSE, col = "black", lty = 1, ylim = c(0, 2), xlim = c(0,40))
  curve(exp_fun(x, 1.9, 0.3), add = TRUE, col = "black", lty = 3)
  curve(exp_fun(x, 1.2, 0.3), add = TRUE, col = "red", lty = 1)
  curve(exp_fun(x, 1.2, 0.4), add = TRUE, col = "red", lty = 3)
}
dev.off()

#---- Q2 ----
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(exp_fun(x, 1, ), add = TRUE, col = "black", lty = 3, ylim = c(0, 2), xlim = c(0,40))

#the a parameter sets the y-intercept, and the b parameter sets the slope of the curve.


# ---- Q3 ----

ric_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

png(
  filename = here("figures", "lab_5_q3.png")
)
{
  curve(ric_fun(x, 25, 0.1), add = FALSE, col = "black", lty = 1, ylim = c(0, 100), xlim = c(0,100), main = "example ricker model curve")
  curve(ric_fun(x, 20, 0.1), add = TRUE, col = "black", lty = 3)
  curve(ric_fun(x, 10, 0.1), add = TRUE, col = "black", lty = 3)
  curve(ric_fun(x, 75, 0.3), add = TRUE, col = "red", lty = 1)
  curve(ric_fun(x, 50, 0.3), add = TRUE, col = "red", lty = 3)
  curve(ric_fun(x, 40, 0.3), add = TRUE, col = "red", lty = 3)
}
dev.off()



# ---- Q4 ----

# the a parameter seems to adjust the slope of the initial rise.
# the b parameter isn't super obvious, but might signify the x value of the peak of the curve? Not sure what the scale is though


# ---- Q5 ---- 
  require(here)
dat_dispersal = read.csv(here("data", "salamander_dispersal.csv"))

png(
  filename = here("figures", "salamander_dispersal_scatter.png")
)
plot(
  x = dat_dispersal$dist.class,
  y = dat_dispersal$disp.rate.ftb,
  main = "dispersal rate and distance of first time breeding salamanders",
  xlab = "distance class",
  ylab = "dispersal rate",
  pch = 9)
dev.off()

#the x-axis shows the distance class, or the distance a salamander traveled binned into sections.
# the y-axis shows the dispersal rate, or what percent of the salamanders traveled that distance.
  
  
# ---- Q6 ----

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


png(
  filename = here("figures", "salamander_dispersal_scatter_linear.png")
)
plot(
  x = dat_dispersal$dist.class,
  y = dat_dispersal$disp.rate.ftb,
  main = "dispersal rate and distance of first time breeding salamanders",
  xlab = "distance class",
  ylab = "dispersal rate",
  pch = 9)
curve(line_point_slope(x, 1100, 0.1, -.00018), add = T)
dev.off()


# ---- Q7 ----

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

png(
  filename = here("figures", "salamander_dispersal_scatter_exp.png")
)
plot(
  x = dat_dispersal$dist.class,
  y = dat_dispersal$disp.rate.ftb,
  main = "dispersal rate and distance of first time breeding salamanders",
  xlab = "distance class",
  ylab = "dispersal rate",
  pch = 9)
curve(exp_fun(x, .9, .0035), add = T)
dev.off()

# ---- Q8 ----

ric_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

png(
  filename = here("figures", "salamander_dispersal_scatter_ric.png")
)
plot(
  x = dat_dispersal$dist.class,
  y = dat_dispersal$disp.rate.ftb,
  main = "dispersal rate and distance of first time breeding salamanders",
  xlab = "distance class",
  ylab = "dispersal rate",
  pch = 9)
curve(ric_fun(x, .0065, 1/230), add = T)
dev.off()



# ---- Q9 ----

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




plot(
  x = dat_dispersal$dist.class,
  y = dat_dispersal$disp.rate.ftb,
  main = "dispersal rate and distance of first time breeding salamanders",
  xlab = "distance class",
  ylab = "dispersal rate",
  pch = 9)
curve(line_point_slope(x, x_1, y_1, slope_1), add = T)
dev.off()

x_1 = 1100
y_1 = .1
slope_1 = -.00018

resids_linear = 
  line_point_slope(
    dat_dispersal$dist.class,
    x_1,
    y_1,
    slope_1) - 
  dat_dispersal$disp.rate.ftb

png(
  filename = here("figures", "linear_residuals_hist_salamander_dispersal.png")
)
hist(resids_linear, main = "residuals of linear model")
dev.off()


# ---- Q10 ----

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}


plot(
  x = dat_dispersal$dist.class,
  y = dat_dispersal$disp.rate.ftb,
  main = "dispersal rate and distance of first time breeding salamanders",
  xlab = "distance class",
  ylab = "dispersal rate",
  pch = 9)
exp_a = .9
exp_b = .0035
curve(exp_fun(x, exp_a, exp_b), add = T)



exp_a = .9
exp_b = .0035

resids_exp = 
  exp_fun(
    dat_dispersal$dist.class,
    exp_a,
    exp_b) -
  dat_dispersal$disp.rate.ftb

hist(resids_exp, main = "exponential model of residuals")
    


png(
  filename = here("figures", "exponential_residuals_hist_salamander_dispersal.png")
)
hist(resids_linear, main = "residuals of exponential model")
dev.off()




# ---- Q11 ----


ric_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}


plot(
  x = dat_dispersal$dist.class,
  y = dat_dispersal$disp.rate.ftb,
  main = "dispersal rate and distance of first time breeding salamanders",
  xlab = "distance class",
  ylab = "dispersal rate",
  pch = 9)
ric_a = .0065
ric_b = 1/230
curve(ric_fun(x, ric_a, ric_b), add = T)

dev.off()

resids_ricker = 
  ric_fun(
    dat_dispersal$dist.class,
    ric_a,
    ric_b) -
  dat_dispersal$disp.rate.ftb

png(
  filename = here("figures", "ricker_residuals_hist_salamander_dispersal.png")
)

hist(resids_ricker, main = "ricker model of residuals")

dev.off()

