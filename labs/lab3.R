install.packages("psych")
require(psych)
pairs.panels(iris)
require(here)

dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

plot(ba.tot ~ elev, data = dat_all)


my_vec = dat_all$CEWA
cewa_present_absent = as.numeric(my_vec > 1)
plot(x = dat_all$elev, y = cewa_present_absent)


#---- logistic functions ----
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}


# positive slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

# negative slope

plot(x = dat_all$ba.tot, y = btpi_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 25, slope = -0.6), add = TRUE)



# ---- deliverables ----

head(dat_all)

pairs.panels(dat_habitat[,c("elev", "aspect", "slope")])


plot(x = dat_all$ba.tot, y = dat_all$BTPI)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)


plot(x = dat_all$ba.tot, y = dat_all$BGWA)


curve(logistic_midpoint_slope(x, midpoint = 40, slope = -0.1), add = FALSE, from = 0, to = 100)
points(x = dat_all$ba.tot, y = dat_all$BTPI)
curve(0)


test_vec = dat_all$BTPI
test_present_absent = as.numeric(test_vec > 1)
plot(x = dat_all$ba.tot, y = test_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 42, slope = -0.7), add = TRUE)

#---- Q1 ----


#Q1.1
 
my_vec = dat_all$BTPI
btpi_present_absent = as.numeric(my_vec > 1)

#^these need to run before the png plot

{
  require(here)
  png(
    filename = here("figures", "btpi_plot_hi_res.png"),
    width = 1200, height = 900, 
    res = 120, units = "px")
  
  plot(
    x = dat_all$ba.tot,
    y = btpi_present_absent,
    xlab = "total basal area",
    ylab = "present or absent",
    main = "Band-tailed Pigeon")
  curve(logistic_midpoint_slope(x, midpoint = 25, slope = -0.6), add = TRUE)
  dev.off()
}

#Q1.2
#the bird species is Band-tailed Pigeon. It seems to prefer areas with a lower basal area. This likely translates to a preference for more mature trees and more canopy cover. It shows that they don't like sites with a high density of stems which are typically younger scrubbier forests.



#---- Q2 ----

#Q2.1

deju_vec = dat_all$DEJU
deju_present_absent = as.numeric(deju_vec > 1)

#^these need to run before the png plot

{
  require(here)
  png(
    filename = here("figures", "deju_plot_hi_res.png"),
    width = 1200, height = 900, 
    res = 120, units = "px")
  
  plot(
    x = dat_all$ba.tot,
    y = deju_present_absent,
    xlab = "total basal area",
    ylab = "present or absent",
    main = "Dark-eyed Junco")
  curve(logistic_midpoint_slope(x, midpoint = 42, slope = -0.7), add = TRUE)
  dev.off()
}

#Q2.2 The bird is a Dark-eyed Junco. It seems to prefer areas with a lower basal area, though there are still many more sites that it is absent from, rather than present in, in the lower basal areas, there are almost no present records from a basal area of higher than 48 or so. Since basal area is a measurement of stems, not canopy cover, it is potentially hard to say how this translates to tree cover. I would say though that this shows that DEJU prefer areas with fewer stems, so presumably more mature trees with a greater canopy cover. 


#---- Q3 ----
GRJA_tot = sum(dat_all$GRJA)
GRJA_tot

#---- Q4 ----

GRJA_sites = sum(dat_all$GRJA > 0)
GRJA_sites

