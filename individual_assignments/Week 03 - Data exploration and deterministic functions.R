install.packages("here")
require("here")
dat_habitat = read.csv(here("data", "hab.sta.csv"))


head(dat_habitat)



#---- linear functions ----

# Calculates the value of x for a linear function, given the coordinates
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



#---- histograms and plots----
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



curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.4), add = TRUE)



hist(dat_habitat$elev)

hist(dat_habitat$aspect, main = "Habitat Aspect")

hist(dat_habitat$slope)

{
require(here)
  png(
  filename = here("figures", "sample_site_terrain_scatterplots.png"),
  width = 450, height = 1000
)
par(mfrow = c(3,1))
plot(x = dat_habitat$elev, y = dat_habitat$ba.tot, col = "red", main = "elevation ~ basal area")
curve(line_point_slope(x, x1 = 20, y1 = 15, slope = 0.03), add = TRUE)

plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot, col = "light green", main = "aspect ~ basal area")
curve(line_point_slope(x, x1 = 1, y1 = 15, slope = 0.05), add = TRUE)

plot(x = dat_habitat$slope, y = dat_habitat$ba.tot, col = "blue", main = "slope ~ basal area")
curve(line_point_slope(x, x1 = 5, y1 = 19, slope = 0.1), add = TRUE)

dev.off()
}

# ^^ make fancy and fit with linear function before submitting

file.exists()

plot(x = dat_habitat$elev, y = dat_habitat$ba.tot)
curve(line_point_slope(x, x1 = 400, y1 = 50, slope = -0.08), add = TRUE)


#Q1
{
  require(here)
  png(
    filename = here("figures", "elev_hist.png")
  )
  hist(dat_habitat$elev, main = "Elevation of sample sites", bg = "transparent")
  dev.off()
}


#Q1.2
  # This histogram shows that the sampling sites are mostly in the 200-500m range in elevation with a peak around 350m as the most frequent elevation. Overall there is a mostly normal distribution with some below 100m and others above 800m.

#Q2
dev.off()

{
  require(here)
  png(
    filename = here("figures", "slope_hist.png")
  )
  hist(dat_habitat$slope, main = "Slope of sample sites", bg = "transparent")
  dev.off()
}

#Q2.2
  #(this question description should be changed to refelct slope and not elevation)
  #The slope also varies a lot between sites, with some near zero % slope and others over 100% slope, but most are between 40 and 80 percent slope. 



#Q3

{
  require(here)
  png(
    filename = here("figures", "aspect_hist.png")
  )
  hist(dat_habitat$aspect, main = "Aspect of sample sites", bg = "transparent")
  dev.off()
}


#Q3.2
#aspect is the compass direction in degrees that the slope faces.

#Q3.3
#Overall the aspect is pretty evenly distributed with the odd exception of 350-400deg (NNW) which is just a fraction of the others.


#Q4.1
#Oregon has a wide range of terrain with some very mountainous areas and some low coastal areas, and some higher flat plains in the eastern part. I'm not sure what part of the state these sites were drawn from, but it would appear from these data that the sites represent at least part of most of these different terrains.

#Q4.2
#I would guess that the range of values for slope and elevation would be much less. Slope and elevation would likely be pretty uniformly distributed, though could show a minor peak. 

#Q5
{
  require(here)
  png(
    filename = here("figures", "terrain_hists_all.png")
  )
  par(mfrow = c(3,1))
  hist(dat_habitat$elev, main = "Elevation of sample sites", bg = "transparent")
  hist(dat_habitat$slope, main = "Slope of sample sites", bg = "transparent")
  hist(dat_habitat$aspect, main = "Aspect of sample sites", bg = "transparent")
  
  dev.off()
}

dev.off()
