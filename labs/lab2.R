dat_birds = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")


dat_habitat = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")



head(dat_habitat)


pairs(dat_habitat[, c("ba.ratio", "elev")])




#Q4
#n=54
#for (i in 1:n) {print(i)}



data("iris")
nrow(iris)