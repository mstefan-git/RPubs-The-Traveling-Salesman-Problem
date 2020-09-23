# Replication file for: "The Traveling Salesman Problem"
# RPubs-link: https://rpubs.com/mstefan-rpubs/salesman
# (c) Martin Stefan, September 2020

rm(list = ls())
graphics.off()
set.seed(1)

# source functions
source("functions.R")
source("randomSearch.R")
source("bruteForce.R")
source("nearestNeighbor.R")
source("twoOpt.R")
source("simAnneal.R")

# simulate cities
n_cities <- 10
cities <- simCities(n_cities)
cities

# plot cities
plotCities(cities)

# adjacency matrix
adjmat <- compAdjMat(cities)
adjmat

# random route
route <- randomRoute(cities)
route

# distance
d <- distRoute(adjmat, route)
d

# plot route
plotRoute(cities,route,d)

# run different algorithms
nn <- nearestNeighbor(cities)          # nearest neighbor
rs <- randomSearch(cities, n_iter=10)  # random search
to <- twoOpt(cities)                   # two-opt
sa <- simAnneal(cities, temp=1)        # simulated annealing
bf <- bruteForce(cities)               # brute force

# compare results
par(mfrow=c(2,3))
plotRoute(cities, nn$route, nn$distance, main="Nearest neighbor")
plotRoute(cities, rs$route, rs$distance, main="Random search")
plotRoute(cities, to$route, to$distance, main="2-opt")
plotRoute(cities, sa$route, sa$distance, main="Simmulated annealing")
plotRoute(cities, bf$route, bf$distance, main="Brute force")

# demonstrate simulated annealing
par(bg="white", mfrow=c(2,2))
plot(sa$track_temp, type="l", ylab="", main="Temperature")
plot(sa$track_prob, ylab="", main="Acceptance Probability", pch=16, cex=.5)
plot(sa$track_dist, type="l", ylab="", main="Distance (SA)", ylim=c(4,12))
plot(to$track_dist, type="l", ylab="", main="Distance (2-opt)", ylim=c(4,12))
par(mfrow=c(1,1))
