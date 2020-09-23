# Replication file for: "The Traveling Salesman Problem"
# RPubs-link: https://rpubs.com/mstefan-rpubs/salesman
# (c) Martin Stefan, September 2020

# "not in" operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# extend "LETTERS" function to run from "A" to "ZZ"
MORELETTERS <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

# function to simulate cities
simCities <- function(n_cities = 5) {
  cities <- matrix(runif(2*n_cities,-1,1),ncol=2)
  rownames(cities) <- MORELETTERS[1:n_cities]
  colnames(cities) <- c("x","y")
  return(cities)
}

# function to compute adjacency matrix
compAdjMat <- function(cities) return(as.matrix(dist(cities)))

# function to generate a random route
randomRoute <- function(cities) {
  start <- rownames(cities)[1]
  route <- sample(rownames(cities)[-1])
  route <- c(start,route,start) # return back home
  return(route)
}

# function to compute the distance of a route
distRoute <- function(adjmat, route) {
  d <- 0
  for(i in 2:nrow(adjmat)) {
    d <- d + adjmat[route[i-1],route[i]]
  }
  return(d)
}

# function to plot cities
plotCities <- function(cities, 
                       main="",
                       bg="white", 
                       main_col="black",
                       point_col="deepskyblue") {
  par(bg=bg)
  plot(cities, 
       pch=16, cex=3,
       col=point_col, 
       ylim=c(-1,1.1), xlim=c(-1,1),
       yaxt="n", xaxt="n",
       ylab="", xlab="",
       bty="n",
       main=main, col.main=main_col)
}

# function to plot route
plotRoute <- function(cities, 
                      route, 
                      dist=NULL, 
                      main="",
                      bg="white", 
                      main_col = "black",
                      point_col = "deepskyblue",
                      start_col = "red",
                      line_col = "black") {
  
  # plot cities
  city_colors <- c(start_col, rep(point_col,nrow(cities)))
  plotCities(cities, 
             bg=bg, 
             main=main, 
             main_col=main_col,
             point_col=city_colors[order(route)])
  
  # plot route
  for(i in 2:(nrow(cities)+1)) {
    lines(cities[c(route[i],route[i-1]),],
          col=line_col,
          lwd=1.5)
  }
  
  # add distance in legend
  if(!is.null(dist)) legend("topleft", 
                            bty="n", # no box around legend
                            legend=round(dist,4), 
                            bg="transparent", 
                            text.col="black")
  
}

# function to perform swap in route
swap <- function(route,i,k) {
  new_route <- route[1:(i-1)]
  new_route <- c( new_route, route[k:(i)] )
  new_route <- c( new_route, route[(k+1):length(route)] )
  return(new_route)
}