# Replication file for: "The Traveling Salesman Problem"
# RPubs-link: https://rpubs.com/mstefan-rpubs/salesman
# (c) Martin Stefan, September 2020

nearestNeighbor <- function(cities) {

  # compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # start of route
  current_city <- rownames(cities)[1]
  route <- c(current_city)
  
  # add nearest neighbor to route
  for(i in 1:(nrow(cities)-1)){
    distances <- adjmat[current_city,-which(rownames(cities) %in% route)]
    current_city <- names(which(distances == min(distances)))
    route <- c(route,current_city)
  }
  
  # add last city and start point
  last <- which(rownames(cities) %!in% route)
  route <- c(route,rownames(cities)[last])
  route <- c(route,rownames(cities)[1])
  best_route <- route # only one route
  
  # compute distance of route
  min_d <- distRoute(adjmat,route)
  
  # plot route
  plotRoute(cities,route,min_d)
  
  # return
  return(list(distance = min_d,
              route = best_route)
  )
  
}