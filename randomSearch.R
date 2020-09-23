# Replication file for: "The Traveling Salesman Problem"
# RPubs-link: https://rpubs.com/mstefan-rpubs/salesman
# (c) Martin Stefan, September 2020

randomSearch <- function(cities, n_iter=1e3, sleep=.05) {
 
  # compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # initial route and distance
  best_route <- NULL
  min_d <- Inf
  
  # loop
  for(i in 1:n_iter) {
    
    # random route
    route <- randomRoute(cities)

    # distance
    new_d <- distRoute(adjmat,route)
    
    # plot
    plotRoute(cities, route, new_d)
    Sys.sleep(sleep)
    
    # if improvement, update minium distance best route
    if(new_d < min_d) {
      min_d <- new_d
      best_route <- route
    }
  }

  # return
  return(list(distance = min_d,
              route = best_route)
  )
  
}
