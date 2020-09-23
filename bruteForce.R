# Replication file for: "The Traveling Salesman Problem"
# RPubs-link: https://rpubs.com/mstefan-rpubs/salesman
# (c) Martin Stefan, September 2020

bruteForce <- function(cities, sleep=.05) {
  
  # stop if too many cities
  if(nrow(cities) > 10) stop("Please don't use Brute Force for n > 10 many cities!")
  
  # library
  library(combinat)
  start <- rownames(cities)[1]
  routes <- permn(rownames(cities)[-1])
  
  # compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # initial route and distance
  best_route <- NULL
  min_d <- Inf
  
  # all possible routes
  for(i in 1:length(routes)) {
    
    route <- routes[[i]]
    route <- c(start,route,start)
    
    # distance
    new_d <- distRoute(adjmat,route)
    
    # update distance and plot
    if(new_d < min_d) {
      min_d <- new_d
      best_route <- route
      plotRoute(cities, route, min_d)
      Sys.sleep(sleep)
    }
  }

  
  # return
  return(list(distance = min_d,
              route = best_route)
  )
  
}
