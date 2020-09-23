# Replication file for: "The Traveling Salesman Problem"
# RPubs-link: https://rpubs.com/mstefan-rpubs/salesman
# (c) Martin Stefan, September 2020

twoOpt <- function(cities, sleep=.05) {
  
  # compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # start with random route
  best_route <- randomRoute(cities)
  min_d <- distRoute(adjmat,best_route)
  
  # variable tracking
  track_dist <- c()
  
  # while-loop to perform complete 2-opt swap
  while(T) { 
    
    # record distance before looping through i and k
    old_d <- min_d
  
    # for-loops through values for i and k
    break_loop <- F
    for(i in 2:nrow(cities)) {
      for(k in (i+1):nrow(cities)) {
        
        # perform swap
        route <- swap(best_route,i,k)
        new_d <- distRoute(adjmat,route)
        
        # update distance and plot
        if(new_d < min_d) {
          min_d <- new_d
          best_route <- route
          plotRoute(cities, route, min_d)
          Sys.sleep(sleep)
          break_loop <- T
          break() # break out of inner loop
        } # end outer if-statement
        
      } # close inner loop
      
      # break out of outer loop
      if(break_loop) break()
      
    } # close outer loop
    
    # update on variable tracking
    track_dist <- c(track_dist,new_d)
    
    # check if the for-loops made any improvements
    if(old_d == new_d) break() # break out of while loop
    
  } # close while loop
    
  # return
  return(list(distance = min_d,
              route = best_route,
              track_dist = track_dist)
  )
  
}
