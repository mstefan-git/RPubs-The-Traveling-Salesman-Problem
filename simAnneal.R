# Replication file for: "The Traveling Salesman Problem"
# RPubs-link: https://rpubs.com/mstefan-rpubs/salesman
# (c) Martin Stefan, September 2020

simAnneal <- function(cities, temp=1e4,
                      cooling=5e-3, break_after=1e2,
                      sleep=.05) {
  
  # compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # start with random route
  best_route <- randomRoute(cities)
  min_d <- distRoute(adjmat,best_route)

  # variable tracking
  track_temp <- c()
  track_prob <- c()
  track_dist <- c()
  
  # iterative loop
  stable_count <- 0
  while(stable_count < break_after) {

    # conduct swap
    ik <- sort(sample(2:nrow(cities),2))
    new_route <- swap(best_route, i=ik[1], k=ik[2])
    new_d <- distRoute(adjmat,new_route)
  
    # probability of adjusting route
    improvement <- min_d - new_d
    p_adjust <- ifelse(improvement > 0, 1, exp(improvement/temp))

    # adjust route?
    adjust <- ifelse(p_adjust >= runif(1,0,1), T, F)

    # if adjustment
    if(adjust) {
      best_route <- new_route
      min_d <- new_d
      stable_count <- 0
      plotRoute(cities, best_route, min_d)
      legend("topright", legend=round(temp,4), 
             bg="transparent", 
             bty="n",
             text.col="black")
      Sys.sleep(sleep)
    } else {
      stable_count <- stable_count+1
    }
    
    # update on variable tracking
    track_temp <- c(track_temp,temp)
    track_prob <- c(track_prob,p_adjust)
    track_dist <- c(track_dist,new_d)

    # cool down
    temp <- temp*(1-cooling)

  } # end of iterative loop
  
  # return
  return(list(distance = min_d,
              route = best_route,
              track_temp = track_temp,
              track_prob = track_prob,
              track_dist = track_dist)
  )
  
  
}