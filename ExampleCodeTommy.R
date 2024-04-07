# This is an R function named elevator, which takes an argument of a n × 3 
# matrix named x which contains all the requests as described below, and returns 
# the time when the last guest arrives his/her destination.

# At time t = 0, the elevators is resting at level 1.

# Input: There are n requests x1, . . . , xn. Each xi is a triple
# xi = (ti, si, di), which means that a guest made a request at time ti > 0 at 
# level si, and the guest is going to level di.

# Rule 1: 
# It keeps going in one direction until there is no guest in it, and there is no 
# other guest waiting to be served at a further level in the current direction.

# Rule 2: 
# It will collect guests that are going in the same direction along the way.

# Rule 3:
# After it stopped going in one direction, it will turn around and go in the 
#opposite direction, if there are any pending requests from guests.

# Rule 4: 
# It takes one unit time to move up or down a level, and takes one unit 
# time to stop at a level for all the guests to enter and/or exit (if any).

options(warn = -1)

# Build six basic functions for the "elevator".

mostupf = function(x,t){  #determine the highest floor the elevator must reach
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6] # avoid repeating filtering to speed up
  
  return(max(c(x[c4 == 1 & c1 <= t & c6 == 1 & c5 == 0, 3], # whom to drop off
               x[c4 == 0 & c1 <= t & c6 == 1 & c5 == 1, 2], # whom to pick up
               x[c4 == 1 & c1 <= t & c6 == 1 & c5 == 1, 2]))) 
}

setopen_up = function(x,t,mostup){  #when going up, check where to open door
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  return(c(x[c4 == 1 & c6 == 1 & c5 == 1 & c1 <= t, 2], # whom to drop off
           x[c4 == 1 & c6 == 1 & c5 == 0 & c1 <= t, 3], # whom to pick up
           mostup))
}

mostdownf = function(x,t){  #determine the lowest floor the elevator must reach
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  return(min(c(x[c4 == 0 & c1 <= t & c6 == 1 & c5 == 0, 3],    
               x[c4 == 0 & c1 <= t & c5 == 1 & c6 == 1, 2],
               x[c4 == 1 & c1 <= t & c5 == 1 & c6 == 1, 2])))
}

setopen_down = function(x,t,mostdown){  #when going down, where to open door
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  return(c(x[c1 <= t & c4 == 0 & c5 == 1 & c6 == 1, 2],
           x[c1 <= t & c4 == 0 & c6 == 1 & c5 == 0, 3],
           mostdown))
}

up_update = function(x,t,s,mostup){ # change the status of each request
  x = matrix(x[x[ ,6]==1, ],sum(x[ ,6]),6)
  c1 = x[, 1]
  c2 = x[, 2]
  c3 = x[, 3]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6] 
  x[c1<=t & c2 == s & c4 == 1 & c5 == 1 & c6 == 1,5] = 0
  x[c1<=t & c3 == s & c4 == 1 & c5 == 0 & c6 == 1,6] = 0
  x[s == mostup & c1<=t & c2 == s & c4 == 0 & c5 == 1 & c6 == 1,5] = 0
  return(x)
}

down_update = function(x,t,s,mostdown){
  x = matrix(x[x[ ,6]==1, ],sum(x[ ,6]),6)
  c1 = x[, 1]
  c2 = x[, 2]
  c3 = x[, 3]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  x[c1<=t & c2 == s & c4 == 0 & c5 == 1 & c6 == 1,5] = 0
  x[c1<=t & c3 == s & c4 == 0 & c5 == 0 & c6 == 1,6] = 0
  x[s == mostdown & c1 <= t & c2 == s & c4 == 1 & c5 == 1 & c6 == 1,5] = 0
  return(x)
}

###### main function #####

elevator = function(x) {
  
  # deal with special case: only one request
  n= nrow(x)
  if (n == 1){
    return(x[1, 1] + (x[1, 2] - 1) + abs(x[1, 3] - x[1, 2]) + 2)
  }
  
  # prepare
  x = x[order(x[, 1]), ] #sort x based on request time
  
  #add 4th column to mark if the passenger will go up or down
  #add 5th column to mark this person is in the elevator(0), 1 by default
  #add 6th column to mark this request is completed(0), 1 by default
  
  #x = cbind(x, (x[, 3] > x[, 2]), matrix(rep(1, 2*n), n, 2))
  x = matrix(c(x, (x[, 3] > x[, 2]), rep(1, 2*n)), n, 6)
  
  t = min(x[, 1]) #elevator starts
  s = 1 # s denotes the current floor the elevator reaches
  
  next_direction = T
  
  #first go up --------------------------------------------------------
  
  while(any(x[, 6] == 1)) {
    
    if(any(x[ ,6] == 1) & next_direction == T){
      
      mostup = mostupf(x,t) 
      setopen_up = setopen_up(x,t,mostup)
      
      while (mostup >= s) { 
        if (s %in% setopen_up) { # open door
          t = t + 1
          
          # stop to pick up and drop off; and then update all request info
          # 5th column: 0 is for pick up successfully
          # 6th column: 0 is for request completed successfully
          x = up_update(x,t,s,mostup)
          
          mostup = mostupf(x,t)
          setopen_up = setopen_up(x,t,mostup)
          next_direction = (mostup>=s) #change the direction if needed
        }
        
        if(s >= mostup) break # reach the highest floor, break while loop
        
        # elevator moves: going up one level, consuming one-unit time;
        t = t + 1
        s = s + 1
        
        mostup = mostupf(x,t)
        setopen_up = setopen_up(x,t,mostup)
        next_direction = (mostup>=s)
      }
    }
    
    # waiting for next request and check next direction
    
    #clear complete requests to speed up
    x = matrix(x[x[ ,6]==1, ],sum(x[ ,6]),6) 
    
    
    if(any(x[,6]==1)){
      mostdown = mostdownf(x,t)
      if(x[1,1]>t & x[1,6]==1){
        
        t = x[1,1] # wait till next request appears
        
        if(x[1,2]==s & x[1,4]==1){ # in case new request is at the current floor
          t = t + 1
          x = up_update(x,t,s,mostup)
          next_direction = T
        } else if(x[1,2]==s & x[1,4]==0){
          t = t + 1
          x = down_update(x,t,s,mostdown)
          next_direction = F
        } else {
          next_direction = x[1,2]>s
        }
      }
    }
    
    #going down --------------------------------------------------------
    
    # check there is uncompleted request and next direction
    if(any(x[ ,6] == 1) & next_direction == F){   
      
      mostdown = mostdownf(x,t)
      setopen_down = setopen_down(x,t,mostdown)
      
      while (mostdown <= s) {
        if (s %in% setopen_down) {
          t = t + 1
          
          # stop and then update all request info
          x = down_update(x,t,s,mostdown)
          
          mostdown = mostdownf(x,t)
          setopen_down = setopen_down(x,t,mostdown)
          next_direction = (mostdown>=s) | (mostdown<0)
        }
        
        if(s <= mostdown) break
        
        t = t + 1
        s = s - 1
        
        mostdown = mostdownf(x,t)
        setopen_down = setopen_down(x,t,mostdown)
        next_direction = (mostdown>=s) | (mostdown<0)
      }
    }
    
    
    # wait and check next direction
    x = matrix(x[x[ ,6]==1, ],sum(x[ ,6]),6) #clear complete requests
    
    if(any(x[,6]==1)){
      if(x[1,1]>t & x[1,6]==1){
        
        t = x[1,1]
        
        if(x[1,2]==s & x[1,4]==1){
          t = t + 1
          x = up_update(x,t,s,mostup)
          next_direction = T
        } else if(x[1,2]==s & x[1,4]==0){
          t = t + 1
          x = down_update(x,t,s,mostdown)
          next_direction = F
        } else {
          next_direction = x[1,2]>s
        }
      }
    } 
  }
  return(t)
}


# TEST CASES:

# Test Case 1: Requests from various floors and times, mixed directions
test_case_1 = matrix(
  c(1, 2, 5,   # Request at time 1 from floor 2 to 5
    2, 3, 1,   # Request at time 2 from floor 3 to 1
    3, 1, 4,   # Request at time 3 from floor 1 to 4
    4, 5, 2,   # Request at time 4 from floor 5 to 2
    5, 4, 6,   # Request at time 5 from floor 4 to 6
    6, 6, 3),  # Request at time 6 from floor 6 to 3
  byrow = TRUE, ncol = 3)
elevator(test_case_1)

# Test Case 2: High volume of requests at the same time
test_case_2 = matrix(
  c(1, 1, 10,  # Multiple requests starting at the same time
    1, 2, 9,
    1, 3, 8,
    1, 4, 7,
    1, 5, 6,
    1, 6, 5,
    1, 7, 4,
    1, 8, 3,
    1, 9, 2,
    1, 10, 1),
  byrow = TRUE, ncol = 3)
elevator(test_case_2)

# Test Case 3: Requests causing frequent direction changes
test_case_3 = matrix(
  c(1, 1, 3,   # Requests that zig-zag the elevator across floors
    2, 4, 1,
    3, 2, 5,
    4, 5, 2,
    5, 3, 6,
    6, 6, 3),
  byrow = TRUE, ncol = 3)
elevator(test_case_3)

# Test Case 4: Large building with sparse requests
test_case_4 = matrix(
  c(1, 10, 5000,  # Sparse requests in a large building
    10, 2000, 40,
    20, 30, 9999,
    30, 40, 20,
    40, 50, 10),
  byrow = TRUE, ncol = 3)
elevator(test_case_4)

# Test Case 5: Large size request to check computational efficiency

set.seed(42)
n_requests = 10000
request_times = sample(1:2000, n_requests, replace = TRUE)
start_floors = sample(1:2000, n_requests, replace = TRUE)
destination_floors = rep(NA, n_requests)

for (i in 1:n_requests) {
  destination_floors[i] = sample(setdiff(1:2000, start_floors[i]), 1)
}

test_case_5 = cbind(request_times, start_floors, destination_floors)
elevator(test_case_5)

# Analyze the runtime to accelerate

# system.time(elevator(test_case_5))    # about 10 seconds in my PC
# profvis(elevator(test_case_5))  

# Remark:
# GC and matrix calculation consume much time
# need to optimize the function for updating info: up_update, and the function
# to calculate the highest/lowest floor: mostupf
# might need to change the framework and data structure in the first place
# to avoid repetitive calculation