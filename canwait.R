options(warn = -1)
mostupf = function(x,t){   #who goes up most at this moment
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  
  return(max(c(x[c4 == 1 & c1 <= t & c6 == 1 & c5 == 0, 3],
               x[c4 == 0 & c1 <= t & c6 == 1 & c5 == 1, 2],
               x[c4 == 1 & c1 <= t & c6 == 1 & c5 == 1, 2]))) 
}

setopen_up = function(x,t,mostup){  #when going up, where to open door for pick or release
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  return(c(x[c4 == 1 & c6 == 1 & c5 == 1 & c1 <= t, 2],
           x[c4 == 1 & c6 == 1 & c5 == 0 & c1 <= t, 3],
           mostup))
}

mostdownf = function(x,t){   #who goes down most at this moment
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  return(min(c(x[c4 == 0 & c1 <= t & c6 == 1 & c5 == 0, 3],    
               x[c4 == 0 & c1 <= t & c5 == 1 & c6 == 1, 2],
               x[c4 == 1 & c1 <= t & c5 == 1 & c6 == 1, 2])))
}

setopen_down = function(x,t,mostdown){  #when going down, where to open door for pick or release
  c1 = x[, 1]
  c4 = x[, 4]
  c5 = x[ ,5]
  c6 = x[ ,6]
  return(c(x[c1 <= t & c4 == 0 & c5 == 1 & c6 == 1, 2],
           x[c1 <= t & c4 == 0 & c6 == 1 & c5 == 0, 3],
           mostdown))
}

up_update = function(x,t,s,mostup){
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
  # deal with special case
  n= nrow(x)
  if (n == 1){
    return(x[1, 1] + (x[1, 2] - 1) + abs(x[1, 3] - x[1, 2]) + 2)
  }
  
  # prepare
  x = x[order(x[, 1]), ] #sort x based on request time
  
  #add 4th column to check request is for up or down
  #add 5th column to mark this person is in the elevator, 1 by default
  #add 6th column to mark this request is completed, 1 by default
  #x = cbind(x, (x[, 3] > x[, 2]), matrix(rep(1, 2*n), n, 2))
  x = matrix(c(x, (x[, 3] > x[, 2]), rep(1, 2*n)), n, 6)
  
  t = min(x[, 1])
  s = 1
  
  next_direction = T
  
  #first go up ---------------------------------------------------
  while(any(x[, 6] == 1)) {
    
    if(any(x[ ,6] == 1) & next_direction == T){
      
      mostup = mostupf(x,t) 
      setopen_up = setopen_up(x,t,mostup)
      
      while (mostup >= s) { 
        if (s %in% setopen_up) { # open door
          t = t + 1
          
          # stop and then update all request info, 5th column: 0 is in-elevator; 6th column: 0 is request completed
          x = up_update(x,t,s,mostup)
          
          mostup = mostupf(x,t)
          setopen_up = setopen_up(x,t,mostup)
          next_direction = (mostup>=s)
        }
        
        if(s >= mostup) break
        
        # going up one level
        t = t + 1
        s = s + 1
        
        mostup = mostupf(x,t)
        setopen_up = setopen_up(x,t,mostup)
        next_direction = (mostup>=s)
      }
    }
    
    # wait and check next direction
    x = matrix(x[x[ ,6]==1, ],sum(x[ ,6]),6) #clear complete requests
    
    
    if(any(x[,6]==1)){
      mostdown = mostdownf(x,t)
      if(x[1,1]>t & x[1,6]==1){
        
        t = x[1,1] # wait till next request appears
        
        if(x[1,2]==s & x[1,4]==1){  # if new request is at current level
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
    
    
    
    if(any(x[ ,6] == 1) & next_direction == F){   # check there is uncompleted request and next direction
      
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