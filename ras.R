ras <- function(m, colgoal, rowgoal, iterations = 100){

  ## check input, if dataframe -> convert to matrix
  if(class(m)!="matrix") 
  
    {m <- data.matrix(m)}

  
  repeat{
  
    m <-  (diag(getrowcor(m, rowgoal))%*%m) 
    m <-  (m %*% diag(getcolcor(m, colgoal)))
    
    iterations <- iterations - 1
    if(iterations == 0) break
  }
  m
}

getcolcor <- function(m, colgoal){
   
  colgoal/colSums(m)
}

getrowcor <- function(m, rowgoal){
  
  rowgoal/rowSums(m)
 
}
