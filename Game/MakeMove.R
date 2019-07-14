MakeMove<-function(from_node, to_node, GameState, set.to = 1){
  
  

  size = dim(GameState$GameBoard)
  
  ##TODO Error checking on inputs
  CurrentValue = GameState$MoveMatrix[from_node, to_node]
  
  if(is.na(CurrentValue)){
    stop("Moving to an invalid Location")
  }
  
  
  if(CurrentValue == 1){
    stop("Moving to an already visited Location")
  }
  
  if(CurrentValue == -1){
    stop("Moving to current location")
  }
  
  
  #Update Move Matrix
  GameState$MoveMatrix[from_node, to_node] = set.to
  GameState$MoveMatrix[to_node, from_node] = set.to

  #Update Game Board
  NewNode = GameState$GamePoints[to_node,]
  GameState$GameBoard[size[1] - NewNode[[1]]+ 1, NewNode[[2]]] = GameState$GameBoard[size[1] - NewNode[[1]]+ 1,  NewNode[[2]]] + 1 
  
  
  return(GameState)
}