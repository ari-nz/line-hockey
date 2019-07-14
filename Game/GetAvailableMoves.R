GetAvailableMoves<-function(current_node, GameState){
  # Get winning location nodes
  size = dim(GameState$GameBoard)
  middle = ceiling(size[2]/2) 
  
  winningnodes1 = seq(size[1],size[1]*size[2],size[1]) + 1 - size[1]
  winningnodes2 = seq(size[1],size[1]*size[2],size[1])
  
  winningnodes1 = winningnodes1[(middle-1):(middle+1)]
  winningnodes2 = winningnodes2[(middle-1):(middle+1)]
  

  
  #
  AllMoves = GameState$MoveMatrix[current_node,]
  AvailableMoves = which(AllMoves == 0)
  
  
  if (current_node %in% winningnodes1){
    AvailableMoves = c(AvailableMoves, 0)
  }
  
  if (current_node %in% winningnodes2){
    AvailableMoves = c(AvailableMoves, size[1] * size[2]+1)
  }
  
  
  return(AvailableMoves)
  
}