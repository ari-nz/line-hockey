ContinueTurn<-function(GameState, to_node){
  #Checks to see if the player should have another movement by bouncing on a node.

  size = dim(GameState$MaxGameBoard)
  
  node_coords = GameState$GamePoints[to_node, c(1,2)]
  
  is.vistied = GameState$GameBoard[1+ size[1]-node_coords[[1]],node_coords[[2]]]

  if (is.vistied > 1){
    continue = TRUE
  } else {
    continue = FALSE
  }
  
  print(paste("Is it still the current player, (", GameState$Turn, ")'s turn: ", continue))
  
  return(continue)
}
  