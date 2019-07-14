size = c(5,5)
GameState = CreateBoard(size)

if (length(size)==1){
  current_node = (size^2 + 1)/2
}else{
  current_node = (size[1] *size[2] + 1)/2
}




isGameOn = TRUE

while(isGameOn){
  
  
  Moves = GetAvailableMoves(current_node, GameState)
  
  #print("Your Moves Are: ")
  #print(Moves)
  print(GameState$GameBoard)
  
  answer = menu(paste(Moves), graphics=TRUE, title="Choose Move")
  
  selected_node = Moves[answer]
  
  GameState = MakeMove(current_node, selected_node, GameState)
  PlotMove(current_node, selected_node, GameState$GamePoints)
  current_node = selected_node
  
}




