source('run.r')


size = c(5,7)
GameState = CreateBoard(size)

if (length(size)==1){
  current_node = (size^2 + 1)/2
}else{
  current_node = (size[1] *size[2] + 1)/2
}




isGameOn = TRUE

while(isGameOn){
  
  
  Moves =GetAvailableMoves(current_node, GameState)
  
  #print("Your Moves Are: ")
  #print(Moves)
  print(GameState$GameBoard)
  
  answer = menu(paste(Moves), graphics=TRUE, title=paste0("Choose Move, Player", GameState$Turn))
  
  selected_node = Moves[answer]
  
  PlotMove(current_node, selected_node, GameState$GamePoints, GameState$Turn)
  
  
  ##Check for victory
  
  if(selected_node == 0){
    isGameOn = FALSE
    print("Player 1 is the winner")
    next
  }
  
  if(selected_node == size[1]*size[2]+1){
    isGameOn = FALSE
    print("Player 2 is the winner")
    next
  }
  
  # TURN Based make move
  GameState = MakeMove(current_node, selected_node, GameState)
  #PlotMove(current_node, selected_node, GameState$GamePoints, GameState$Turn)
  continue_move = ContinueTurn(GameState, selected_node)
  
  
  ## End Game on No 

  rowcol = GameState$GamePoints[selected_node,c(1,2)]
  selected_node_max_moves = GameState$MaxGameBoard[size[2]-rowcol[[1]]+1, rowcol[[2]]]
  selected_node_current_moves = GameState$GameBoard[size[2]-rowcol[[1]]+1, rowcol[[2]]]
  
  if(selected_node_current_moves == selected_node_max_moves){
    isGameOn = FALSE
    cat(paste0("Player ", GameState$Turn, " has lost\n", "Player ", 2 %/% GameState$Turn, " is the winner"))
    next
  }
  

  
  #If not continuing move, swap turn
  if(!continue_move){
    GameState$Turn = 2 %/% GameState$Turn
  }
  


  current_node = selected_node
  
}