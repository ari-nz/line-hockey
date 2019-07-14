CreateBoard<-function(size = 7, walls = T){
  
  if(length(size) == 1){
    size = c(size, size)
  }
  
  ## TODO - Assert the size is odd
  
  #Create game board Plot
  plot(0,0, type = 'n', xlim = c(0.5,size[1]+0.5), ylim = c(-0.5, size[2]+2), xann = NULL, yann = NULL, axes = FALSE, xlab = "", ylab = "", asp = 1 )
  
  #Create set of plotable points
  GamePoints = data.frame(
              "rows" = rep(1:size[2], size[1]),
              "cols" = rep(1:size[1], each = size[2]),
              "uID"  = 1:(size[1]*size[2])
  )
 
  #Add points to graph
  points(GamePoints$cols,GamePoints$rows, pch = 16)
  text(GamePoints$cols+0.15,GamePoints$rows+0.1, labels = paste0(GamePoints$cols,", ",GamePoints$rows), cex = 0.8)
  text(GamePoints$cols-0.15,GamePoints$rows-0.1, labels = GamePoints$uID, col = 'red', cex = 0.8)
  points(rep(ceiling(size[1]/2),2), c(0,size[2]+1), pch = 21, bg = c('blue','red'), cex = 1.5)
  text(x=rep(ceiling(size[1]/2),2), y=c(-0.5,size[2]+1.5),labels =c("Player 1 Come Here", "Player 2 Come Here"), col = c('blue','red'), cex = 1.5)
  
  
  
  #Create Game Board Matrix
  GameBoard <- t(matrix(data = 0, nrow = size[1], ncol = size[2]))
  
  #Start the first node off as entered
  middle_node = (size[1] *size[2] + 1)/2
  GameBoard[middle_node]=GameBoard[middle_node]+1 

  
  ## Add these in if you need an extra line at the top and bottom of the bame board
  #GameBoard <- matrix(data = 0, nrow = size[1]+2, ncol = size[2])
  #GameBoard[c(1,size[1]+2),] = NA
  #GameBoard[c(1,size[1]+2),ceiling(size[2]/2)] = 0
  
  
  #Initialise matrix of possible moves
  MoveMatrix = .InitialiseMoveMatrix(GamePoints)

  
  #Randomise Starting player
  Turn = sample(c(1,2),1)
  
  
  GamePoints = rbind(GamePoints,c(0, ceiling(size[1]/2), 0), c( size[2]+1, ceiling(size[1]/2), size[1]*size[2]+1))
  
  #Wrap the Initialisation up in the game state
  GameState = list("GameBoard" = GameBoard, "GamePoints" = GamePoints, "MoveMatrix" = MoveMatrix, "Turn" = Turn)
  
  
  #Holds the number of moves through a points required to draw on that point, ie 2 in the corners causeon the second time into that point
  GameState$MaxGameBoard = .InitialiseMaxGameBoard(GameState)
  
  ## TODO Build walls and add them to matrix
  #if(walls){
  #  GameState = BuildTheWall(GameState)
  #}
  
  return(GameState)
}


.BuildTheWall <- function(size, walls){

  
}

.InitialiseMaxGameBoard <- function(GameState){
  size = dim(GameState$GameBoard)
  middle_node = (size[1] *size[2] + 1)/2
  n_nodes = size[1] * size[2]
  
  MaxGameBoard = GameState$GameBoard
  
  for (i in 1:n_nodes) {
    
    MaxGameBoard[1 + (i-1) %% size[1],(i-1) %/% size[1] + 1] = length(which(GameState$MoveMatrix[i,]>=0))
  }
  MaxGameBoard[middle_node]=MaxGameBoard[middle_node]+1 
  MaxGameBoard[which(MaxGameBoard%%2 == 0)] = 0
  MaxGameBoard = ceiling(MaxGameBoard/2)
  
  return(MaxGameBoard)
}


.InitialiseMoveMatrix<-function(GamePoints){
  
    # Create a matrix which stores which links between points have been done. 
    #   NA - Impossible move
    #   0  - Available Move
    #   1  - User made move
    #   -1 - Going from to location
    
    n.uniquePoints = dim.data.frame(GamePoints)[1]
    x.max = max(GamePoints$cols)
    y.max = max(GamePoints$rows)
    MoveMatrix =  matrix(data = NA, nrow = n.uniquePoints, ncol = n.uniquePoints)
    
    for (i in 1:n.uniquePoints){
      checkPoint = GamePoints[i,]
      x.loc = checkPoint$cols
      y.loc = checkPoint$rows
      #make sure that the available moves at a given point are within the bounds and are only for adjacent moves
      for (j in (x.loc-1):(x.loc+1)){
        # if the adjacent positions are not outside the bounds
        if( j > 0 & j <= x.max){
          
          for (k in (y.loc-1):(y.loc+1)){
            if( k > 0 & k <= y.max){
              #browser()
              node_searched = intersect(which(GamePoints$col == j), which(GamePoints$row == k))
              #print(paste("Current Node = ", i, "\nj and k are, ",j,k, "\nFound node, ", node_searched))
              MoveMatrix[i,node_searched] = 0
              
              
            }
          }
        }
      }
    }
    
    diag(MoveMatrix) = -1
    return(MoveMatrix)
}












