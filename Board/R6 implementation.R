library(R6)
library(ReinforcementLearning)

Board= R6::R6Class(classname = 'Board',
                  public = list(
                    Dots = NULL,
                    MoveMatrix=NULL,
                    RelationMatrix = NULL,
                    initialize = function(m,n,walls) {

                      size = c(m,n)

                      Dots = data.frame(
                        "rows" = rep(1:size[2], size[1]),
                        "cols" = rep(1:size[1], each = size[2]),
                        "uID"  = 1:(size[1]*size[2])
                      )
                      Goals = data.frame(
                        "rows" = c(0, n+1),
                        "cols" = c(ceiling(n/2), ceiling(n/2)),
                        "uID"  = c(0,size[1]*size[2] + 1)
                      )
                      self$Dots = rbind(Dots,Goals)
                      made_matricies =  private$InitialiseMoveMatrix(self$Dots)
                      self$MoveMatrix = made_matricies$MoveMatrix
                      self$RelationMatrix = made_matricies$RelationMatrix
                    },
                    plot_state = function(){
                      ds = head(self$Dots,-2)
                      ds$node_string = sprintf("%02i", ds$uID)
                      ds = ds[order(ds$row, ds$cols),]
                      rm = self$RelationMatrix
                      mm = self$MoveMatrix

                      n.uniquePoints = dim.data.frame(ds)[1]
                      x.max = max(ds$cols)
                      y.max = max(ds$rows)
                      string_mat = matrix(data = "NA", nrow = 3 *x.max, ncol = 3* y.max)

                      for (i in 1:n.uniquePoints){
                        mini_mat = matrix(data = "  ", nrow = 3, ncol = 3)
                        current_point = ds[i,]
                        x.loc = current_point$cols
                        y.loc = current_point$rows
                        # print('-----')
                        # print(paste('i=',i))
                        for (j in (x.loc-1):(x.loc+1)){

                          for (k in (y.loc-1):(y.loc+1)){
                            node_searched = intersect(which(ds$col == j), which(ds$row == k))

                            if(length(node_searched) != 0){
                              mm_x = j-x.loc+2
                              mm_y = k-y.loc+2
                              if(mm[i,node_searched] == 1){
                                mini_mat[mm_x, mm_y] = rm[i,node_searched]
                                # print(current_point)
                                # print(ds[node_searched,])
                                # print(rm[i,node_searched])
                                print(paste0(j,",",k,",",rm[i,node_searched]))
                              }
                            }



                          }
                        }
                        mini_mat[2,2] = current_point[['node_string']]
                        # print(mini_mat)
                        smx = (3*(x.loc-1) + 1):(3*(x.loc))
                        smy = (3*(y.loc-1) + 1):(3*(y.loc))
                        # print(smx)
                        # print(smy)
                        string_mat[smx, smy] = mini_mat

                      }


                      pretty_string = gsub('\\|','\\ |',
                                           paste0(apply(string_mat, 1, paste, collapse = ""), collapse = '\n')

                      )
                      cat(pretty_string)
                      return(invisible())
                                          }
                  ),
                    private = list(
                      InitialiseMoveMatrix=function(GamePoints){

                        # Create a matrix which stores which links between points have been done.
                        #   NA - Impossible move
                        #   0  - Available Move
                        #   1  - User made move
                        #   -1 - Going from to location

                        n.uniquePoints = dim.data.frame(GamePoints)[1]
                        x.max = max(GamePoints$cols)
                        y.max = max(GamePoints$rows)
                        MoveMatrix =  matrix(data = NA, nrow = n.uniquePoints, ncol = n.uniquePoints)
                        RelationMatrix = matrix(data = NA, nrow = n.uniquePoints, ncol = n.uniquePoints)
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
                                  # browser()
                                  node_searched = intersect(which(GamePoints$col == j), which(GamePoints$row == k))
                                  # print(paste("Current Node = ", i, "j and k are, ",j,k, "Found node, ", node_searched))
                                  MoveMatrix[i,node_searched] = 0


                                  x_diff = (checkPoint[1,'rows'] - GamePoints[node_searched,'rows'])
                                  y_diff = (checkPoint[1,'cols'] - GamePoints[node_searched,'cols'])
                                  diffs = c(x_diff, y_diff)

                                  if      (all(diffs == c(1, 1))   ) {RelationMatrix[i,node_searched] =' \\' }
                                  else if (all(diffs == c(0, 1))   ) {RelationMatrix[i,node_searched] ='--'}
                                  else if (all(diffs == c(-1, 1))  ) {RelationMatrix[i,node_searched] =' /' }
                                  else if (all(diffs == c(-1, 0))  ) {RelationMatrix[i,node_searched] ='|' }
                                  else if (all(diffs == c(-1, -1)) ) {RelationMatrix[i,node_searched] =' \\' }
                                  else if (all(diffs == c(0, -1))  ) {RelationMatrix[i,node_searched] ='--'}
                                  else if (all(diffs == c(1, -1))  ) {RelationMatrix[i,node_searched] =' /' }
                                  else if (all(diffs == c(1, 0))   ) {RelationMatrix[i,node_searched] ='|' }
                                  # print(paste("The link between (", checkPoint[1,'rows'],",", checkPoint[1,'cols'],") and (",
                                  #             GamePoints[node_searched,'rows'],",", GamePoints[node_searched,'cols'], ") is ",
                                  #             RelationMatrix[i,node_searched]
                                  #
                                  #             ))


                                }
                              }
                            }
                          }
                        }

                        diag(MoveMatrix) = -1
                        diag(RelationMatrix) = ' .'
                        return(list("MoveMatrix" = MoveMatrix,"RelationMatrix"= RelationMatrix))
                      }
                    )
              )

b = Board$new(3,3,FALSE)


b$plot_state()







string_mat




  x.loc = checkPoint$cols
  y.loc = checkPoint$rows
  #make sure that the available moves at a given point are within the bounds and are only for adjacent moves
  for (j in (x.loc-1):(x.loc+1)){

string_rep

    row_types




#
# CreateBoard<-function(size, walls = T){
#
#   if(length(size) == 1){
#     size = c(size, size)
#   }
#
#   ## TODO - Assert the size is odd
#
#   #Create game board Plot
#   plot(0,0, type = 'n', xlim = c(0.5,size[1]+0.5), ylim = c(-0.5, size[2]+2), xann = NULL, yann = NULL, axes = FALSE, xlab = "", ylab = "", asp = 1 )
#
#   #Create set of plotable points
#   GamePoints = data.frame(
#     "rows" = rep(1:size[2], size[1]),
#     "cols" = rep(1:size[1], each = size[2]),
#     "uID"  = 1:(size[1]*size[2])
#   )
#
#   #Add points to graph
#   points(GamePoints$cols,GamePoints$rows, pch = 16)
#   text(GamePoints$cols+0.15,GamePoints$rows+0.1, labels = paste0(GamePoints$cols,", ",GamePoints$rows), cex = 0.8)
#   text(GamePoints$cols-0.15,GamePoints$rows-0.1, labels = GamePoints$uID, col = 'red', cex = 0.8)
#   points(rep(ceiling(size[1]/2),2), c(0,size[2]+1), pch = 21, bg = c('blue','red'), cex = 1.5)
#   text(x=rep(ceiling(size[1]/2),2), y=c(-0.5,size[2]+1.5),labels =c("Player 1 Come Here", "Player 2 Come Here"), col = c('blue','red'), cex = 1.5)
#
#
#
#   #Create Game Board Matrix
#   GameBoard <- t(matrix(data = 0, nrow = size[1], ncol = size[2]))
#
#   #Start the first node off as entered
#   middle_node = (size[1] *size[2] + 1)/2
#   GameBoard[middle_node]=GameBoard[middle_node]+1
#
#
#   ## Add these in if you need an extra line at the top and bottom of the bame board
#   #GameBoard <- matrix(data = 0, nrow = size[1]+2, ncol = size[2])
#   #GameBoard[c(1,size[1]+2),] = NA
#   #GameBoard[c(1,size[1]+2),ceiling(size[2]/2)] = 0
#
#
#   #Initialise matrix of possible moves
#   MoveMatrix = .InitialiseMoveMatrix(GamePoints)
#
#
#   #Randomise Starting player
#   Turn = sample(c(1,2),1)
#
#
#   GamePoints = rbind(GamePoints,c(0, ceiling(size[1]/2), 0), c( size[2]+1, ceiling(size[1]/2), size[1]*size[2]+1))
#
#   #Wrap the Initialisation up in the game state
#   GameState = list("GameBoard" = GameBoard, "GamePoints" = GamePoints, "MoveMatrix" = MoveMatrix, "Turn" = Turn)
#
#
#   #Holds the number of moves through a points required to draw on that point, ie 2 in the corners causeon the second time into that point
#   GameState$MaxGameBoard = .InitialiseMaxGameBoard(GameState)
#
#   ## TODO Build walls and add them to matrix
#   #if(walls){
#   #  GameState = BuildTheWall(GameState)
#   #}
#
#   return(GameState)
# }
#
#
# .BuildTheWall <- function(size, walls){
#
#
# }
#
# .InitialiseMaxGameBoard <- function(GameState){
#   size = dim(GameState$GameBoard)
#   middle_node = (size[1] *size[2] + 1)/2
#   n_nodes = size[1] * size[2]
#
#   MaxGameBoard = GameState$GameBoard
#
#   for (i in 1:n_nodes) {
#
#     MaxGameBoard[1 + (i-1) %% size[1],(i-1) %/% size[1] + 1] = length(which(GameState$MoveMatrix[i,]>=0))
#   }
#   MaxGameBoard[middle_node]=MaxGameBoard[middle_node]+1
#   MaxGameBoard[which(MaxGameBoard%%2 == 0)] = 0
#   MaxGameBoard = ceiling(MaxGameBoard/2)
#
#   return(MaxGameBoard)
# }

